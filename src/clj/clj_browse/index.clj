(ns clj-browse.index
  "Write analyses, git trees and commits into the database.

  Everything is idempotent and keyed by content (blob keys, tree SHAs, commit
  SHAs, form hashes), so an interrupted index simply resumes."
  (:require [babashka.fs :as fs]
            [clj-browse.analyze :as analyze]
            [clj-browse.db :as db]
            [clj-browse.events :as events]
            [clj-browse.git :as git]
            [clj-browse.projects :as projects]
            [clj-browse.schema :as schema]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [datalevin.core :as d]))

(def analysis-batch-size 200)

;; ---------------------------------------------------------------------------
;; small helpers

(defn strip-nils [m]
  (into {} (remove (fn [[_ v]] (or (nil? v) (and (coll? v) (not (map? v)) (empty? v))))) m))

(defn repo-eid [repo-path] (db/entid [:repo/path (str repo-path)]))

(defn kondo-config-dir [repo-path]
  (let [p (fs/path repo-path ".clj-kondo")]
    (when (fs/directory? p) (str p))))

(defn kondo-config-hash
  "Hash of the repo's clj-kondo config + hooks; part of every blob key, so
  config changes trigger re-analysis."
  [repo-path]
  (if-let [dir (kondo-config-dir repo-path)]
    (let [files (->> (fs/glob dir "**.{edn,clj,cljc,bb}")
                     (remove #(str/includes? (str %) ".cache"))
                     (map str) sort)]
      (subs (git/sha1-hex (str/join "\n" (map #(str % (git/sha1-hex (slurp %))) files))) 0 10))
    "none"))

(defn blob-key [sha ext cfg] (str sha "." ext "@" cfg))

;; ---------------------------------------------------------------------------
;; analysis → tx

(defn- existing-forms [hashes]
  (if (empty? hashes)
    #{}
    (set (d/q '[:find [?h ...] :in $ [?h ...] :where [_ :form/hash ?h]] (db/db) (vec hashes)))))

(defn- tx-builder []
  {:vars (volatile! {}) :forms (volatile! #{})})

(defn- var-ref [{:keys [vars]} {:keys [fqn ns name keyword?]}]
  (let [tid (str "v:" fqn)]
    (vswap! vars assoc fqn (strip-nils {:db/id tid :var/fqn fqn :var/ns ns
                                        :var/name (if keyword? fqn name)
                                        :var/keyword? (boolean keyword?)}))
    tid))

(defn- form-ref [{:keys [forms] :as b} existing f]
  (let [h (:hash f)]
    (cond
      (contains? existing h) [:form/hash h]
      (contains? @forms h) (str "f:" h)
      :else
      (do (vswap! forms conj h)
          (strip-nils
           {:db/id (str "f:" h)
            :form/hash h
            :form/kind (:kind f)
            :form/head (:head f)
            :form/name (:name f)
            :form/summary (:summary f)
            :form/defs (mapv (fn [dd]
                               (strip-nils
                                {:def/var (var-ref b dd)
                                 :def/kind (:kind dd)
                                 :def/name (:name dd)
                                 :def/doc (:doc dd)
                                 :def/arglists (not-empty (:arglists dd))
                                 :def/private? (:private? dd)
                                 :def/langs (:langs dd)
                                 :def/defined-by (:defined-by dd)
                                 :def/drow (:drow dd) :def/col (:col dd)
                                 :def/end-drow (:end-drow dd) :def/end-col (:end-col dd)}))
                             (:defs f))
            :form/usages (mapv (fn [u]
                                 {:usage/var (var-ref b u)
                                  :usage/lang (:lang u)
                                  :usage/drow (:drow u) :usage/col (:col u)})
                               (:usages f))
            :form/children (mapv (fn [c]
                                   (strip-nils
                                    {:child/index (:index c) :child/drow (:drow c) :child/col (:col c)
                                     :child/end-drow (:end-drow c) :child/end-col (:end-col c)
                                     :child/summary (:summary c)}))
                                 (:children f))})))))

(defn- blob-entity [b existing {:keys [key sha ext]} result]
  (strip-nils
   {:blob/key key
    :blob/sha sha
    :blob/ext ext
    :blob/extractor-version schema/extractor-version
    :blob/error (:error result)
    :blob/ns (:ns result)
    :blob/ns-doc (:ns-doc result)
    :blob/requires (:requires result)
    :blob/aliases (not-empty (:aliases result))
    :blob/occs (mapv (fn [f]
                       {:occ/form (form-ref b existing f)
                        :occ/index (:index f)
                        :occ/row (:row f) :occ/col (:col f)
                        :occ/end-row (:end-row f) :occ/end-col (:end-col f)
                        :occ/langs (:langs f)})
                     (:forms result))}))

(def ^:private analysis-attrs
  [:blob/error :blob/ns :blob/ns-doc :blob/requires :blob/aliases :blob/extractor-version])

(defn- stale-blob-retractions
  "Clear a previously analyzed blob (older extractor): its occurrences and every
  analysis attribute, so nothing from the old result (e.g. :blob/error) survives."
  [keys]
  (for [k keys
        :let [e (d/entity (db/db) [:blob/key k])]
        :when e
        tx (concat (for [occ (:blob/occs e)] [:db/retractEntity (:db/id occ)])
                   (for [a analysis-attrs :when (some? (get e a))] [:db.fn/retractAttribute (:db/id e) a]))]
    tx))

(defn blob-needs-analysis? [k]
  (let [v (ffirst (d/q '[:find ?v :in $ ?k :where [?b :blob/key ?k] [?b :blob/extractor-version ?v]]
                       (db/db) k))]
    (or (nil? v) (< v schema/extractor-version))))

(defn ensure-blobs!
  "items: [{:key :sha :path :text-fn}] — analyzes and stores the ones that are
   missing or stale. `text-fn` is called lazily only for blobs that need it."
  [repo-path items]
  (let [todo (->> items
                  (filter #(blob-needs-analysis? (:key %)))
                  (reduce (fn [m it] (if (contains? m (:key it)) m (assoc m (:key it) it))) {})
                  vals vec)
        cfg-dir (kondo-config-dir repo-path)]
    (doseq [batch (partition-all analysis-batch-size todo)]
      (let [with-text (mapv #(assoc % :text (or ((:text-fn %)) "")) batch)
            results (analyze/analyze-batch (mapv #(select-keys % [:path :text]) with-text)
                                           {:config-dir cfg-dir})
            hashes (into #{} (comp (mapcat :forms) (map :hash)) results)
            existing (existing-forms hashes)
            b (tx-builder)
            blobs (mapv #(blob-entity b existing %1 %2) batch results)
            retractions (vec (stale-blob-retractions (map :key batch)))]
        (when (seq retractions) (db/transact! retractions))
        (db/transact! (into (vec (vals @(:vars b))) blobs))))
    (count todo)))

;; ---------------------------------------------------------------------------
;; projects

(defn sync-projects!
  "Re-discover project files and upsert them; returns the project maps."
  [repo-path]
  (let [repo-path (str repo-path)
        rid (repo-eid repo-path)
        found (projects/discover-projects repo-path)
        keys (set (map #(str repo-path "|" (:project/file %)) found))
        old (d/q '[:find ?e ?k :in $ ?r :where [?e :project/repo ?r] [?e :project/key ?k]]
                 (db/db) rid)]
    (db/transact!
     (concat
      (for [[e k] old :when (not (contains? keys k))] [:db/retractEntity e])
      (for [p found]
        (strip-nils (assoc p
                           :project/key (str repo-path "|" (:project/file p))
                           :project/repo rid
                           :project/paths (vec (:project/paths p))
                           :project/aliases (vec (:project/aliases p))
                           :project/builds (vec (:project/builds p)))))))
    found))

(defn source-dirs [projects]
  (->> projects (mapcat :project/paths) (map :path/dir) distinct sort vec))

;; ---------------------------------------------------------------------------
;; worktree snapshot

(defn- existing-files [rid]
  (into {} (d/q '[:find ?p ?s :in $ ?r :where [?f :file/repo ?r] [?f :file/path ?p] [?f :file/sha ?s]]
                (db/db) rid)))

(defn index-worktree!
  "Index the working tree. With `only-paths`, only those repo-relative paths are
   re-checked (watcher fast path); otherwise the full source tree is scanned.
   Returns {:changed [paths] :removed [paths]}."
  ([repo-path] (index-worktree! repo-path nil))
  ([repo-path only-paths]
   (let [repo-path (str repo-path)
         rid (repo-eid repo-path)
         cfg (kondo-config-hash repo-path)
         _ (when (not= cfg (:repo/kondo-config-hash (d/entity (db/db) rid)))
             (db/transact! [{:db/id rid :repo/kondo-config-hash cfg}]))
         projects (if only-paths
                    (map #(into {} %) (d/q '[:find [(pull ?p [*]) ...] :in $ ?r :where [?p :project/repo ?r]]
                                           (db/db) rid))
                    (sync-projects! repo-path))
         dirs (source-dirs projects)
         in-source? (fn [rel] (some #(or (= % "") (str/starts-with? rel (str % "/"))) dirs))
         old (existing-files rid)
         candidates (if only-paths
                      (filter #(and (projects/clojure-file? %) (in-source? %)) only-paths)
                      (projects/list-source-files repo-path dirs))
         present (->> candidates
                      (keep (fn [rel]
                              (let [f (fs/path repo-path rel)]
                                (when (fs/regular-file? f)
                                  (let [bs (fs/read-all-bytes f)]
                                    {:path rel :bytes bs :sha (git/blob-sha bs)
                                     :mtime (.toMillis (fs/last-modified-time f))})))))
                      vec)
         present-paths (set (map :path present))
         removed (if only-paths
                   (filter #(and (contains? old %) (not (contains? present-paths %))) only-paths)
                   (remove present-paths (keys old)))
         changed (->> present
                      (filter (fn [{:keys [path sha]}]
                                (let [k (blob-key sha (analyze/ext-of path) cfg)]
                                  (or (not= sha (get old path))
                                      (blob-needs-analysis? k)
                                      (nil? (db/entid [:blob/key k])))))))]
     (ensure-blobs! repo-path
                    (for [{:keys [path sha bytes]} changed]
                      {:key (blob-key sha (analyze/ext-of path) cfg) :sha sha :path path
                       :text-fn (fn [] (String. ^bytes bytes "UTF-8"))}))
     (db/transact!
      (concat
       (for [p removed] [:db/retractEntity [:file/key (str repo-path "|" p)]])
       (for [{:keys [path sha mtime]} changed]
         {:file/key (str repo-path "|" path)
          :file/repo rid
          :file/path path
          :file/sha sha
          :file/mtime mtime
          :file/blob [:blob/key (blob-key sha (analyze/ext-of path) cfg)]})))
     (let [res {:changed (mapv :path changed) :removed (vec removed)}]
       (when (or (seq (:changed res)) (seq (:removed res)))
         (events/broadcast! [:repo/changed (assoc res :repo repo-path)]))
       res))))

;; ---------------------------------------------------------------------------
;; git history

(defn- skip-tree-entry? [{:keys [name type]}]
  (and (= type :tree) (or (projects/skip-dir? name) (str/starts-with? name "."))))

(defn- tree-exists? [sha] (some? (db/entid [:tree/sha sha])))

(defn- walk-tree
  "Collect new trees under `sha` (not yet in db) bottom-up and the blobs they need.
   acc: {:trees [{:sha :entries [...]}] :blobs {key item} :seen #{sha}}"
  [batch repo-path cfg sha prefix acc]
  (if (or (contains? (:seen acc) sha) (tree-exists? sha))
    acc
    (let [entries (or (git/read-tree batch sha) [])
          acc (update acc :seen conj sha)
          acc (reduce (fn [acc e]
                        (if (and (= :tree (:type e)) (not (skip-tree-entry? e)))
                          (walk-tree batch repo-path cfg (:sha e) (str prefix (:name e) "/") acc)
                          acc))
                      acc entries)
          kept (->> entries
                    (keep (fn [e]
                            (case (:type e)
                              :tree (when-not (skip-tree-entry? e)
                                      {:entry/name (:name e) :entry/sha (:sha e) :kind :tree})
                              :blob (when (projects/clojure-file? (:name e))
                                      (let [path (str prefix (:name e))
                                            k (blob-key (:sha e) (analyze/ext-of path) cfg)]
                                        {:entry/name (:name e) :entry/sha (:sha e) :kind :blob
                                         :key k :path path}))
                              nil)))
                    vec)
          acc (reduce (fn [acc {:keys [kind key path] :as e}]
                        (if (= kind :blob)
                          (update acc :blobs assoc key {:key key :sha (:entry/sha e) :path path
                                                        :text-fn #(git/cat-blob-string batch (:entry/sha e))})
                          acc))
                      acc kept)]
      (update acc :trees conj {:sha sha :entries kept}))))

(defn- tree-tx [trees]
  (let [local (set (map :sha trees))]
   (for [{:keys [sha entries]} trees]
    {:db/id (str "t:" sha)
     :tree/sha sha
     :tree/entries (vec (for [{:keys [kind key] :as e} entries]
                          (cond-> {:entry/name (:entry/name e) :entry/sha (:entry/sha e)}
                            (= kind :blob) (assoc :entry/blob [:blob/key key])
                            (= kind :tree) (assoc :entry/tree (if (local (:entry/sha e))
                                                                (str "t:" (:entry/sha e))
                                                                [:tree/sha (:entry/sha e)])))))})))

(defn- commit-indexed? [sha]
  (some? (ffirst (d/q '[:find ?c :in $ ?s :where [?c :commit/sha ?s] [?c :commit/indexed? true]]
                      (db/db) sha))))

(defn index-commits!
  "Index the given commit maps (from git/log-commits). Processes in chunks:
   walk new trees, analyze new blobs in one kondo batch, then transact."
  [repo-path batch commits {:keys [on-progress chunk-size] :or {chunk-size 20}}]
  (let [repo-path (str repo-path)
        rid (repo-eid repo-path)
        cfg (or (:repo/kondo-config-hash (d/entity (db/db) rid)) (kondo-config-hash repo-path))
        todo (vec (remove #(commit-indexed? (:sha %)) commits))
        total (count todo)]
    (loop [chunks (partition-all chunk-size todo) done 0]
      (when-let [chunk (first chunks)]
        (let [with-trees (mapv (fn [c]
                                 (assoc c :tree (:tree (git/parse-commit (:bytes (git/cat-file batch (:sha c)))))))
                               chunk)
              acc (reduce (fn [acc c] (walk-tree batch repo-path cfg (:tree c) "" acc))
                          {:trees [] :blobs {} :seen #{}} with-trees)]
          (ensure-blobs! repo-path (vals (:blobs acc)))
          ;; trees are collected bottom-up, so subtrees are created before parents
          (doseq [tchunk (partition-all 500 (:trees acc))]
            (db/transact! (vec (tree-tx tchunk))))
          (db/transact!
           (vec (for [c with-trees]
                  (strip-nils
                   {:commit/sha (:sha c)
                    :commit/repos rid
                    :commit/parent-shas (:parents c)
                    :commit/tree [:tree/sha (:tree c)]
                    :commit/author (:author c)
                    :commit/email (:email c)
                    :commit/time (:time c)
                    :commit/subject (:subject c)
                    :commit/indexed? true}))))
          (let [done (+ done (count chunk))]
            (when on-progress (on-progress done total))
            (recur (rest chunks) done)))))
    total))

(defn link-parents!
  "Connect :commit/parents for indexed commits of the repo and mark boundary
   commits (a parent outside the indexed window)."
  [repo-path]
  (let [rid (repo-eid (str repo-path))
        rows (d/q '[:find ?c ?ps :in $ ?r
                    :where [?c :commit/repos ?r] [?c :commit/indexed? true]
                    [?c :commit/parent-shas ?ps]]
                  (db/db) rid)
        indexed (set (d/q '[:find [?s ...] :in $ ?r :where [?c :commit/repos ?r]
                            [?c :commit/indexed? true] [?c :commit/sha ?s]]
                          (db/db) rid))
        linked (set (d/q '[:find ?c ?ps :in $ ?r :where [?c :commit/repos ?r]
                           [?c :commit/parents ?p] [?p :commit/sha ?ps]]
                         (db/db) rid))
        missing (group-by first (remove #(contains? indexed (second %)) rows))
        tx (concat
            (for [[c ps] rows :when (and (contains? indexed ps) (not (contains? linked [c ps])))]
              [:db/add c :commit/parents [:commit/sha ps]])
            (for [c (distinct (map first rows))]
              [:db/add c :commit/boundary? (contains? missing c)]))]
    (doseq [part (partition-all 2000 tx)] (db/transact! (vec part)))))

(defn tracked-refs
  "Default branch + current branch + pinned refs (that exist)."
  [repo-path]
  (let [rid (repo-eid (str repo-path))
        pinned (:repo/pinned-branches (d/entity (db/db) rid))
        all (git/refs repo-path)
        names (set (map :name all))]
    (->> (concat [(git/default-branch repo-path) (git/current-branch repo-path)] pinned)
         (remove nil?) distinct (filter names) vec)))

(defn sync-refs!
  "Upsert ref entities; returns all refs with :tracked?."
  [repo-path]
  (let [repo-path (str repo-path)
        rid (repo-eid repo-path)
        tracked (set (tracked-refs repo-path))
        all (git/refs repo-path)
        keys (set (map #(str repo-path "|" (:name %)) all))
        old (d/q '[:find ?e ?k :in $ ?r :where [?e :ref/repo ?r] [?e :ref/key ?k]] (db/db) rid)]
    (db/transact!
     (vec (concat
           (for [[e k] old :when (not (contains? keys k))] [:db/retractEntity e])
           (for [{:keys [name kind commit]} all]
             (cond-> {:ref/key (str repo-path "|" name) :ref/repo rid :ref/name name
                      :ref/kind kind :ref/tracked? (contains? tracked name)}
               (commit-indexed? commit) (assoc :ref/commit [:commit/sha commit]))))))
    (mapv #(assoc % :tracked? (contains? tracked (:name %))) all)))

(defn index-history!
  "Index tracked branches: the last `window` commits of each (or all when the
   repo opted into full history). Newest commits first."
  [repo-path {:keys [window on-progress]}]
  (let [repo-path (str repo-path)
        rid (repo-eid repo-path)
        repo (d/entity (db/db) rid)
        n (when-not (:repo/full-history? repo) (or (:repo/commit-window repo) window 100))
        tracked (tracked-refs repo-path)
        batch (git/open-batch repo-path)]
    (try
      (let [commits (->> tracked
                         (mapcat #(git/log-commits repo-path % n))
                         (reduce (fn [[seen acc] c] (if (seen (:sha c)) [seen acc] [(conj seen (:sha c)) (conj acc c)]))
                                 [#{} []])
                         second)
            ;; for incremental runs (window already indexed) also pick up new commits
            ;; that are ancestors beyond the window? No: the window slides forward with
            ;; new commits and older indexed commits stay indexed.
            total (index-commits! repo-path batch commits {:on-progress on-progress})]
        (link-parents! repo-path)
        (sync-refs! repo-path)
        (when (pos? total)
          (events/broadcast! [:repo/history-changed {:repo repo-path :new-commits total}]))
        total)
      (finally (git/close-batch batch)))))

(defn add-repo!
  "Register a repo (does not index)."
  [repo-path]
  (let [repo-path (str (fs/canonicalize repo-path))]
    (when-not (repo-eid repo-path)
      (db/transact! [(strip-nils {:repo/path repo-path
                                  :repo/name (str (fs/file-name repo-path))
                                  :repo/remote-url (git/remote-url repo-path)
                                  :repo/added-at (System/currentTimeMillis)
                                  :repo/position (count (d/q '[:find [?r ...] :where [?r :repo/path]] (db/db)))})]))
    repo-path))

(defn index-repo!
  "Full index: worktree, then history."
  [repo-path opts]
  (index-worktree! repo-path)
  (index-history! repo-path opts))

(comment
  (log/info "x"))
