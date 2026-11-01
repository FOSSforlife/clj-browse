(ns clj-browse.queries
  "Read-side API used by the UI. Returns plain data (no entities)."
  (:require [babashka.fs :as fs]
            [clj-browse.db :as db]
            [clj-browse.resolve :as resolve]
            [clojure.string :as str]
            [datalevin.core :as d]))

(defn repos []
  (->> (d/q '[:find [(pull ?r [:db/id :repo/path :repo/name :repo/position :repo/full-history?
                               :repo/commit-window :repo/pinned-branches :repo/repl-config
                               :repo/remote-url]) ...]
              :where [?r :repo/path]]
            (db/db))
       (map (fn [r]
              (assoc r
                     :exists? (fs/directory? (:repo/path r))
                     :bb? (fs/exists? (fs/path (:repo/path r) "bb.edn")))))
       (sort-by (juxt #(or (:repo/position %) 0) :repo/name))
       vec))

(defn scan-roots []
  (vec (sort (d/q '[:find [?p ...] :where [_ :root/path ?p]] (db/db)))))

(defn- repo-id [repo] (db/entid [:repo/path repo]))

(defn outline
  "Projects of a repo with their paths and the files under each path."
  [repo]
  (let [db (db/db)
        rid (repo-id repo)
        files (->> (d/q '[:find ?path ?sha ?b
                          :in $ ?r
                          :where [?f :file/repo ?r] [?f :file/path ?path] [?f :file/sha ?sha]
                          [?f :file/blob ?b]]
                        db rid)
                   (map (fn [[path sha b]]
                          (let [be (d/entity db b)]
                            {:path path :sha sha
                             :ns (:blob/ns be)
                             :error (:blob/error be)
                             :form-count (count (:blob/occs be))})))
                   (sort-by :path) vec)
        projects (->> (d/q '[:find [(pull ?p [*]) ...] :in $ ?r :where [?p :project/repo ?r]] db rid)
                      (sort-by (juxt #(count (str/split (:project/file %) #"/")) :project/file)))]
    {:repo repo
     :projects
     (vec (for [p projects]
            {:key (:project/key p)
             :name (:project/name p)
             :type (:project/type p)
             :file (:project/file p)
             :dir (:project/dir p)
             :error (:project/error p)
             :aliases (:project/aliases p)
             :builds (:project/builds p)
             :paths (vec (for [{:path/keys [dir tags]} (:project/paths p)]
                           {:dir dir :tags (vec (sort tags))
                            :files (filterv #(str/starts-with? (:path %) (str dir "/")) files)}))}))}))

(defn- def-data [de]
  {:fqn (:var/fqn (:def/var de))
   :kind (:def/kind de)
   :name (:def/name de)
   :doc (:def/doc de)
   :arglists (:def/arglists de)
   :private? (:def/private? de)
   :langs (vec (sort (:def/langs de)))
   :defined-by (:def/defined-by de)
   :drow (:def/drow de)
   :col (:def/col de)
   :end-drow (:def/end-drow de)
   :end-col (:def/end-col de)})

(defn- usage-counts
  "fqn → usage count resolved to `repo`'s definition."
  [db fqns repo]
  (into {} (for [fqn fqns] [fqn (count (resolve/usages-of db fqn repo))])))

(defn file-forms
  "Top-level forms of a worktree file, in source order."
  [repo path]
  (let [db (db/db)
        fe (d/entity db [:file/key (str repo "|" path)])]
    (when fe
      (let [be (:file/blob fe)
            occs (sort-by :occ/index (:blob/occs be))
            forms (vec (for [o occs
                             :let [f (:occ/form o)]]
                         {:index (:occ/index o)
                          :row (:occ/row o) :col (:occ/col o)
                          :end-row (:occ/end-row o) :end-col (:occ/end-col o)
                          :langs (vec (sort (:occ/langs o)))
                          :hash (:form/hash f)
                          :kind (:form/kind f)
                          :head (:form/head f)
                          :name (:form/name f)
                          :summary (:form/summary f)
                          :defs (vec (sort-by :drow (map def-data (:form/defs f))))
                          :children (vec (sort-by :index
                                                  (map (fn [c] {:index (:child/index c)
                                                                :drow (:child/drow c) :col (:child/col c)
                                                                :end-drow (:child/end-drow c)
                                                                :end-col (:child/end-col c)
                                                                :summary (:child/summary c)})
                                                       (:form/children f))))}))
            fqns (distinct (keep #(-> % :defs first :fqn) forms))
            counts (usage-counts db fqns repo)]
        {:repo repo :path path :sha (:file/sha fe)
         :ns (:blob/ns be) :ns-doc (:blob/ns-doc be) :error (:blob/error be)
         :aliases (:blob/aliases be)
         :forms (mapv (fn [f] (if-let [fqn (-> f :defs first :fqn)]
                                (assoc f :usage-count (get counts fqn 0))
                                f))
                      forms)}))))

(defn usages [fqn repo]
  (resolve/usages-of fqn repo))

(defn definitions
  "Where fqn is defined (any repo's worktree)."
  [fqn]
  (->> (d/q '[:find ?rp ?path ?orow ?drow
              :in $ ?fqn
              :where [?v :var/fqn ?fqn] [?d :def/var ?v] [?d :def/drow ?drow] [?f :form/defs ?d]
              [?o :occ/form ?f] [?o :occ/row ?orow] [?b :blob/occs ?o]
              [?file :file/blob ?b] [?file :file/path ?path] [?file :file/repo ?r]
              [?r :repo/path ?rp]]
            (db/db) fqn)
       (map (fn [[rp path orow drow]] {:repo rp :path path :row (+ orow drow)}))
       (sort-by (juxt :repo :path))
       vec))

;; ---------------------------------------------------------------------------
;; search

(defonce ^:private search-cache (atom nil))

(defn invalidate-search! [] (reset! search-cache nil))

(defn- search-rows []
  (or @search-cache
      (reset! search-cache
              (->> (d/q '[:find ?rp ?path ?orow ?fqn ?dname ?kind ?ns
                          :where [?file :file/repo ?r] [?r :repo/path ?rp] [?file :file/path ?path]
                          [?file :file/blob ?b] [?b :blob/occs ?o] [?o :occ/row ?orow]
                          [?o :occ/form ?f] [?f :form/defs ?d] [?d :def/name ?dname]
                          [?d :def/kind ?kind] [?d :def/var ?v] [?v :var/fqn ?fqn]
                          [(get-else $ ?b :blob/ns "") ?ns]]
                        (db/db))
                   (mapv (fn [[rp path row fqn dname kind ns]]
                           {:repo rp :path path :row row :fqn fqn :name dname :kind kind :ns ns
                            :hay (str/lower-case fqn)}))))))

(defn fuzzy-score
  "Subsequence match score of query in s (both lower-case); nil if no match.
  Higher is better: rewards contiguous runs, matches at word starts, and short targets."
  [q s]
  (let [qn (count q) sn (count s)]
    (loop [qi 0 si 0 score 0 run 0]
      (cond
        (= qi qn) (- score (* 0.01 sn))
        (= si sn) nil
        (= (.charAt ^String q qi) (.charAt ^String s si))
        (let [boundary? (or (zero? si) (#{\/ \. \- \:} (.charAt ^String s (dec si))))
              run (inc run)]
          (recur (inc qi) (inc si) (+ score 1 (* 2 (dec run)) (if boundary? 3 0)) run))
        :else (recur qi (inc si) score 0)))))

(defn search
  "Fuzzy name search + docstring full-text search across all repos."
  [q {:keys [limit] :or {limit 50}}]
  (let [ql (str/lower-case (str/trim (or q "")))]
    (if (str/blank? ql)
      {:names [] :docs []}
      (let [names (->> (search-rows)
                       (keep (fn [r] (when-let [s (fuzzy-score ql (:hay r))] (assoc r :score s))))
                       (sort-by (comp - :score))
                       (take limit)
                       (mapv #(dissoc % :hay)))
            docs (try
                   (->> (d/q '[:find ?rp ?path ?orow ?fqn ?doc ?kind
                               :in $ ?q
                               :where [(fulltext $ ?q) [[?d _ ?doc]]]
                               [?d :def/var ?v] [?v :var/fqn ?fqn] [?d :def/kind ?kind]
                               [?f :form/defs ?d] [?o :occ/form ?f] [?o :occ/row ?orow]
                               [?b :blob/occs ?o] [?file :file/blob ?b]
                               [?file :file/path ?path] [?file :file/repo ?r] [?r :repo/path ?rp]]
                             (db/db) q)
                        (take limit)
                        (mapv (fn [[rp path row fqn doc kind]]
                                {:repo rp :path path :row row :fqn fqn :doc doc :kind kind})))
                   (catch Exception _ []))]
        {:names names :docs docs}))))

;; ---------------------------------------------------------------------------
;; source text

(defn file-text [repo path]
  (let [f (fs/path repo path)]
    (when (fs/regular-file? f) (slurp (str f)))))

(defn- slice [text row col end-row end-col]
  (let [lines (str/split-lines text)
        lines (subvec (vec lines) (dec row) (min (count lines) end-row))]
    (if (= 1 (count lines))
      (subs (first lines) (dec col) (min (count (first lines)) (dec end-col)))
      (str/join "\n" (concat [(subs (first lines) (dec col))]
                             (butlast (rest lines))
                             [(let [l (last lines)] (subs l 0 (min (count l) (dec end-col))))])))))

(defn form-source
  "Exact source text of a top-level form in the worktree file."
  [repo path {:keys [row col end-row end-col]}]
  (when-let [t (file-text repo path)]
    (slice t row col end-row end-col)))
