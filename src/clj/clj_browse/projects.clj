(ns clj-browse.projects
  "Discovery of git repos and Clojure projects (deps.edn, project.clj,
  shadow-cljs.edn) and their source files. Project files are only ever read
  as data with edamame, never evaluated."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [edamame.core :as e]))

(def skip-dir-names
  #{"node_modules" "target" ".git" ".cpcache" ".shadow-cljs" "out" ".clj-kondo"
    ".lsp" ".calva" ".idea" ".vscode" "dist" ".cljs_node_repl"})

(def clojure-exts #{"clj" "cljs" "cljc" "bb"})

(defn skip-dir?
  "True if `dir-name` is one of `skip-dir-names`."
  [dir-name]
  (contains? skip-dir-names (str dir-name)))

(defn clojure-file?
  "True if `path` has a Clojure source extension."
  [path]
  (contains? clojure-exts (fs/extension (str path))))

(defn- hidden-name? [n]
  (str/starts-with? (str n) "."))

(defn- fname [p]
  (some-> (fs/file-name p) str))

(defn- slashes [s]
  (str/replace (str s) "\\" "/"))

(defn- norm-abs [p]
  (fs/normalize (fs/absolutize (fs/path p))))

(defn- rel-str
  "Repo-relative, forward-slash string; \"\" for the repo itself."
  [repo p]
  (let [r (slashes (fs/relativize repo (norm-abs p)))]
    (if (= r ".") "" r)))

;; ---------------------------------------------------------------------------
;; Repos

(defn find-repos
  "Walk `root` (string or path) looking for git repos: a directory containing a
  `.git` dir or `.git` file. Does not descend into a found repo; skips
  skip-dir-names and hidden dirs. Opts: :max-depth (default 6). Returns a
  sorted vector of absolute normalized path strings."
  ([root] (find-repos root {}))
  ([root {:keys [max-depth] :or {max-depth 6}}]
   (let [root (norm-abs root)
         found (volatile! [])]
     (when (fs/directory? root)
       (fs/walk-file-tree
        root
        {:max-depth (inc max-depth)
         :pre-visit-dir
         (fn [dir _]
           (cond
             (fs/exists? (fs/path dir ".git"))
             (do (vswap! found conj (str dir)) :skip-subtree)

             (and (not= dir root)
                  (let [n (fname dir)] (or (skip-dir? n) (hidden-name? n))))
             :skip-subtree

             :else :continue))
         :visit-file (fn [_ _] :continue)
         :visit-file-failed (fn [_ _] :continue)
         :post-visit-dir (fn [_ _] :continue)}))
     (vec (sort @found)))))

;; ---------------------------------------------------------------------------
;; Reading project files

(def ^:private read-opts
  {:all true
   :auto-resolve (fn [_] 'user)
   :readers (fn [_tag] (fn [form] form))
   :read-cond :preserve
   :features #{:clj}})

(defn- read-forms [file]
  (e/parse-string-all (slurp (str file)) read-opts))

(defn- string-vec? [v]
  (and (vector? v) (every? string? v)))

(defn- kw-str [k]
  (if (keyword? k) (subs (str k) 1) (str k)))

(defn- norm-rel
  "Normalize `p` relative to project dir `dir` (repo-relative); result is repo-relative."
  [repo dir p]
  (rel-str repo (fs/path repo dir (str p))))

(defn- build-paths
  "entries: seq of [path-string tag]. Existing dirs only, deduped with merged tags."
  [repo dir entries]
  (let [merged (reduce (fn [m [p tag]]
                         (let [d (norm-rel repo dir p)]
                           (update m d (fnil conj #{}) tag)))
                       (array-map) entries)]
    (vec (for [[d tags] merged
               :when (fs/directory? (fs/path repo d))]
           {:path/dir d :path/tags tags}))))

;; deps.edn

(defn- resolve-path-refs [aliases coll]
  (when (sequential? coll)
    (mapcat (fn [x]
              (cond
                (string? x) [x]
                (keyword? x) (let [v (get aliases x)] (when (string-vec? v) v))
                :else nil))
            coll)))

(defn- dep-maps [m]
  (let [aliases (when (map? (:aliases m)) (:aliases m))]
    (concat (when (map? (:deps m)) [(:deps m)])
            (for [[_ a] aliases
                  :when (map? a)
                  k [:extra-deps :replace-deps :deps]
                  :when (map? (get a k))]
              (get a k)))))

(defn- all-coords [m]
  (for [dm (dep-maps m)
        [lib coord] dm
        :when (map? coord)]
    [lib coord]))

(defn- git-url-for [lib coord]
  (or (when (string? (:git/url coord)) (:git/url coord))
      (when (and (or (:git/sha coord) (:git/tag coord)) (symbol? lib) (namespace lib))
        (let [ns* (namespace lib)
              n (name lib)]
          (some (fn [[prefix host]]
                  (when (str/starts-with? ns* prefix)
                    (str "https://" host "/" (subs ns* (count prefix)) "/" n)))
                [["io.github." "github.com"] ["com.github." "github.com"]
                 ["io.gitlab." "gitlab.com"] ["com.gitlab." "gitlab.com"]])))))

(defn- deps-project [repo dir file-abs]
  (let [[m err] (try (let [f (first (read-forms file-abs))]
                       (if (or (nil? f) (map? f)) [(or f {}) nil] [{} "deps.edn is not a map"]))
                     (catch Throwable t [{} (or (ex-message t) (str t))]))
        aliases (when (map? (:aliases m)) (:aliases m))
        top (if (contains? m :paths) (:paths m) ["src"])
        entries (concat
                 (for [p (resolve-path-refs aliases top)] [p "default"])
                 (for [[ak a] aliases
                       :when (map? a)
                       k [:extra-paths :paths]
                       p (resolve-path-refs aliases (get a k))]
                   [p (str ":" (kw-str ak))]))]
    {:project/type :deps
     :project/aliases (vec (map kw-str (keys aliases)))
     :project/paths (build-paths repo dir entries)
     :project/builds []
     :project/local-roots (->> (all-coords m)
                               (keep (fn [[_ c]] (when (string? (:local/root c)) (:local/root c))))
                               (map #(str (norm-abs (fs/path repo dir %))))
                               distinct vec)
     :project/git-urls (->> (all-coords m) (keep (fn [[l c]] (git-url-for l c))) distinct vec)
     :project/error err}))

;; project.clj

(defn- lein-project [repo dir file-abs]
  (let [[forms err] (try [(read-forms file-abs) nil]
                         (catch Throwable t [nil (or (ex-message t) (str t))]))
        dp (first (filter #(and (seq? %) (= 'defproject (first %))) forms))
        pname (when (and dp (second dp)) (str (second dp)))
        kvs (when dp (apply hash-map (let [r (drop 3 dp)] (take (* 2 (quot (count r) 2)) r))))
        lit (fn [m k default]
              (let [v (get m k ::none)]
                (cond (string-vec? v) v
                      (= v ::none) default
                      :else default)))
        profiles (when (map? (:profiles kvs)) (:profiles kvs))
        entries (concat
                 (for [p (lit kvs :source-paths ["src"])] [p "default"])
                 (for [p (lit kvs :test-paths ["test"])] [p ":test"])
                 (for [[pk pv] profiles
                       :when (map? pv)
                       k [:source-paths :test-paths]
                       :let [v (get pv k)]
                       :when (string-vec? v)
                       p v]
                   [p (str ":" (kw-str pk))]))]
    (cond-> {:project/type :lein
             :project/aliases (vec (map kw-str (keys profiles)))
             :project/paths (build-paths repo dir entries)
             :project/builds []
             :project/local-roots []
             :project/git-urls []
             :project/error err}
      pname (assoc :project/name pname))))

;; shadow-cljs.edn

(defn- shadow-project [repo dir file-abs]
  (let [[m err] (try (let [f (first (read-forms file-abs))]
                       (if (or (nil? f) (map? f)) [(or f {}) nil] [{} "shadow-cljs.edn is not a map"]))
                     (catch Throwable t [{} (or (ex-message t) (str t))]))
        default (if (contains? m :deps) [] ["src"])
        sp (let [v (:source-paths m ::none)]
             (if (string-vec? v) v default))]
    {:project/type :shadow
     :project/aliases []
     :project/paths (build-paths repo dir (for [p sp] [p "default"]))
     :project/builds (if (map? (:builds m)) (vec (map kw-str (keys (:builds m)))) [])
     :project/local-roots []
     :project/git-urls []
     :project/error err}))

(def ^:private project-files
  {"deps.edn" [:deps deps-project]
   "project.clj" [:lein lein-project]
   "shadow-cljs.edn" [:shadow shadow-project]})

(defn- walk-dirs
  "Walk `root` calling (on-file path) for each regular file, skipping skip and hidden dirs."
  [root on-file]
  (fs/walk-file-tree
   root
   {:pre-visit-dir (fn [dir _]
                     (let [n (fname dir)]
                       (if (and (not= dir root) (or (skip-dir? n) (hidden-name? n)))
                         :skip-subtree
                         :continue)))
    :visit-file (fn [f _] (on-file f) :continue)
    :visit-file-failed (fn [_ _] :continue)
    :post-visit-dir (fn [_ _] :continue)}))

(defn discover-projects
  "Find deps.edn, project.clj and shadow-cljs.edn anywhere in the repo.
  Returns a vector of project maps sorted by :project/file."
  [repo-path]
  (let [repo (norm-abs repo-path)
        files (volatile! [])]
    (when (fs/directory? repo)
      (walk-dirs repo (fn [f] (when (contains? project-files (fname f))
                                (vswap! files conj f)))))
    (->> @files
         (map (fn [f]
                (let [[ptype build] (get project-files (fname f))
                      dir (rel-str repo (fs/parent f))
                      p (build repo dir f)]
                  (merge {:project/name (let [dn (if (= dir "") (fname repo) (fname dir))]
                                          dn)}
                         p
                         {:project/type ptype
                          :project/file (rel-str repo f)
                          :project/dir dir}))))
         (sort-by :project/file)
         vec)))

;; ---------------------------------------------------------------------------
;; Source files

(defn list-source-files
  "Given `repo-path` and a coll of repo-relative dirs, return a sorted distinct
  vector of repo-relative Clojure file paths (forward slashes) beneath them."
  [repo-path dirs]
  (let [repo (norm-abs repo-path)
        out (volatile! #{})]
    (doseq [d dirs
            :let [root (norm-abs (fs/path repo (str d)))]
            :when (fs/directory? root)]
      (fs/walk-file-tree
       root
       {:pre-visit-dir (fn [dir _]
                         (if (and (not= dir root) (skip-dir? (fname dir)))
                           :skip-subtree
                           :continue))
        :visit-file (fn [f _]
                      (when (clojure-file? f) (vswap! out conj (rel-str repo f)))
                      :continue)
        :visit-file-failed (fn [_ _] :continue)
        :post-visit-dir (fn [_ _] :continue)}))
    (vec (sort @out))))
