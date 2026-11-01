(ns clj-browse.resolve
  "Cross-repo usage resolution. THE RULES LIVE HERE AND ONLY HERE.
  See docs/decisions/0001-usage-resolution.md before changing them.

  A usage is recorded as a reference to a var by fully-qualified name. When a
  repo `using` refers to fqn F, which indexed repos' definitions does it mean?

    0. If `using` itself defines F (in its worktree), it means its own definition.
    1. Else, if exactly one indexed repo defines F, it means that one.
    2. Else, prefer the defining repos that `using` depends on through a
       :local/root or git dependency in one of its projects.
    3. Else, it is ambiguous: all defining repos are candidates.

  Only each repo's current worktree snapshot is considered."
  (:require [clj-browse.db :as db]
            [clojure.string :as str]
            [datalevin.core :as d]))

(defn defining-repos
  "Repo paths whose worktree defines fqn."
  [db fqn]
  (set (d/q '[:find [?rp ...]
              :in $ ?fqn
              :where [?v :var/fqn ?fqn] [?d :def/var ?v] [?f :form/defs ?d]
              [?o :occ/form ?f] [?b :blob/occs ?o] [?file :file/blob ?b]
              [?file :file/repo ?r] [?r :repo/path ?rp]]
            db fqn)))

(defn normalize-git-url
  "git@github.com:a/b.git, https://github.com/a/b(.git) → github.com/a/b"
  [url]
  (when url
    (-> (str/trim url)
        (str/replace #"^[a-z+]+://" "")
        (str/replace #"^[^@/]+@" "")
        (str/replace #"^([^/:]+):(?!\d)" "$1/")
        (str/replace #"\.git/?$" "")
        (str/replace #"/$" "")
        str/lower-case)))

(defn- under? [path root]
  (or (= path root) (str/starts-with? path (str root "/"))))

(defn dependency-graph
  "{using-repo-path #{repo-paths it depends on}} derived from project
  :local-roots and :git-urls."
  [db]
  (let [repos (d/q '[:find ?rp ?url :where [?r :repo/path ?rp] [(get-else $ ?r :repo/remote-url "") ?url]] db)
        by-url (into {} (keep (fn [[rp url]] (when-let [u (normalize-git-url (not-empty url))] [u rp]))) repos)
        paths (map first repos)
        local (d/q '[:find ?rp ?lr :where [?p :project/repo ?r] [?r :repo/path ?rp] [?p :project/local-roots ?lr]] db)
        gits (d/q '[:find ?rp ?u :where [?p :project/repo ?r] [?r :repo/path ?rp] [?p :project/git-urls ?u]] db)]
    (reduce (fn [g [from to]] (if (and to (not= from to)) (update g from (fnil conj #{}) to) g))
            {}
            (concat
             (for [[rp lr] local, target paths :when (under? lr target)] [rp target])
             (for [[rp u] gits] [rp (by-url (normalize-git-url u))])))))

(defn resolve-fqn
  "Returns {:status :own|:unique|:dependency|:ambiguous|:unresolved :repos #{...}}."
  ([fqn using-repo] (resolve-fqn (db/db) fqn using-repo nil))
  ([db fqn using-repo deps]
   (let [defs (defining-repos db fqn)
         deps (or deps (dependency-graph db))]
     (cond
       (empty? defs) {:status :unresolved :repos #{}}
       (contains? defs using-repo) {:status :own :repos #{using-repo}}
       (= 1 (count defs)) {:status :unique :repos defs}
       :else (let [preferred (set (filter (get deps using-repo #{}) defs))]
               (if (seq preferred)
                 {:status :dependency :repos preferred}
                 {:status :ambiguous :repos defs}))))))

(defn usage-rows
  "All worktree usages of fqn: [{:repo :path :row :col :lang :form}]."
  [db fqn]
  (->> (d/q '[:find ?rp ?path ?orow ?drow ?col ?lang ?fname ?fhash
              :in $ ?fqn
              :where [?v :var/fqn ?fqn] [?u :usage/var ?v] [?f :form/usages ?u]
              [?u :usage/drow ?drow] [?u :usage/col ?col] [?u :usage/lang ?lang]
              [?f :form/hash ?fhash] [(get-else $ ?f :form/name "") ?fname]
              [?o :occ/form ?f] [?o :occ/row ?orow] [?b :blob/occs ?o]
              [?file :file/blob ?b] [?file :file/path ?path]
              [?file :file/repo ?r] [?r :repo/path ?rp]]
            db fqn)
       (map (fn [[rp path orow drow col lang fname _]]
              {:repo rp :path path :row (+ orow drow) :col col :lang lang :form fname}))
       (sort-by (juxt :repo :path :row))
       vec))

(defn usages-of
  "Usages of fqn that resolve to `def-repo`'s definition, per the rules above.
   Each row also carries :resolution."
  ([fqn def-repo] (usages-of (db/db) fqn def-repo))
  ([db fqn def-repo]
   (let [deps (dependency-graph db)
         cache (memoize (fn [using] (resolve-fqn db fqn using deps)))]
     (->> (usage-rows db fqn)
          (keep (fn [row]
                  (let [res (cache (:repo row))]
                    (when (contains? (:repos res) def-repo)
                      (assoc row :resolution (:status res))))))
          vec))))
