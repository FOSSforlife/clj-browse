> Prompt: Using the [[rewrite-clj#Parser API|rewrite-clj Parser API]] described below, how would I convert a parse result into a datascript ingestion so that I can query the namespaces, functions, etc in my codebases?

Use the parser API to read the forms, pull facts out of the nodes, and transact those facts into Datascript as entity maps. The main idea is to give namespaces and vars a **unique identity** attribute. Then references such as calls and requires will automatically upsert stub entities, and you can query across files and across codebases.

## 1. Schema

```clojure
(def schema
  {:file/path     {:db/unique :db.unique/identity}
   :ns/name       {:db/unique :db.unique/identity}
   :ns/file       {:db/valueType :db.type/ref}
   :ns/requires   {:db/valueType   :db.type/ref
                   :db/cardinality :db.cardinality/many}
   :var/fqn       {:db/unique :db.unique/identity}   ; "my.ns/my-fn"
   :var/ns        {:db/valueType :db.type/ref}
   :var/calls     {:db/valueType   :db.type/ref
                   :db/cardinality :db.cardinality/many}
   :var/arglists  {:db/cardinality :db.cardinality/many}})
```

## 2. Extraction

```clojure
(ns code-index.core
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [datascript.core :as d]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; --- node helpers -------------------------------------------------------

(defn code-children
  "Children minus whitespace, comments and #_ forms."
  [node]
  (remove #(or (n/whitespace-or-comment? %) (= :uneval (n/tag %)))
          (n/children node)))

(defn safe-sexpr [node]
  (try (n/sexpr node) (catch Exception _ nil)))

(defn list-head [node]
  (when (= :list (n/tag node))
    (some-> node code-children first safe-sexpr)))

(defn unwrap-meta
  "^:private foo => [foo {:private true}]"
  [node]
  (if (= :meta (n/tag node))
    (let [[m target] (code-children node)
          mv (safe-sexpr m)]
      [(safe-sexpr target)
       (cond (keyword? mv) {mv true} (map? mv) mv :else {})])
    [(safe-sexpr node) {}]))

(defn all-nodes [node]
  (tree-seq n/inner? code-children node))

;; --- ns form ------------------------------------------------------------

(defn parse-requires
  "Returns {:requires #{ns-sym} :aliases {alias ns-sym} :refers {sym ns-sym}}"
  [ns-form]
  (let [specs (->> ns-form
                   (filter #(and (seq? %) (= :require (first %))))
                   (mapcat rest))]
    (reduce
     (fn [acc spec]
       (let [[lib & opts] (if (sequential? spec) spec [spec])
             {:keys [as refer]} (apply hash-map opts)]
         (cond-> (update acc :requires conj lib)
           as                (assoc-in [:aliases as] lib)
           (sequential? refer)
           (update :refers merge (zipmap refer (repeat lib))))))
     {:requires #{} :aliases {} :refers {}}
     specs)))

;; --- symbol resolution --------------------------------------------------

(defn resolve-sym [sym {:keys [ns-name aliases refers local-defs]}]
  (cond
    (qualified-symbol? sym)
    (let [ns* (symbol (namespace sym))]
      (str (get aliases ns* ns*) "/" (name sym)))
    (contains? local-defs sym) (str ns-name "/" sym)
    (contains? refers sym)     (str (refers sym) "/" sym)
    (resolve 'clojure.core sym) (str "clojure.core/" sym) ; best effort
    :else nil))

;; --- def forms ----------------------------------------------------------

(def def-syms '#{def defn defn- defmacro defmulti defonce
                 defprotocol defrecord deftype})

(defn arglists [children]
  (let [vs (filter #(= :vector (n/tag %)) children)]
    (if (seq vs)
      [(n/string (first vs))]                     ; single arity
      (->> children                                ; multi arity
           (filter #(= :list (n/tag %)))
           (keep #(some->> (code-children %) first
                           (filter-vec-node) n/string))))))

(defn filter-vec-node [node]
  (when (= :vector (n/tag node)) node))

(defn def->tx [node ctx]
  (let [[head name-node & more] (code-children node)
        kind      (safe-sexpr head)
        [sym md]  (unwrap-meta name-node)
        doc       (let [d (some-> more first safe-sexpr)]
                    (when (string? d) d))
        {:keys [row col]} (meta node)
        calls     (->> more
                       (mapcat all-nodes)
                       (filter #(= :token (n/tag %)))
                       (keep safe-sexpr)
                       (filter symbol?)
                       (keep #(resolve-sym % ctx))
                       distinct)]
    (cond-> {:var/fqn     (str (:ns-name ctx) "/" sym)
             :var/name    (str sym)
             :var/kind    (keyword kind)
             :var/ns      [:ns/name (str (:ns-name ctx))]
             :var/private (boolean (or (= kind 'defn-) (:private md)))
             :var/row     row
             :var/col     col
             :var/source  (n/string node)
             :var/calls   (mapv (fn [fqn] {:var/fqn fqn}) calls)}
      doc (assoc :var/doc doc)
      (#{'defn 'defn- 'defmacro} kind)
      (assoc :var/arglists (arglists more)))))

;; --- file ---------------------------------------------------------------

(defn file->tx [path]
  (let [forms   (code-children (p/parse-file-all path))
        ns-node (first (filter #(= 'ns (list-head %)) forms))
        ns-form (safe-sexpr ns-node)
        ns-name (second ns-form)
        reqs    (parse-requires ns-form)
        defs    (filter #(def-syms (list-head %)) forms)
        local   (set (map #(first (unwrap-meta (second (code-children %))))
                          defs))
        ctx     (assoc reqs :ns-name ns-name :local-defs local)]
    (concat
     [{:file/path (str path)}
      {:ns/name     (str ns-name)
       :ns/file     [:file/path (str path)]
       :ns/requires (mapv (fn [r] {:ns/name (str r)}) (:requires reqs))}]
     (map #(def->tx % ctx) defs))))

(defn index-dirs [conn & dirs]
  (doseq [dir  dirs
          f    (file-seq (io/file dir))
          :when (re-find #"\.clj[cs]?$" (.getName f))]
    (try
      (d/transact! conn (vec (file->tx f)))
      (catch Exception e
        (println "skip" (str f) (ex-message e))))))
```

## 3. Usage and queries

```clojure
(def conn (d/create-conn schema))
(index-dirs conn "src" "../other-project/src")

;; All public fns in a namespace
(d/q '[:find ?name ?args
       :in $ ?ns
       :where [?n :ns/name ?ns]
              [?v :var/ns ?n]
              [?v :var/kind :defn]
              [?v :var/private false]
              [?v :var/name ?name]
              [?v :var/arglists ?args]]
     @conn "my.app.core")

;; Who calls a given fn?
(d/q '[:find [?caller ...]
       :in $ ?target
       :where [?t :var/fqn ?target]
              [?c :var/calls ?t]
              [?c :var/fqn ?caller]]
     @conn "my.app.db/save!")

;; Namespaces nobody requires (entry points / dead code candidates)
(d/q '[:find [?name ...]
       :where [?n :ns/name ?name]
              [?n :ns/file _]
              (not [_ :ns/requires ?n])]
     @conn)

;; Transitive dependency closure via rules
(def rules
  '[[(depends ?a ?b) [?a :ns/requires ?b]]
    [(depends ?a ?b) [?a :ns/requires ?x] (depends ?x ?b)]])

(d/q '[:find [?dep ...]
       :in $ % ?ns
       :where [?a :ns/name ?ns]
              (depends ?a ?d)
              [?d :ns/name ?dep]]
     @conn rules "my.app.core")
```

## Things to watch for

- **`n/sexpr` can throw.** It fails on some nodes, like namespaced maps without context or certain reader macros. That's why everything goes through `safe-sexpr`.
- **Positions:** the parser puts `:row`, `:col`, `:end-row` and `:end-col` in each node's `meta`, so you get source locations without extra work.
- **Stub entities:** a call to something you haven't indexed, like `clojure.core/map`, creates an entity with only `:var/fqn`. To list only the vars defined in your code, add `[?v :var/ns _]` to the query.
- **Call resolution is a heuristic.** It doesn't track local bindings, so a `let`-bound `x` that has the same name as a var will look like a call to that var. For exact resolution, use **clj-kondo's analysis output** (`:analysis {:var-definitions true :var-usages true}`). It gives you resolved definitions and usages as data, so you can transact it the same way and keep rewrite-clj for the parts that need the source text.
- **`(in-ns ...)`, `.cljc` reader conditionals and macros that define vars** like `defn-spec` aren't handled. To support custom def macros, add them to `def-syms`.

----
> Prompt: What are some important considerations for building this database in a way that allows the user to browse codebases over time by symbolically indexing each commit?

The key decision is to **model the database the way git does**. Commits point to trees, trees point to file blobs, and blobs hold the forms you extract. Snapshot-per-commit and valid-time intervals both run into problems that this layout avoids.

## 1. Don't rely on Datascript for history

Datascript has no `as-of` or `history`, and it keeps everything in memory. Indexing every commit will either use too much memory or force you to build versioning yourself. You have three options:

- **Datahike**: an API close to Datascript's, durable storage, and `as-of`/`history` built in. It's the easiest move from your current code.
- **Datalevin**: fast and durable with a Datascript-compatible API. It has no history, so you'd model versions explicitly (as below).
- **XTDB**: bitemporal, which lets you use commit time as valid time.

Even with Datahike, **don't use transaction time to represent commit time.** Git history is a DAG, not a line. Make commits first-class entities.

## 2. Separate stable identity from versioned facts

A var like `my.app/save!` has many versions. If you upsert on `:var/fqn` as in the earlier version, each commit overwrites the last. Split it into two kinds of entity:

| Entity | Identity | Holds |
|---|---|---|
| `:var/fqn` | stable across time | just the name |
| `:form/hash` | hash of the normalized form | kind, arglists, doc, calls, position |
| `:blob/sha` | git blob SHA | the ns and the forms in that file version |
| `:commit/sha` | git commit SHA | parents, author, date, tree entries |

```clojure
(def schema
  {:commit/sha     {:db/unique :db.unique/identity}
   :commit/parents {:db/valueType :db.type/ref
                    :db/cardinality :db.cardinality/many}
   :commit/entries {:db/valueType :db.type/ref        ; path -> blob
                    :db/cardinality :db.cardinality/many
                    :db/isComponent true}
   :entry/path     {}
   :entry/blob     {:db/valueType :db.type/ref}
   :blob/sha       {:db/unique :db.unique/identity}
   :blob/ns        {:db/valueType :db.type/ref}
   :blob/forms     {:db/valueType :db.type/ref
                    :db/cardinality :db.cardinality/many}
   :form/hash      {:db/unique :db.unique/identity}
   :form/var       {:db/valueType :db.type/ref}       ; -> :var/fqn
   :form/calls     {:db/valueType :db.type/ref
                    :db/cardinality :db.cardinality/many}
   :var/fqn        {:db/unique :db.unique/identity}
   :ns/name        {:db/unique :db.unique/identity}})
```

This gives you deduplication for free. A file that didn't change between commits keeps the same blob SHA, so it's parsed once and shared. A var that didn't change keeps the same form hash, even if the file around it changed.

## 3. Indexing incrementally

- Walk commits in topological order (`git rev-list --topo-order --reverse`), not by date. Author and committer dates can be out of order.
- For each commit, get its tree with `git ls-tree -r <sha>`. **Only parse blobs you haven't seen before.** `git cat-file --batch` lets you read blobs without checking anything out.
- Make every step idempotent so you can resume after a crash. Upserting on SHAs and hashes takes care of this.
- **Stamp each blob with an extractor version.** When you improve the extraction logic, re-index only the blobs that have an old version.

## 4. Hashing forms

- Hash a normalized form, not the raw text. Strip whitespace and comments (for example, `pr-str` of the cleaned sexpr), so that reformatting doesn't create a new version. Decide whether docstring changes should count as a new version.
- Keep row and column on the blob-to-form relationship, or as a separate occurrence entity, not on the form. The same form can sit on different lines in different file versions.
- **Call resolution depends on the ns context.** The same body can resolve `save!` to different namespaces if the `:require` aliases changed. Either include the resolved calls in the hash, or hash the resolution context along with the body.

## 5. Renames and moves

Stable identity breaks when a function is renamed or moved to another namespace. You can handle this in a few ways:

- Use git's rename detection (`git diff -M`) for file moves.
- Match vars by body hash: if `a/foo` disappears and `b/foo` (or `a/bar`) appears in the same commit with an identical or very similar body, record a `:var/renamed-from` link.
- Store these links as inferred facts with a confidence score. Don't merge the identities.

## 6. Branches and merges

- Interval models like "valid from commit X to commit Y" don't work on a DAG. "Present at commit C" should mean "reachable through C's tree," which the git-shaped model gives you directly.
- Questions like "when was this introduced?" need ancestry, so store `:commit/parents` and query it with recursive rules. Cache the first-parent chain for each branch, since most timeline views follow it.
- Index refs (branches and tags) as entities that point to commits, so users can browse by name.

## 7. Storage and scale

- Don't store full source text. Store blob SHA plus offsets and fetch the text from git when needed. Git is already a content-addressed store.
- Even with deduplication, call edges are the biggest cost (forms × calls). Make sure the backend can handle tens of millions of datoms.
- **Record parse failures** as facts (`:blob/error`). Old commits will have broken syntax, unusual reader macros, and merge-conflict markers. One bad file shouldn't stop indexing.

## 8. Example queries

```clojure
;; Vars present at a commit
(def at-commit
  '[[(at ?c ?fqn)
     [?c :commit/entries ?e] [?e :entry/blob ?b]
     [?b :blob/forms ?f] [?f :form/var ?v] [?v :var/fqn ?fqn]]])

;; Vars added between two commits
(d/q '[:find [?fqn ...]
       :in $ % ?old ?new
       :where [?n :commit/sha ?new] (at ?n ?fqn)
              [?o :commit/sha ?old] (not (at ?o ?fqn))]
     db at-commit "abc123" "def456")

;; Every distinct version of a var, with the blobs it appeared in
(d/q '[:find ?hash ?blob
       :in $ ?fqn
       :where [?v :var/fqn ?fqn] [?f :form/var ?v]
              [?f :form/hash ?hash] [?b :blob/forms ?f]
              [?b :blob/sha ?blob]]
     db "my.app.db/save!")
```

Some views are expensive to compute with ad-hoc queries: per-var lifetime (first and last commit seen), churn counts, and how the call graph changes over time. Compute these in a second pass after indexing and store them as their own entities.

## 9. Use clj-kondo's analysis

If you switch call resolution to clj-kondo, cache its analysis per blob SHA the same way. It's the expensive step, and the deduplication applies to it just as well.
