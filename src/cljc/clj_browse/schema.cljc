(ns clj-browse.schema
  "Datalevin schema for the clj-browse index. See docs/data-model.md.

  The model is git-shaped:

    repo ──< ref ──> commit ──> tree ──< entry ──> blob ──< occurrence ──> form ──< def ──> var
     │                                                                     └──< usage ──> var
     └──< file (worktree snapshot) ──> blob
     └──< project ──< path

  * `blob` is an *analysis* of a git blob: keyed by blob SHA + file extension +
    clj-kondo config hash, because the same bytes analyze differently as .clj
    vs .cljs, or under a different `:lint-as` config.
  * `form` is one version of a top-level form, keyed by a hash of its
    normalized text + namespace + resolved usages. Positions live on the
    occurrence, not the form, so a form that moves keeps its identity.
  * `var` is the stable identity of a definition (`:var/fqn`), shared across
    repos and history. A var with no defining form in any worktree is a stub.")

(def extractor-version
  "Bump when analysis output changes; blobs with an older version get re-analyzed."
  4)

(def schema
  {;; ---- scan roots -------------------------------------------------------
   :root/path            {:db/unique :db.unique/identity}

   ;; ---- repos ------------------------------------------------------------
   :repo/path            {:db/unique :db.unique/identity}   ; absolute
   :repo/name            {}
   :repo/remote-url      {}
   :repo/added-at        {}
   :repo/full-history?   {}
   :repo/commit-window   {}
   :repo/pinned-branches {:db/cardinality :db.cardinality/many} ; ref names
   :repo/kondo-config-hash {}
   :repo/repl-config     {}                                   ; edn map
   :repo/position        {}                                   ; sort order in UI

   ;; ---- refs -------------------------------------------------------------
   :ref/key              {:db/unique :db.unique/identity}   ; "<repo-path>|<refname>"
   :ref/repo             {:db/valueType :db.type/ref}
   :ref/name             {}                                   ; refs/heads/main
   :ref/kind             {}                                   ; :branch :remote :tag
   :ref/tracked?         {}
   :ref/commit           {:db/valueType :db.type/ref}

   ;; ---- commits ----------------------------------------------------------
   :commit/sha           {:db/unique :db.unique/identity}
   :commit/repos         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :commit/parents       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :commit/parent-shas   {:db/cardinality :db.cardinality/many}
   :commit/tree          {:db/valueType :db.type/ref}
   :commit/author        {}
   :commit/email         {}
   :commit/time          {}                                   ; epoch ms
   :commit/subject       {}
   :commit/boundary?     {}   ; true when some parent is outside the indexed window
   :commit/indexed?      {}

   ;; ---- trees (only Clojure-relevant entries are stored) ----------------
   :tree/sha             {:db/unique :db.unique/identity}
   :tree/entries         {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                          :db/isComponent true}
   :entry/name           {}
   :entry/sha            {}
   :entry/blob           {:db/valueType :db.type/ref}
   :entry/tree           {:db/valueType :db.type/ref}

   ;; ---- blobs (analyses) -------------------------------------------------
   :blob/key             {:db/unique :db.unique/identity}   ; "<sha>.<ext>@<cfg>"
   :blob/sha             {}
   :blob/ext             {}
   :blob/extractor-version {}
   :blob/error           {}
   :blob/ns              {}                                   ; ns name string
   :blob/ns-doc          {}
   :blob/requires        {:db/cardinality :db.cardinality/many} ; ns name strings
   :blob/aliases         {}                                   ; {"alias" "ns"} edn
   :blob/occs            {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                          :db/isComponent true}

   ;; ---- occurrences ------------------------------------------------------
   :occ/form             {:db/valueType :db.type/ref}
   :occ/index            {}
   :occ/row              {}
   :occ/col              {}
   :occ/end-row          {}
   :occ/end-col          {}
   :occ/langs            {:db/cardinality :db.cardinality/many}

   ;; ---- forms ------------------------------------------------------------
   :form/hash            {:db/unique :db.unique/identity}
   :form/kind            {}   ; :defn :def :defmacro ... :ns :comment :reader-conditional :other
   :form/head            {}   ; head symbol as written, e.g. "rf/reg-event-db"
   :form/name            {}   ; primary display name
   :form/summary         {}   ; one-line text for non-definition forms
   :form/defs            {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                          :db/isComponent true}
   :form/usages          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                          :db/isComponent true}
   :form/children        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many
                          :db/isComponent true}   ; inner forms of (comment ...)

   ;; ---- definitions inside a form ---------------------------------------
   :def/var              {:db/valueType :db.type/ref}
   :def/kind             {}
   :def/name             {}
   :def/doc              {:db/fulltext true}
   :def/arglists         {}   ; vector of strings
   :def/private?         {}
   :def/langs            {:db/cardinality :db.cardinality/many}
   :def/defined-by       {}   ; e.g. "clojure.core/defn"
   :def/drow             {}   ; row offset from form start
   :def/col              {}
   :def/end-drow         {}
   :def/end-col          {}

   ;; ---- usages inside a form --------------------------------------------
   :usage/var            {:db/valueType :db.type/ref}
   :usage/lang           {}
   :usage/drow           {}
   :usage/col            {}

   ;; ---- children of comment blocks --------------------------------------
   :child/index          {}
   :child/drow           {}
   :child/col            {}
   :child/end-drow       {}
   :child/end-col        {}
   :child/summary        {}

   ;; ---- vars ---------------------------------------------------------------
   :var/fqn              {:db/unique :db.unique/identity}   ; "ns/name" or ":kw/name"
   :var/ns               {}
   :var/name             {}
   :var/keyword?         {}

   ;; ---- worktree snapshot ----------------------------------------------
   :file/key             {:db/unique :db.unique/identity}   ; "<repo-path>|<rel-path>"
   :file/repo            {:db/valueType :db.type/ref}
   :file/path            {}
   :file/sha             {}
   :file/blob            {:db/valueType :db.type/ref}
   :file/mtime           {}

   ;; ---- projects ----------------------------------------------------------
   :project/key          {:db/unique :db.unique/identity}   ; "<repo-path>|<rel-file>"
   :project/repo         {:db/valueType :db.type/ref}
   :project/type         {}
   :project/file         {}
   :project/dir          {}
   :project/name         {}
   :project/paths        {}   ; vector of {:path/dir :path/tags}
   :project/aliases      {}
   :project/builds       {}
   :project/local-roots  {:db/cardinality :db.cardinality/many}
   :project/git-urls     {:db/cardinality :db.cardinality/many}
   :project/error        {}

   ;; ---- run history -------------------------------------------------------
   :run/repo             {:db/valueType :db.type/ref}
   :run/fqn              {}
   :run/arity            {}
   :run/args             {}   ; vector of source strings
   :run/time             {}
   :run/result           {}
   :run/error?           {}})

(def runnable-kinds #{:defn :defn- :defmulti})

(def keyword-reg-kinds
  "re-frame registrations get named by their keyword."
  #{:reg-event-db :reg-event-fx :reg-event-ctx :reg-sub :reg-sub-raw :reg-fx :reg-cofx})
