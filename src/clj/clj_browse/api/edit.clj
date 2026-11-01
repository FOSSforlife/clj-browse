(ns clj-browse.api.edit
  "Writing to source files: docstrings, whole forms, paste with requires,
  plus open-in-editor and live linting for the inline editor."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clj-browse.api :refer [handle]]
            [clj-browse.bb-tasks :as bb]
            [clj-browse.config :as config]
            [clj-browse.db :as db]
            [clj-browse.edit :as edit]
            [clj-browse.index :as index]
            [clj-browse.queries :as queries]
            [clj-browse.resolve :as resolve]
            [clj-kondo.core :as kondo]
            [clojure.string :as str]
            [datalevin.core :as d]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as parser]))

;; undo: per file, a stack of {:before text :after-sha sha}
(defonce undo-stack (atom {}))

(defn- abs-path [repo path] (str (fs/path repo path)))

(defn- current [repo path]
  (let [f (abs-path repo path)
        bs (fs/read-all-bytes f)]
    {:text (String. ^bytes bs "UTF-8") :sha (edit/blob-sha bs)}))

(defn- write! [repo path expected-sha new-text label]
  (let [{:keys [text]} (current repo path)
        res (edit/write-file-checked! (abs-path repo path) expected-sha new-text)]
    (if (:ok res)
      (do (swap! undo-stack update [repo path] (fnil conj []) {:before text :after-sha (:sha res) :label label})
          (index/index-worktree! repo [path])
          (queries/invalidate-search!)
          {:ok true :sha (:sha res) :file (queries/file-forms repo path)})
      {:ok false :reason :conflict
       :message "The file changed on disk since it was indexed. Refreshed — please retry."
       :file (do (index/index-worktree! repo [path]) (queries/file-forms repo path))})))

(defmethod handle :doc/set [_ {:keys [repo path pos doc sha]} _]
  (let [{:keys [text]} (current repo path)]
    (write! repo path sha (edit/set-docstring text pos doc) "Edit docstring")))

(defmethod handle :form/replace [_ {:keys [repo path pos text sha]} _]
  (let [cur (current repo path)]
    (write! repo path sha (edit/replace-form (:text cur) pos text) "Edit form")))

(defmethod handle :form/insert [_ {:keys [repo path after text sha]} _]
  (let [cur (current repo path)]
    (write! repo path sha (edit/insert-form (:text cur) after text) "Insert form")))

(defmethod handle :edit/undo [_ {:keys [repo path]} _]
  (let [stack (get @undo-stack [repo path])
        {:keys [before after-sha]} (peek stack)
        cur (current repo path)]
    (cond
      (nil? before) {:ok false :message "Nothing to undo"}
      (not= after-sha (:sha cur)) {:ok false :message "File changed since the edit; can't undo safely."}
      :else (do (spit (abs-path repo path) before)
                (swap! undo-stack update [repo path] pop)
                (index/index-worktree! repo [path])
                {:ok true :file (queries/file-forms repo path)}))))

(defmethod handle :edit/undo-info [_ {:keys [repo path]} _]
  (some-> (peek (get @undo-stack [repo path])) :label))

;; ---------------------------------------------------------------------------
;; copy / paste

(defn- qualified-syms [text]
  (let [out (volatile! #{})]
    (letfn [(walk [nd]
              (if (n/inner? nd)
                (run! walk (n/children nd))
                (when (= :token (n/tag nd))
                  (let [v (try (n/sexpr nd) (catch Exception _ nil))]
                    (when (qualified-symbol? v) (vswap! out conj v))))))]
      (walk (parser/parse-string-all text)))
    @out))

(defn- form-usages [hash]
  (d/q '[:find ?fqn ?ns ?name
         :in $ ?h
         :where [?f :form/hash ?h] [?f :form/usages ?u] [?u :usage/var ?v]
         [?v :var/fqn ?fqn] [?v :var/ns ?ns] [?v :var/name ?name]
         [(get-else $ ?v :var/keyword? false) ?kw] [(not ?kw)]]
       (db/db) hash))

(defn paste-plan
  [{:keys [source target]}]
  (let [src-text (:text (current (:repo source) (:path source)))
        src-aliases (edit/ns-aliases src-text)
        src-ns (:ns source)
        tgt (current (:repo target) (:path target))
        tgt-aliases (edit/ns-aliases (:text tgt))
        tgt-ns (:ns (queries/file-forms (:repo target) (:path target)))
        text (:text source)
        qsyms (qualified-syms text)
        specs (->> (form-usages (:hash source))
                   (keep (fn [[_ ns nm]]
                           (let [as-alias (some #(when (and (= nm (name %))
                                                            (= ns (get src-aliases (namespace %))))
                                                   (namespace %))
                                                qsyms)]
                             (cond
                               (= ns tgt-ns) nil
                               as-alias {:ns ns :as as-alias}
                               (some #(and (= nm (name %)) (= ns (namespace %))) qsyms) {:ns ns}
                               :else {:ns ns :refer [nm]}))))
                   (group-by :ns)
                   (map (fn [[ns ss]]
                          (let [as (some :as ss) refers (vec (sort (distinct (mapcat :refer ss))))]
                            (cond-> {:ns ns}
                              as (assoc :as as)
                              (seq refers) (assoc :refer refers)))))
                   (sort-by :ns) vec)
        conflicts (vec (for [{:keys [ns as]} specs
                             :when (and as (contains? tgt-aliases as) (not= ns (get tgt-aliases as)))]
                         {:alias as :wanted ns :existing (get tgt-aliases as)}))
        missing (vec (remove (fn [{:keys [ns as refer]}]
                               (and (some #(= ns %) (vals tgt-aliases)) (not refer)
                                    (or (nil? as) (= ns (get tgt-aliases as)))))
                             specs))
        deps (resolve/dependency-graph (db/db))
        warnings (cond-> []
                   (and (not= (:repo source) (:repo target))
                        (not (contains? (get deps (:repo target) #{}) (:repo source))))
                   (conj (str "The target repo's projects don't depend on "
                              (fs/file-name (:repo source))
                              " — required namespaces from it may not be on the classpath."))
                   (= src-ns tgt-ns)
                   (conj "Pasting into the same namespace: names may collide with the original."))]
    {:requires missing :conflicts conflicts :warnings warnings :sha (:sha tgt)}))

(defmethod handle :paste/plan [_ params _] (paste-plan params))

(defmethod handle :paste/apply [_ {:keys [target text requires sha]} _]
  (let [{:keys [repo path after]} target
        cur (current repo path)
        inserted (edit/insert-form (:text cur) after text)
        {:keys [source]} (if (seq requires)
                           (edit/add-requires inserted requires)
                           {:source inserted})]
    (write! repo path sha source "Paste form")))

;; ---------------------------------------------------------------------------
;; external editor

(defmethod handle :file/open-external [_ {:keys [repo path line]} _]
  (let [file (abs-path repo path)
        tmpl (or (:editor-command (config/get-config)) "$EDITOR")
        tmpl (if (str/includes? tmpl "$EDITOR")
               (str/replace tmpl "$EDITOR" (or (System/getenv "VISUAL") (System/getenv "EDITOR") "xdg-open"))
               tmpl)
        cmd (str/replace (str/replace tmpl "{line}" (str (or line 1))) "{file}" file)
        argv (cond-> (bb/split-args cmd)
               (not (str/includes? tmpl "{file}")) (conj file))]
    (p/process {:dir repo :out :discard :err :discard} argv)
    {:ok true :command argv}))

;; ---------------------------------------------------------------------------
;; lint for the inline editor

(defmethod handle :lint [_ {:keys [repo path text]} _]
  (let [cfg-dir (index/kondo-config-dir repo)
        res (with-in-str text
              (kondo/run! (cond-> {:lint ["-"] :filename path :cache false
                                   :lang (case (fs/extension path) "cljs" :cljs "cljc" :cljc :clj)}
                            cfg-dir (assoc :config-dir cfg-dir))))]
    (mapv #(select-keys % [:row :col :end-row :end-col :level :message :type]) (:findings res))))
