(ns clj-browse.analyze
  "Turn Clojure source text into plain data describing its top-level forms.

  clj-kondo (as a library) supplies definitions, usages, arglists and docstrings,
  honoring the repo's .clj-kondo config (hooks, :lint-as). rewrite-clj supplies
  the exact top-level form boundaries, including forms kondo doesn't report
  (comment blocks, side-effect calls).

  Many files are analyzed in one kondo run (`analyze-batch`): the texts are
  written to a temp dir and linted in parallel, which is far faster than one
  run per file."
  (:require [babashka.fs :as fs]
            [clj-browse.git :as git]
            [clj-kondo.core :as kondo]
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as parser]))

(def ignored-usage-nses
  "Usages of these namespaces are not recorded: they'd dominate the index
  without helping answer 'who uses my code'."
  '#{clojure.core cljs.core})

(def kondo-analysis-config
  {:var-definitions {:shallow false}
   :var-usages true
   :namespace-definitions true
   :namespace-usages true
   :arglists true
   :keywords true})

(defn ext-of [path] (fs/extension path))

(defn file-langs [ext]
  (case ext "cljs" #{:cljs} "cljc" #{:clj :cljs} #{:clj}))

;; ---------------------------------------------------------------------------
;; rewrite-clj helpers

(defn- code-node? [node]
  (not (or (n/whitespace-or-comment? node) (= :uneval (n/tag node)))))

(defn code-children [node] (filter code-node? (n/children node)))

(defn- head-str [node]
  (when (= :list (n/tag node))
    (let [h (first (code-children node))]
      (when (and h (= :token (n/tag h))) (n/string h)))))

(defn- norm-tokens
  "Whitespace- and comment-insensitive token sequence for hashing."
  [node]
  (let [out (java.util.ArrayList.)]
    (letfn [(walk [nd]
              (if (n/inner? nd)
                (let [cs (code-children nd)]
                  (.add out (str "(" (name (n/tag nd)) (count cs)))
                  (run! walk cs))
                (.add out (n/string nd))))]
      (walk node))
    (vec out)))

(defn- one-line [s max-len]
  (let [s (str/trim (str/replace s #"\s+" " "))]
    (if (> (count s) max-len) (str (subs s 0 max-len) "…") s)))

(defn- within? [{:keys [row col end-row end-col]} r c]
  (and (or (> r row) (and (= r row) (>= c col)))
       (or (< r end-row) (and (= r end-row) (<= c end-col)))))

;; ---------------------------------------------------------------------------
;; kind detection

(def ^:private def-kind-names
  #{"def" "defn" "defn-" "defmacro" "defmulti" "defmethod" "defonce" "defprotocol"
    "defrecord" "deftype" "definterface" "defstruct" "deftest" "defspec"})

(defn- sym-name [s] (when s (name (symbol (str s)))))

(defn- def-kind [d]
  (let [by (sym-name (:defined-by->lint-as d (:defined-by d)))]
    (cond
      (= "defn" by) (if (:private d) :defn- :defn)
      (contains? def-kind-names by) (keyword by)
      by (keyword by)
      :else :def)))

(defn- reg-kind [kw] (keyword (sym-name (:reg kw))))

(defn- kw-fqn [{:keys [ns name]}]
  (if ns (str ":" ns "/" name) (str ":" name)))

;; ---------------------------------------------------------------------------
;; per-file assembly

(defn- group-defs
  "Merge clj/cljs duplicates of the same definition (same name and position)."
  [defs file-langs*]
  (->> defs
       (group-by (juxt :ns :name :row :col))
       vals
       (map (fn [ds]
              (let [d (first ds)
                    langs (set (keep :lang ds))]
                (assoc d :langs (if (seq langs) langs file-langs*)))))
       (sort-by (juxt :name-row :name-col))))

(defn- build-form
  [{:keys [node index ns-name file-langs* defs regs usages kw-usages]}]
  (let [{:keys [row col end-row end-col]} (meta node)
        rng {:row row :col col :end-row end-row :end-col end-col}
        in? (fn [x] (and (:row x) (:col x) (within? rng (:row x) (:col x))))
        head (head-str node)
        tag (n/tag node)
        my-defs (group-defs (filter in? defs) file-langs*)
        my-regs (filter in? regs)
        own (into #{} (map #(str (:ns %) "/" (:name %))) my-defs)
        my-usages (->> (filter in? usages)
                       (filter :to)
                       (remove #(contains? ignored-usage-nses (:to %)))
                       (remove #(contains? own (str (:to %) "/" (:name %))))
                       (map (fn [u] {:fqn (str (:to u) "/" (:name u))
                                     :ns (str (:to u)) :name (str (:name u))
                                     :lang (:lang u (first file-langs*))
                                     :drow (- (:name-row u (:row u)) row)
                                     :col (:name-col u (:col u))})))
        my-kw-usages (->> (filter in? kw-usages)
                          (map (fn [k] {:fqn (kw-fqn k) :ns (str (:ns k)) :name (:name k)
                                        :keyword? true
                                        :lang (:lang k (first file-langs*))
                                        :drow (- (:row k) row) :col (:col k)})))
        all-usages (->> (concat my-usages my-kw-usages)
                        (reduce (fn [acc u] (if (contains? (:seen acc) [(:fqn u) (:lang u)])
                                              acc
                                              (-> acc (update :seen conj [(:fqn u) (:lang u)])
                                                  (update :out conj u))))
                                {:seen #{} :out []})
                        :out)
        def-maps (concat
                  (for [d my-defs]
                    {:fqn (str (:ns d) "/" (:name d)) :ns (str (:ns d)) :name (str (:name d))
                     :kind (def-kind d)
                     :doc (:doc d)
                     :arglists (vec (:arglist-strs d))
                     :private? (boolean (:private d))
                     :langs (:langs d)
                     :defined-by (some-> (:defined-by d) str)
                     :drow (- (:row d) row) :col (:col d)
                     :end-drow (- (:end-row d) row) :end-col (:end-col d)})
                  (for [k my-regs]
                    {:fqn (kw-fqn k) :ns (some-> (:ns k) str) :name (kw-fqn k)
                     :keyword? true
                     :kind (reg-kind k)
                     :defined-by (str (:reg k))
                     :langs file-langs*
                     :arglists []
                     :drow (- (:row k) row) :col (:col k)}))
        primary (first def-maps)
        method? (and (not primary) (= "defmethod" (sym-name head)))
        kind (cond
               (= head "ns") :ns
               method? :defmethod
               (#{"comment" "clojure.core/comment"} head) :comment
               (= tag :reader-macro) (if primary :reader-conditional :other)
               primary (:kind primary)
               :else :other)
        children (when (= kind :comment)
                   (->> (rest (code-children node))
                        (map-indexed
                         (fn [i c]
                           (let [m (meta c)]
                             {:index i :drow (- (:row m) row) :col (:col m)
                              :end-drow (- (:end-row m) row) :end-col (:end-col m)
                              :summary (one-line (n/string c) 160)})))
                        vec))
        langs (if (seq my-defs) (into #{} (mapcat :langs) my-defs) file-langs*)
        fname (cond (= kind :ns) (some-> (second (code-children node)) n/string)
                    method? (str/join " " (map n/string (take 2 (rest (code-children node)))))
                    primary (:name primary)
                    :else head)
        hash (git/sha1-hex (pr-str [ns-name kind (norm-tokens node)
                                    (sort (map (juxt :fqn :lang) all-usages))]))]
    {:hash hash :kind kind :head head :name fname
     :summary (when-not (or primary method?) (one-line (n/string node) 200))
     :defs (vec def-maps) :usages (vec all-usages) :children children
     :index index :row row :col col :end-row end-row :end-col end-col :langs langs}))

(defn- analyze-one [{:keys [text ext]} {:keys [var-definitions var-usages namespace-definitions
                                               namespace-usages keywords]}]
  (let [file-langs* (file-langs ext)
        ns-def (first namespace-definitions)
        ns-name (some-> (:name ns-def) str)
        root (parser/parse-string-all text)
        tops (filter code-node? (n/children root))
        regs (filter :reg keywords)
        kw-usages (->> keywords
                       (remove :reg)
                       (filter #(or (:ns %) (:auto-resolved %)))
                       (remove #(:keys-destructuring %)))]
    {:ns ns-name
     :ns-doc (:doc ns-def)
     :requires (vec (distinct (map (comp str :to) namespace-usages)))
     :aliases (into {} (keep (fn [u] (when (:alias u) [(str (:alias u)) (str (:to u))]))) namespace-usages)
     :forms (vec (map-indexed (fn [i node]
                                (build-form {:node node :index i :ns-name ns-name
                                             :file-langs* file-langs*
                                             :defs var-definitions :regs regs
                                             :usages var-usages :kw-usages kw-usages}))
                              tops))}))

(defn- group-analysis [analysis idx-of]
  (reduce-kv (fn [acc k xs]
               (reduce (fn [acc x]
                         (if-let [i (idx-of (:filename x))]
                           (update-in acc [i k] (fnil conj []) x)
                           acc))
                       acc xs))
             {} analysis))

(defn analyze-batch
  "files: [{:path rel-path :text source}]. opts: {:config-dir abs-path-or-nil}.
   Returns a vector (same order) of {:ns :ns-doc :requires :aliases :forms} or {:error msg}."
  [files {:keys [config-dir]}]
  (if (empty? files)
    []
    (let [tmp (fs/create-temp-dir {:prefix "clj-browse-an"})]
      (try
        (let [paths (vec (map-indexed
                          (fn [i {:keys [path text]}]
                            (let [p (fs/path tmp (str i) path)]
                              (fs/create-dirs (fs/parent p))
                              (spit (str p) text)
                              (str p)))
                          files))
              root (str tmp)
              idx-of (fn [filename]
                       (let [rel (str (fs/relativize root filename))]
                         (some-> (re-find #"^(\d+)[/\\]" rel) second parse-long)))
              res (kondo/run! (cond-> {:lint paths
                                       :parallel true
                                       :cache false
                                       :repro true
                                       :config [{:output {:format :edn}
                                                 :analysis kondo-analysis-config}]}
                                (and config-dir (fs/exists? config-dir))
                                (assoc :config-dir (str config-dir))))
              by-file (group-analysis (:analysis res) idx-of)
              errors (->> (:findings res)
                          (filter #(and (= :error (:level %)) (= :syntax (:type %))))
                          (group-by #(idx-of (:filename %))))]
          (vec (map-indexed
                (fn [i f]
                  (let [ext (ext-of (:path f))]
                    (if-let [errs (get errors i)]
                      {:error (str/join "; " (map #(str (:row %) ":" (:col %) " " (:message %)) errs))}
                      (try (analyze-one (assoc f :ext ext) (get by-file i))
                           (catch Exception e {:error (str "analysis failed: " (ex-message e))})))))
                files)))
        (finally (fs/delete-tree tmp))))))

(defn analyze-text
  "Convenience for a single file."
  ([path text] (analyze-text path text {}))
  ([path text opts] (first (analyze-batch [{:path path :text text}] opts))))
