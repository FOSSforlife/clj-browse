(ns clj-browse.edit
  "Pure source-string editing (docstrings, forms, requires) built on rewrite-clj.
  Every function preserves all untouched text byte-for-byte. Only the two
  `*-file!`/blob helpers touch the disk."
  (:refer-clojure :exclude [ns-aliases])
  (:require [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip :as z])
  (:import [java.nio.file Files OpenOption StandardOpenOption]
           [java.security MessageDigest]
           [rewrite_clj.node.stringz StringNode]))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; positions / locating forms

(defn- line-starts [^String source]
  (loop [acc [0] i 0]
    (let [j (.indexOf source "\n" (int i))]
      (if (neg? j) acc (recur (conj acc (inc j)) (inc j))))))

(defn- pos->offset [source row col]
  (+ (nth (line-starts source) (dec row)) (dec col)))

(defn- loc-pos [loc] (z/position loc))

(defn- loc-offset [source loc]
  (let [[r c] (loc-pos loc)] (pos->offset source r c)))

(defn- loc-end-offset [source loc]
  (+ (loc-offset source loc) (count (z/string loc))))

(defn- top-level-locs [source]
  (when-let [l (z/of-string source {:track-position? true})]
    (take-while some? (iterate z/right l))))

(defn find-top-level
  "Zipper loc of the top-level form starting at {:row :col}, or nil."
  [source {:keys [row col]}]
  (first (filter #(= [row col] (loc-pos %)) (top-level-locs source))))

(defn- require-loc [source pos]
  (or (find-top-level source pos)
      (throw (ex-info "No top-level form at position" {:type :not-found :pos pos}))))

(defn form-text
  "Exact source string of the top-level form at pos."
  [source pos]
  (z/string (require-loc source pos)))

(defn- spaces [k] (apply str (repeat k \space)))

;; ---------------------------------------------------------------------------
;; docstrings

(defn- ignorable? [nd]
  (or (n/whitespace-or-comment? nd) (= :uneval (n/tag nd))))

(defn- string-node? [nd] (instance? StringNode nd))

(defn- sym-name [nd]
  (when (= :token (n/tag nd))
    (let [v (try (n/sexpr nd) (catch Exception _ nil))]
      (when (symbol? v) (name v)))))

(def ^:private named-heads #{"defn" "defn-" "defmacro" "defmulti" "defprotocol" "ns"})
(def ^:private def-heads #{"def" "defonce"})

(defn- unsupported [msg data]
  (ex-info msg (assoc data :type :unsupported-form)))

(defn- parse-def-form
  "Parse form text; returns {:cs [child strings] :nodes [...] :sig [idx...] :kind}."
  [text]
  (let [node (p/parse-string text)]
    (when-not (= :list (n/tag node))
      (throw (unsupported "Form is not a list" {})))
    (let [nodes (vec (n/children node))
          sig (vec (keep-indexed (fn [i x] (when-not (ignorable? x) i)) nodes))
          head (some-> (first sig) nodes sym-name)
          kind (cond (named-heads head) :named
                     (def-heads head) :def
                     :else (throw (unsupported (str "Cannot hold a docstring: " head)
                                               {:head head})))]
      (when (< (count sig) 2)
        (throw (unsupported "Form has no name" {:head head})))
      (when (and (= kind :def) (< (count sig) 3))
        (throw (unsupported "def without value cannot hold a docstring" {:head head})))
      {:nodes nodes :sig sig :kind kind})))

(defn- existing-doc-idx [{:keys [nodes sig kind]}]
  (let [i (get sig 2)]
    (when (and i (string-node? (nodes i))
               (or (= kind :named) (>= (count sig) 4)))
      i)))

(defn get-docstring
  "Docstring of the definition at pos, or nil."
  [source pos]
  (let [info (parse-def-form (form-text source pos))]
    (when-let [i (existing-doc-idx info)]
      (n/sexpr (get-in info [:nodes i])))))

(defn- escape-doc [s]
  (-> s (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")))

(defn- normalize-doc
  "Strip common indentation of continuation lines, trim, drop trailing blanks per line."
  [doc]
  (let [lines (str/split (str/replace doc "\r\n" "\n") #"\n" -1)
        [l0 & more] lines
        indent (fn [l] (count (take-while #{\space \tab} l)))
        mi (->> more (remove str/blank?) (map indent) (reduce min Long/MAX_VALUE))
        more' (map #(if (str/blank? %) "" (subs % (min mi (indent %)))) more)]
    (str/trim (str/join "\n" (cons (str/triml l0) more')))))

(defn- render-doc
  "String literal for doc whose opening quote sits at 0-based column qc."
  [doc qc]
  (let [lines (str/split (escape-doc (normalize-doc doc)) #"\n" -1)
        pad (spaces (inc qc))]
    (str "\""
         (str/join "\n" (cons (first lines)
                              (map #(if (str/blank? %) "" (str pad %)) (rest lines))))
         "\"")))

(defn- end-col
  "0-based column after `(` followed by the given pieces, for a form at 0-based col0."
  [col0 pieces]
  (let [s (apply str "(" pieces)
        i (.lastIndexOf ^String s "\n")]
    (if (neg? i) (+ col0 (count s)) (- (count s) i 1))))

(defn- edit-doc-text [text col0 doc]
  (let [{:keys [nodes sig] :as info} (parse-def-form text)
        strs (mapv n/string nodes)
        blank? (str/blank? doc)
        di (existing-doc-idx info)
        ni (second sig)
        indent (str "\n" (spaces (+ col0 2)))
        wrap #(str "(" (apply str %) ")")]
    (cond
      (and di blank?)
      (let [j (loop [j di] (if (and (pos? j) (n/whitespace? (nodes (dec j)))) (recur (dec j)) j))]
        (wrap (concat (subvec strs 0 j) (subvec strs (inc di)))))

      di
      (let [qc (end-col col0 (subvec strs 0 di))]
        (wrap (assoc strs di (render-doc doc qc))))

      blank? text

      :else
      (let [s2 (get sig 2)
            run (if s2 (subvec nodes (inc ni) s2) [])
            same-line? (and (seq run) (every? #(and (n/whitespace? %) (not (n/linebreak? %))) run))
            before (subvec strs 0 (inc ni))
            mid (cond (nil? s2) [indent]
                      same-line? [indent]
                      :else (subvec strs (inc ni) s2))
            pre (into before mid)
            qc (end-col col0 pre)
            rest-strs (if s2 (into [(render-doc doc qc) indent] (subvec strs s2))
                          (into [(render-doc doc qc)] (subvec strs (inc ni))))]
        (wrap (into pre rest-strs))))))

(defn set-docstring
  "Return new source with the docstring of the definition at pos set to `doc`
  (nil or blank = remove). Throws ex-info {:type :unsupported-form} when the
  form cannot hold a docstring."
  [source pos doc]
  (let [loc (require-loc source pos)
        start (loc-offset source loc)
        text (z/string loc)
        new-text (edit-doc-text text (dec (:col pos)) doc)]
    (str (subs source 0 start) new-text (subs source (+ start (count text))))))

;; ---------------------------------------------------------------------------
;; whole-form edits

(defn replace-form
  "Replace the whole text of the top-level form at pos."
  [source pos new-form-text]
  (let [loc (require-loc source pos)
        start (loc-offset source loc)]
    (str (subs source 0 start) new-form-text (subs source (loc-end-offset source loc)))))

(defn- trailing-comment-len [^String source off]
  (if-let [m (re-find #"^[ \t]*;[^\n]*" (subs source off))] (count m) 0))

(defn insert-form
  "Insert form-text as a new top-level form after the form at `after`
  (or at end of file when nil), separated by exactly one blank line."
  [source after form-text]
  (let [ft (str/trim form-text)]
    (if (nil? after)
      (let [base (str/trimr source)]
        (if (str/blank? base)
          (str ft "\n")
          (str base "\n\n" ft "\n")))
      (let [loc (require-loc source after)
            e (loc-end-offset source loc)
            e (+ e (trailing-comment-len source e))
            before (subs source 0 e)
            rest-s (subs source e)
            rest' (if-let [m (re-find #"\A[ \t]*\n(?:[ \t]*\n)*" rest-s)]
                    (subs rest-s (count m))
                    rest-s)]
        (if (str/blank? rest')
          (str before "\n\n" ft "\n")
          (str before "\n\n" ft "\n\n" rest'))))))

;; ---------------------------------------------------------------------------
;; requires

(defn- head-name [loc]
  (when (= :list (z/tag loc))
    (when-let [d (z/down loc)]
      (try (let [v (z/sexpr d)] (when (symbol? v) (name v))) (catch Exception _ nil)))))

(defn- ns-loc [source]
  (first (filter #(= "ns" (head-name %)) (top-level-locs source))))

(defn- children-locs [loc]
  (take-while some? (iterate z/right (z/down loc))))

(defn- require-clauses [nloc]
  (filter #(and (= :list (z/tag %)) (some-> (z/down %) z/string (= ":require")))
          (children-locs nloc)))

(defn- entry-specs [e]
  (case (z/tag e)
    :token (if (symbol? (try (z/sexpr e) (catch Exception _ nil)))
             [{:ns (z/string e) :as nil}]
             [])
    :vector (let [strs (mapv z/string (children-locs e))
                  as (some (fn [[a b]] (when (= a ":as") b)) (partition 2 1 strs))]
              (if (seq strs) [{:ns (first strs) :as as}] []))
    :list (let [[pre & more] (children-locs e)]
            (when pre
              (mapcat (fn [s] (map #(update % :ns (fn [x] (str (z/string pre) "." x)))
                                   (entry-specs s)))
                      more)))
    []))

(defn- clause-entries [clause]
  (take-while some? (iterate z/right (some-> (z/down clause) z/right))))

(defn- all-specs [nloc]
  (mapcat #(mapcat entry-specs (clause-entries %)) (require-clauses nloc)))

(defn ns-aliases
  "{\"alias\" \"ns.name\"} from the ns form's :require clauses."
  [source]
  (if-let [nl (ns-loc source)]
    (into {} (keep (fn [{:keys [ns as]}] (when as [as ns]))) (all-specs nl))
    {}))

(defn- render-spec [{:keys [ns as refer]}]
  (str "[" ns
       (when (and as (not (str/blank? (str as)))) (str " :as " as))
       (when (seq refer) (str " :refer [" (str/join " " refer) "]"))
       "]"))

(defn add-requires
  "Add require specs [{:ns :as :refer}] to the ns form. Returns
  {:source :added [ns...] :conflicts [{:ns :wanted-as :existing-as}]}."
  [source specs]
  (let [nl (or (ns-loc source)
               (throw (ex-info "No ns form" {:type :no-ns})))
        existing0 (reduce (fn [m {:keys [ns as]}] (assoc m ns as)) {} (all-specs nl))
        {:keys [added texts conflicts]}
        (reduce
         (fn [{:keys [existing] :as acc} spec]
           (let [nsn (str (:ns spec))
                 want (some-> (:as spec) str)
                 spec (assoc spec :ns nsn :as want)]
             (cond
               (contains? existing nsn)
               (let [have (get existing nsn)]
                 (if (and want have (not= want have))
                   (update acc :conflicts conj {:ns nsn :wanted-as want :existing-as have})
                   acc))
               :else
               (-> acc
                   (update :added conj nsn)
                   (update :texts conj (render-spec spec))
                   (update :existing assoc nsn want)))))
         {:existing existing0 :added [] :texts [] :conflicts []}
         specs)
        clause (first (require-clauses nl))
        join-at (fn [col] (str/join (str "\n" (spaces col)) texts))
        insert-at (fn [off text] (str (subs source 0 off) text (subs source off)))
        skip (fn [off] (+ off (trailing-comment-len source off)))
        new-src
        (cond
          (empty? texts) source

          (and clause (seq (clause-entries clause)))
          (let [lastp (last (clause-entries clause))
                col (dec (second (loc-pos lastp)))]
            (insert-at (skip (loc-end-offset source lastp))
                       (str "\n" (spaces col) (join-at col))))

          clause
          (let [kw (z/down clause)
                col (+ (dec (second (loc-pos kw))) (count ":require") 1)]
            (insert-at (loc-end-offset source kw) (str " " (join-at col))))

          :else
          (let [lastc (last (children-locs nl))
                ind (+ (dec (second (loc-pos nl))) 2)]
            (insert-at (skip (loc-end-offset source lastc))
                       (str "\n" (spaces ind) "(:require " (join-at (+ ind 10)) ")"))))]
    {:source new-src :added added :conflicts conflicts}))

;; ---------------------------------------------------------------------------
;; checked writes

(defn blob-sha
  "Git blob sha-1 hex of the given bytes."
  [^bytes bs]
  (let [md (MessageDigest/getInstance "SHA-1")]
    (.update md (.getBytes (str "blob " (alength bs) "\u0000") "UTF-8"))
    (.update md bs)
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) (.digest md)))))

(defn write-file-checked!
  "Write new-content to path only if the file's current git blob sha equals
  expected-sha. Returns {:ok true :sha new-sha} or
  {:ok false :reason :conflict :sha current-sha}."
  [path expected-sha new-content]
  (let [p (.toPath (java.io.File. (str path)))
        current (blob-sha (Files/readAllBytes p))]
    (if (= current expected-sha)
      (let [bs (.getBytes (str new-content) "UTF-8")]
        (Files/write p bs ^"[Ljava.nio.file.OpenOption;" (into-array OpenOption
                                      [StandardOpenOption/TRUNCATE_EXISTING
                                       StandardOpenOption/WRITE]))
        {:ok true :sha (blob-sha bs)})
      {:ok false :reason :conflict :sha current})))
