(ns clj-browse.ui.util
  (:require ["marked" :refer [marked]]
            [clojure.string :as str]))

(defn escape-html [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn dedent
  "Clojure docstrings indent continuation lines; strip the common indent."
  [s]
  (let [[first-line & more] (str/split-lines (str s))
        indents (keep #(when-not (str/blank? %) (count (re-find #"^\s*" %))) more)
        n (if (seq indents) (apply min indents) 0)]
    (str/join "\n" (cons first-line (map #(if (>= (count %) n) (subs % n) (str/triml %)) more)))))

(defn markdown-html [s]
  (marked (escape-html (dedent s)) #js {:breaks false :gfm true}))

(defn basename [p] (last (str/split (str p) #"/")))

(defn short-repo [p] (basename p))

(def ansi-re #"\u001b\[([0-9;]*)m")

(defn ansi->spans
  "Very small ANSI SGR renderer → hiccup spans."
  [text]
  (loop [s text classes #{} out []]
    (if-let [m (re-find ansi-re s)]
      (let [idx (.indexOf s (first m))
            before (subs s 0 idx)
            codes (map js/parseInt (remove str/blank? (str/split (second m) #";")))
            next-classes (reduce (fn [cs c] (cond (or (js/isNaN c) (= c 0)) #{}
                                                  (= c 39) (set (remove #(re-find #"cb-ansi-(3|9)" %) cs))
                                                  :else (conj cs (str "cb-ansi-" c))))
                                 classes (if (empty? codes) [0] codes))]
        (recur (subs s (+ idx (count (first m)))) next-classes
               (cond-> out (seq before) (conj [:span {:class (str/join " " classes)} before]))))
      (cond-> out (seq s) (conj [:span {:class (str/join " " classes)} s])))))

(defn key-string
  "KeyboardEvent → \"Ctrl+Shift+k\" style string (Meta counts as Ctrl on mac)."
  [^js e]
  (let [k (.-key e)
        k (if (= 1 (count k)) (str/lower-case k) k)]
    (str (when (or (.-ctrlKey e) (.-metaKey e)) "Ctrl+")
         (when (.-altKey e) "Alt+")
         (when (and (.-shiftKey e) (not= 1 (count (.-key e)))) "Shift+")
         k)))

(defn editable-target? [^js e]
  (let [t (.-target e)
        tag (some-> t .-tagName str/lower-case)]
    (or (#{"input" "textarea" "select"} tag)
        (some-> t (.closest ".cm-editor"))
        (.-isContentEditable t))))

(defn html-div
  "A div whose content is trusted-ish HTML (escaped markdown output).
  Sets innerHTML via a ref; :dangerouslySetInnerHTML doesn't survive
  Reagent 2's prop conversion here."
  [attrs html]
  [:div (assoc attrs :ref (fn [el] (when (and el (not= (.-innerHTML el) html))
                                     (set! (.-innerHTML el) html))))])
