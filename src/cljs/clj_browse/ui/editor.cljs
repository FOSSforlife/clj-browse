(ns clj-browse.ui.editor
  "CodeMirror 6 + nextjournal clojure-mode (highlighting, paredit) as a Reagent component.
  Used for the inline form editor, the Run modal inputs and the REPL input."
  (:require ["@codemirror/autocomplete" :refer [autocompletion]]
            ["@codemirror/commands" :refer [history historyKeymap defaultKeymap]]
            ["@codemirror/language" :refer [HighlightStyle syntaxHighlighting]]
            ["@codemirror/lint" :refer [linter]]
            ["@codemirror/state" :refer [EditorState Prec]]
            ["@codemirror/view" :refer [EditorView keymap lineNumbers hoverTooltip placeholder]]
            ["@lezer/highlight" :refer [tags]]
            ["@nextjournal/clojure-mode" :refer [default_extensions complete_keymap]]
            [reagent.core :as r]))

(def highlight-style
  (.define HighlightStyle
           (clj->js [{:tag (.-keyword tags) :class "cb-hl-def"}
                     {:tag (.-atom tags) :class "cb-hl-keyword"}
                     {:tag (.-bool tags) :class "cb-hl-keyword"}
                     {:tag (.-null tags) :class "cb-hl-keyword"}
                     {:tag ((.-definition tags) (.-variableName tags)) :class "cb-hl-def"}
                     {:tag (.-string tags) :class "cb-hl-string"}
                     {:tag (.-regexp tags) :class "cb-hl-string"}
                     {:tag (.-emphasis tags) :class "cb-hl-string"}
                     {:tag (.-number tags) :class "cb-hl-number"}
                     {:tag (.-lineComment tags) :class "cb-hl-comment"}
                     {:tag (.-comment tags) :class "cb-hl-comment"}
                     {:tag (.-bracket tags) :class "cb-hl-bracket"}])))

(defn- pos->offset [^js doc row col]
  (let [row (max 1 (min row (.-lines doc)))
        line (.line doc row)]
    (min (.-to line) (+ (.-from line) (max 0 (dec (or col 1)))))))

(defn- lint-extension [lint-fn]
  (linter (fn [^js view]
            (-> (lint-fn (.. view -state -doc toString))
                (.then (fn [findings]
                         (let [doc (.. view -state -doc)]
                           (clj->js
                            (for [{:keys [row col end-row end-col level message]} findings
                                  :when row]
                              (let [from (pos->offset doc row col)
                                    to (max (inc from) (pos->offset doc (or end-row row) (or end-col (inc col))))]
                                {:from from :to (min to (.-length doc))
                                 :severity (case level :error "error" :warning "warning" "info")
                                 :message message}))))))
                (.catch (fn [_] #js []))))
          #js {:delay 400}))

(defn- completion-extension [complete-fn]
  (autocompletion
   #js {:override
        #js [(fn [^js ctx]
               (let [word (.matchBefore ctx #"[\w\-.*+!?<>=:/'&$%]+")]
                 (if (or (nil? word) (and (= (.-from word) (.-to word)) (not (.-explicit ctx))))
                   nil
                   (-> (complete-fn (.-text word))
                       (.then (fn [cands]
                                #js {:from (.-from word)
                                     :options (clj->js (for [{:keys [candidate type ns]} cands]
                                                         {:label candidate :type (some-> type name)
                                                          :detail ns}))}))
                       (.catch (fn [_] nil))))))]}))

(defn- hover-extension [info-fn]
  (hoverTooltip
   (fn [^js view pos _side]
     (let [doc (.. view -state -doc)
           line (.lineAt doc pos)
           text (.-text line)
           i (- pos (.-from line))
           sym-char? #(re-find #"[\w\-.*+!?<>=/'&$%]" (str %))
           start (loop [s i] (if (and (pos? s) (sym-char? (.charAt text (dec s)))) (recur (dec s)) s))
           end (loop [e i] (if (and (< e (count text)) (sym-char? (.charAt text e))) (recur (inc e)) e))
           sym (subs text start end)]
       (when (seq sym)
         (-> (info-fn sym)
             (.then (fn [info]
                      (when (and info (or (:doc info) (:arglists-str info)))
                        #js {:pos (+ (.-from line) start) :end (+ (.-from line) end) :above true
                             :create (fn [_]
                                       (let [dom (js/document.createElement "div")]
                                         (set! (.-className dom) "max-w-md p-2 text-xs font-mono whitespace-pre-wrap bg-panel border border-line rounded")
                                         (set! (.-textContent dom)
                                               (str (:ns info) "/" (:name info) "\n" (:arglists-str info) "\n\n" (:doc info)))
                                         #js {:dom dom}))})))
             (.catch (fn [_] nil))))))))

(defn code-editor
  "props:
    :value          initial text (external changes replace the doc)
    :on-change      (fn [text])
    :on-submit      Mod-Enter (fn [text])
    :on-save        Mod-s (fn [text])
    :on-cancel      Escape
    :line-numbers?  :lint-fn (fn [text] → Promise findings)  :complete-fn (fn [prefix] → Promise)
    :info-fn        (fn [sym] → Promise)  :placeholder  :autofocus?  :class  :min-height"
  [_props]
  (let [view (atom nil)
        props-ref (atom nil)
        el (atom nil)]
    (r/create-class
     {:display-name "code-editor"
      :component-did-mount
      (fn [this]
        (let [{:keys [value line-numbers? lint-fn complete-fn info-fn autofocus?] :as props} (r/props this)
              _ (reset! props-ref props)
              call (fn [k] (fn [^js v]
                             (when-let [f (get @props-ref k)]
                               (f (.. v -state -doc toString)))
                             (some? (get @props-ref k))))
              keys (.of keymap (clj->js (cond-> []
                                          true (conj {:key "Mod-Enter" :run (call :on-submit)})
                                          true (conj {:key "Mod-s" :run (call :on-save) :preventDefault true})
                                          true (conj {:key "Escape" :run (call :on-cancel)}))))
              exts (cond-> [(.highest Prec keys)
                            (history)
                            (syntaxHighlighting highlight-style)
                            (.of keymap (.concat complete_keymap historyKeymap defaultKeymap))
                            (.. EditorView -lineWrapping)
                            (.of (.-updateListener EditorView)
                                 (fn [^js u]
                                   (when (.-docChanged u)
                                     (when-let [f (:on-change @props-ref)]
                                       (f (.. u -state -doc toString))))))]
                     true (into (js->clj default_extensions))
                     line-numbers? (conj (lineNumbers))
                     (:placeholder props) (conj (placeholder (:placeholder props)))
                     lint-fn (conj (lint-extension lint-fn))
                     complete-fn (conj (completion-extension complete-fn))
                     info-fn (conj (hover-extension info-fn)))
              v (EditorView. #js {:state (.create EditorState #js {:doc (or value "") :extensions (clj->js exts)})
                                  :parent @el})]
          (reset! view v)
          (when autofocus?
            (.focus v)
            (.dispatch v #js {:selection #js {:anchor (.. v -state -doc -length)}}))))
      :component-did-update
      (fn [this _]
        (let [props (r/props this)
              ^js v @view]
          (reset! props-ref props)
          (when (and v (contains? props :value)
                     (not= (:value props) (.. v -state -doc toString))
                     (:controlled? props))
            (.dispatch v #js {:changes #js {:from 0 :to (.. v -state -doc -length) :insert (or (:value props) "")}}))))
      :component-will-unmount
      (fn [_] (some-> ^js @view .destroy) (reset! view nil))
      :reagent-render
      (fn [{:keys [class min-height]}]
        [:div {:class class
               :style (when min-height {:min-height min-height})
               :ref #(reset! el %)}])})))
