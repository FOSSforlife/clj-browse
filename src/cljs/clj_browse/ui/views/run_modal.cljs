(ns clj-browse.ui.views.run-modal
  "The Run modal: one labeled, syntax-highlighted input per parameter,
  arity tabs, variadic lists, evaluated in the function's namespace."
  (:require [clj-browse.arglists :as arglists]
            [clj-browse.ui.editor :as editor]
            [clj-browse.ui.registry :as registry]
            [clj-browse.ui.util :as u]
            [clj-browse.ui.ws :as ws]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn mode-for [kind]
  (case kind
    (:defn :defn- :defmulti) :call
    (:def :defonce) :eval-var
    :defmacro :macroexpand
    :call))

(defn- initial-inputs [arity]
  {:fixed (mapv :template (:params arity))
   :rest []})

(rf/reg-event-fx
 :run/open
 (fn [{:keys [db]} [_ {:keys [repo path def file-data]}]]
   (let [arities (arglists/arities (:arglists def))
         lang (cond (str/ends-with? path ".cljs") :cljs
                    (str/ends-with? path ".cljc") (get-in db [:repl repo :lang] :clj)
                    :else :clj)]
     {:db (assoc db :modal {:type :run :repo repo :path path :def def :ns (:ns file-data)
                            :lang lang
                            :mode (mode-for (:kind def))
                            :arities arities
                            ;; default to the widest arity: usually the "real" one
                            :arity (if (seq arities)
                                     (apply max-key #(+ (count (:params (nth arities %)))
                                                        (if (:variadic (nth arities %)) 0.5 0))
                                            (range (count arities)))
                                     0)
                            :inputs (mapv initial-inputs arities)
                            :result nil :running? false})
      :ws {:op :run/history :params {:repo repo :fqn (:fqn def)} :on-ok [::history-loaded (:fqn def)]}})))

(rf/reg-event-db ::history-loaded
                 (fn [db [_ fqn h]] (if (= fqn (get-in db [:modal :def :fqn])) (assoc-in db [:modal :history] h) db)))

(rf/reg-event-db ::set-arity (fn [db [_ i]] (assoc-in db [:modal :arity] i)))
(rf/reg-event-db ::set-input (fn [db [_ path v]] (assoc-in db (into [:modal :inputs] path) v)))
(rf/reg-event-db ::add-rest (fn [db [_ a]] (update-in db [:modal :inputs a :rest] conj "")))
(rf/reg-event-db ::remove-rest (fn [db [_ a i]] (update-in db [:modal :inputs a :rest]
                                                          #(vec (concat (subvec % 0 i) (subvec % (inc i)))))))
(rf/reg-event-db ::set-lang (fn [db [_ l]] (assoc-in db [:modal :lang] l)))

(rf/reg-event-db
 ::apply-history
 (fn [db [_ {:run/keys [arity args]}]]
   (let [a (or arity 0)
         n (count (get-in db [:modal :arities a :params]))
         args (vec args)]
     (-> db
         (assoc-in [:modal :arity] a)
         (assoc-in [:modal :inputs a] {:fixed (subvec args 0 (min n (count args)))
                                       :rest (if (> (count args) n) (subvec args n) [])})
         (update-in [:modal :rev] (fnil inc 0))))))

(rf/reg-event-fx
 ::run
 (fn [{:keys [db]} _]
   (let [{:keys [repo def ns lang mode arity inputs]} (:modal db)
         {:keys [fixed rest]} (get inputs arity)
         args (vec (concat (map #(if (str/blank? %) "nil" %) fixed) (remove str/blank? rest)))]
     (if-not (= :connected (get-in db [:repl repo :status :status]))
       {:db (assoc-in db [:modal :result] {:ex {:message "No REPL connected. Start or connect one in the REPL panel."}})}
       {:db (assoc-in db [:modal :running?] true)
        :ws {:op :run/invoke
             :params {:repo repo :fqn (:fqn def) :ns ns :args args :arity arity :mode mode :lang lang}
             :on-ok [::ran (:fqn def)] :on-err [::failed (:fqn def)]}}))))

(rf/reg-event-fx
 ::ran
 (fn [{:keys [db]} [_ fqn res]]
   (when (= fqn (get-in db [:modal :def :fqn]))
     {:db (update db :modal assoc :running? false :result res)
      :ws {:op :run/history :params {:repo (get-in db [:modal :repo]) :fqn fqn} :on-ok [::history-loaded fqn]}})))

(rf/reg-event-db
 ::failed
 (fn [db [_ fqn err]]
   (if (= fqn (get-in db [:modal :def :fqn]))
     (update db :modal assoc :running? false :result {:ex {:message (str err)}})
     db)))

;; ---------------------------------------------------------------------------
;; result viewers (extension point; the default pretty EDN viewer is built in)

(defn edn-viewer [value]
  [:div {:class "rounded-md border border-line overflow-hidden max-h-[40vh] overflow-y-auto"}
   ^{:key value} [editor/code-editor {:value value}]])

(registry/register! :result-viewer {:id :edn :label "EDN" :order 0 :applies? (constantly true) :render edn-viewer})

(defn- result-view [_result]
  (let [viewer (r/atom nil)]
    (fn [{:keys [value out err ex code elapsed-ms]}]
      (let [viewers (registry/applicable :result-viewer value)
            v (or (some #(when (= @viewer (:id %)) %) viewers) (first viewers))]
        [:div {:class "space-y-2"}
         [:div {:class "flex items-center gap-2 text-xs text-muted"}
          [:span.font-medium.text-fg (if ex "Error" "Result")]
          (when elapsed-ms [:span (str elapsed-ms " ms")])
          (when code [:span {:class "font-mono truncate" :title code} code])
          [:span.flex-1]
          (when (and value (> (count viewers) 1))
            (into [:div.flex.gap-1]
                  (for [{:keys [id label]} viewers]
                    [:button {:class (str "rounded px-1.5 " (if (= id (:id v)) "bg-sel text-fg" "hover:bg-hover"))
                              :on-click #(reset! viewer id)} label])))]
         (when ex
           [:pre {:class "rounded-md bg-red-500/10 p-2 font-mono text-[12px] whitespace-pre-wrap text-red-700 dark:text-red-300"}
            (str (when (:class ex) (str (:class ex) ": ")) (:message ex)
                 (when (and (:data-str ex) (not= "nil" (:data-str ex)) (not= "{}" (:data-str ex)))
                   (str "\n" (:data-str ex))))])
         (when (and value (not ex)) [(:render v) value])
         (when (seq out)
           [:div [:div.text-xs.text-muted.mb-1 "stdout"]
            (into [:pre {:class "rounded bg-panel p-2 font-mono text-[12px] whitespace-pre-wrap max-h-40 overflow-auto"}] (u/ansi->spans out))])
         (when (seq err)
           [:div [:div.text-xs.text-muted.mb-1 "stderr"]
            (into [:pre {:class "rounded bg-panel p-2 font-mono text-[12px] whitespace-pre-wrap max-h-40 overflow-auto text-orange-600"}] (u/ansi->spans err))])]))))

(defn- param-input [{:keys [label value on-change on-submit autofocus? ns repo hint]}]
  [:div {:class "grid grid-cols-[minmax(6rem,auto)_1fr] items-start gap-3"}
   [:label {:class "pt-1.5 text-right font-mono text-[12.5px] text-muted"} label
    (when hint [:div {:class "text-[10.5px] text-faint font-sans"} hint])]
   [editor/code-editor {:value value :class "cb-input" :autofocus? autofocus?
                        :on-change on-change :on-submit on-submit
                        :on-cancel #(rf/dispatch [:modal/close])
                        :complete-fn (fn [p] (ws/request :repl/complete {:repo repo :prefix p :ns ns}))}]])

(defn run-modal []
  (let [{:keys [repo def ns arities arity inputs result running? mode history lang path rev]} @(rf/subscribe [:modal])
        a (get arities arity)
        {:keys [fixed rest]} (get inputs arity)
        submit #(rf/dispatch [::run])
        repl @(rf/subscribe [:repl repo])
        connected? (= :connected (get-in repl [:status :status]))]
    [:div {:class "fixed inset-0 z-50 flex items-start justify-center bg-black/30 pt-[8vh]"
           :on-click #(rf/dispatch [:modal/close])}
     [:div {:class "w-[min(760px,94vw)] max-h-[84vh] overflow-auto rounded-xl border border-line bg-bg shadow-2xl"
            :on-click #(.stopPropagation %)
            :on-key-down (fn [e] (when (= "Escape" (.-key e)) (rf/dispatch [:modal/close])))}
      ;; header
      [:div {:class "border-b border-line px-5 py-3"}
       [:div {:class "flex items-center gap-2"}
        [:span {:class "rounded bg-accent/10 px-1.5 text-[11px] font-mono text-accent"}
         (case mode :call "run" :eval-var "inspect" :macroexpand "macroexpand")]
        [:span {:class "font-mono font-semibold text-[15px]"} (:name def)]
        [:span {:class "font-mono text-xs text-faint truncate"} ns]
        [:span.flex-1]
        (when (str/ends-with? path ".cljc")
          [:div {:class "flex rounded border border-line text-xs overflow-hidden"}
           (for [l [:clj :cljs]]
             ^{:key l} [:button {:class (str "px-2 py-0.5 " (if (= l lang) "bg-sel" "hover:bg-hover"))
                                 :on-click #(rf/dispatch [::set-lang l])} (name l)])])
        [:button {:class "text-muted hover:text-fg px-1" :on-click #(rf/dispatch [:modal/close])} "✕"]]
       (when-let [doc (:doc def)]
         [u/html-div {:class "cb-md mt-1.5 text-[13px] text-fg/80 max-h-24 overflow-auto"}
          (u/markdown-html doc)])]
      ;; body
      [:div {:class "space-y-3 px-5 py-4"}
       (when-not connected?
         [:div {:class "flex items-center gap-2 rounded-md bg-amber-500/10 px-3 py-2 text-[13px]"}
          [:span "No REPL is connected for this repo."]
          [:button {:class "rounded border border-line px-2 py-0.5 hover:bg-hover"
                    :on-click #(do (rf/dispatch [:modal/close]) (rf/dispatch [:panel/open :repl]))}
           "Open REPL panel"]])
       (when (and (not= mode :eval-var) (> (count arities) 1))
         (into [:div {:class "flex flex-wrap gap-1"}]
               (map-indexed (fn [i ar]
                              [:button {:class (str "rounded-md border px-2 py-0.5 font-mono text-[12px] "
                                                    (if (= i arity) "border-accent bg-sel" "border-line hover:bg-hover"))
                                        :on-click #(rf/dispatch [::set-arity i])}
                               (:raw ar)])
                            arities)))
       (when (not= mode :eval-var)
         (if (and (empty? (:params a)) (not (:variadic a)))
           [:div {:class "text-[13px] text-muted"} "No parameters."]
           ^{:key (str arity "-" rev)}
           [:div {:class "space-y-2"}
            (doall
             (map-indexed
              (fn [i p]
                ^{:key i}
                [param-input {:label (:label p) :value (get fixed i) :autofocus? (zero? i) :repo repo :ns ns
                              :hint (case (:destructured p) :map "map" :vector "vector" nil)
                              :on-change #(rf/dispatch [::set-input [arity :fixed i] %])
                              :on-submit submit}])
              (:params a)))
            (when-let [v (:variadic a)]
              [:div {:class "space-y-2"}
               (doall
                (map-indexed
                 (fn [i val]
                   ^{:key (str "rest" i)}
                   [:div {:class "flex items-start gap-1"}
                    [:div.flex-1
                     [param-input {:label (str "& " (:label v) " [" i "]") :value val :repo repo :ns ns
                                   :on-change #(rf/dispatch [::set-input [arity :rest i] %])
                                   :on-submit submit}]]
                    [:button {:class "mt-1 px-1 text-muted hover:text-red-500" :title "Remove"
                              :on-click #(rf/dispatch [::remove-rest arity i])} "✕"]])
                 rest))
               [:div {:class "grid grid-cols-[minmax(6rem,auto)_1fr] gap-3"}
                [:span]
                [:button {:class "justify-self-start rounded border border-dashed border-line px-2 py-0.5 text-xs text-muted hover:text-fg"
                          :on-click #(rf/dispatch [::add-rest arity])}
                 (str "+ add " (:label v))]]])]))
       [:div {:class "text-[11.5px] text-faint"}
        "Inputs are Clojure expressions evaluated in " [:span.font-mono ns] " — aliases and locals of that namespace work. Ctrl+Enter runs."]
       (when result [result-view result])]
      ;; footer
      [:div {:class "flex items-center gap-2 border-t border-line bg-panel/60 px-5 py-2.5"}
       (when (seq history)
         [:div {:class "flex items-center gap-1 min-w-0 overflow-x-auto"}
          [:span {:class "text-xs text-muted shrink-0"} "Recent:"]
          (for [[i h] (map-indexed vector (take 6 history))]
            ^{:key i}
            [:button {:class (str "shrink-0 max-w-[12rem] truncate rounded border border-line px-1.5 font-mono text-[11px] hover:bg-hover "
                                  (when (:run/error? h) "text-red-500"))
                      :title (str/join " " (:run/args h))
                      :on-click #(rf/dispatch [::apply-history h])}
             (let [s (str/join " " (:run/args h))] (if (str/blank? s) "()" s))])])
       [:span.flex-1]
       [:button {:class "rounded-md px-3 py-1 text-[13px] hover:bg-hover" :on-click #(rf/dispatch [:modal/close])} "Cancel"]
       [:button {:class "rounded-md bg-accent px-4 py-1 text-[13px] font-medium text-white disabled:opacity-60"
                 :disabled running?
                 :on-click submit}
        (if running? "Running…" (case mode :call "Run" :eval-var "Inspect" :macroexpand "Expand"))]]]]))
