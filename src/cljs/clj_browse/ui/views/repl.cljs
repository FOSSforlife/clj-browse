(ns clj-browse.ui.views.repl
  "Per-repo nREPL panel: connect/start, output log, input, clj/cljs toggle."
  (:require [clj-browse.ui.editor :as editor]
            [clj-browse.ui.events :as events]
            [clj-browse.ui.util :as u]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(def max-log 2000)

(defn- append-log [db repo entry]
  (update-in db [:repl repo :log]
             (fn [log] (let [log (conj (or log []) (assoc entry :id (random-uuid)))]
                         (if (> (count log) max-log) (subvec log (- (count log) max-log)) log)))))

;; ---------------------------------------------------------------------------
;; events

(rf/reg-event-fx
 :repl/refresh-status
 (fn [_ [_ repo]]
   {:ws {:op :repl/status :params {:repo repo} :on-ok [:repl/status-loaded repo]}}))

(rf/reg-event-db :repl/status-loaded (fn [db [_ repo st]] (update-in db [:repl repo] merge {:status st})))

(defmethod events/server-event :repl/status [{:keys [db]} [_ _ {:keys [repo] :as st}]]
  {:db (assoc-in db [:repl repo :status] st)})

(defmethod events/server-event :repl/output [{:keys [db]} [_ _ {:keys [repo stream text]}]]
  {:db (append-log db repo {:kind stream :text text})})

(rf/reg-event-fx
 :repl/connect
 (fn [{:keys [db]} [_ repo port]]
   {:db (assoc-in db [:repl repo :status :status] :connecting)
    :ws {:op :repl/connect :params {:repo repo :port port}
         :on-ok [:repl/status-loaded repo] :on-err [:repl/failed repo]}}))

(rf/reg-event-fx
 :repl/start
 (fn [{:keys [db]} [_ repo project-key aliases]]
   {:db (-> db (assoc-in [:repl repo :status :status] :starting) (assoc :panel :repl))
    :ws {:op :repl/start :params {:repo repo :project-key project-key :aliases aliases}
         :on-ok [:repl/status-loaded repo] :on-err [:repl/failed repo]}}))

(rf/reg-event-fx
 :repl/failed
 (fn [{:keys [db]} [_ repo err]]
   {:db (-> db (assoc-in [:repl repo :status :status] :error) (append-log repo {:kind :err :text (str err "\n")}))
    :dispatch [:toast/show {:kind :error :text (str "REPL: " err)}]}))

(rf/reg-event-fx
 :repl/disconnect
 (fn [_ [_ repo]] {:ws {:op :repl/disconnect :params {:repo repo} :on-ok [:repl/refresh-status repo]}}))

(rf/reg-event-db :repl/set-lang (fn [db [_ repo lang]] (assoc-in db [:repl repo :lang] lang)))
(rf/reg-event-db :repl/clear (fn [db [_ repo]] (assoc-in db [:repl repo :log] [])))

(defn- connected? [db repo] (= :connected (get-in db [:repl repo :status :status])))

(defn- lang-for [db repo path]
  (cond (str/ends-with? path ".cljs") :cljs
        (str/ends-with? path ".cljc") (get-in db [:repl repo :lang] :clj)
        :else :clj))

(rf/reg-event-fx
 :repl/eval
 (fn [{:keys [db]} [_ {:keys [repo code ns lang path line echo]}]]
   (if-not (connected? db repo)
     {:db (assoc db :panel :repl)
      :dispatch [:toast/show {:kind :error :text "No REPL connected for this repo — connect or start one in the REPL panel."}]}
     {:db (-> db (assoc :panel :repl) (append-log repo {:kind :input :text (or echo code) :ns ns}))
      :ws {:op :repl/eval :params {:repo repo :code code :ns ns :lang (or lang :clj) :path path :line line}
           :on-ok [:repl/result repo] :on-err [:repl/failed repo]}})))

(rf/reg-event-db
 :repl/result
 (fn [db [_ repo {:keys [value ex]}]]
   (cond-> db
     ex (append-log repo {:kind :ex :text (str (:class ex) ": " (:message ex)
                                              (when (and (:data-str ex) (not (#{"nil" "{}"} (:data-str ex)))) (str "\n" (:data-str ex))))})
     (and (not ex) value) (append-log repo {:kind :value :text value}))))

(rf/reg-event-fx
 :repl/eval-form
 (fn [{:keys [db]} [_ {:keys [repo path form child file-data]}]]
   (let [pos (if child
               {:row (+ (:row form) (:drow child)) :col (:col child)
                :end-row (+ (:row form) (:end-drow child)) :end-col (:end-col child)}
               (select-keys form [:row :col :end-row :end-col]))]
     {:ws {:op :form/source :params {:repo repo :path path :pos pos}
           :on-ok [:repl/eval-source {:repo repo :path path :ns (:ns file-data) :line (:row pos)
                                      :lang (lang-for db repo path)}]}})))

(rf/reg-event-fx
 :repl/eval-source
 (fn [_ [_ opts {:keys [text]}]]
   {:dispatch [:repl/eval (assoc opts :code text)]}))

(rf/reg-event-fx
 :repl/load-file
 (fn [{:keys [db]} [_ {:keys [repo path]}]]
   (if-not (connected? db repo)
     {:db (assoc db :panel :repl)
      :dispatch [:toast/show {:kind :error :text "No REPL connected for this repo."}]}
     {:db (-> db (assoc :panel :repl) (append-log repo {:kind :input :text (str ";; load-file " path)}))
      :ws {:op :repl/load-file :params {:repo repo :path path :lang (lang-for db repo path)}
           :on-ok [:repl/result repo] :on-err [:repl/failed repo]}})))

(rf/reg-event-fx
 :repl/select-build
 (fn [_ [_ repo build]]
   {:ws {:op :repl/select-build :params {:repo repo :build build} :on-ok [:repl/status-loaded repo]
         :on-err [:repl/failed repo]}}))

;; ---------------------------------------------------------------------------
;; view

(defn- start-form [repo]
  (let [outline @(rf/subscribe [:outline repo])
        repo-ent @(rf/subscribe [:repo repo])
        saved (:repo/repl-config repo-ent)
        projects (:projects outline)
        state (r/atom {:project (or (:project-key saved) (:key (first projects)))
                       :aliases (set (:aliases saved))})]
    (fn [repo]
      (let [projects (:projects @(rf/subscribe [:outline repo]))
            p (some #(when (= (:key %) (:project @state)) %) projects)]
        [:div {:class "space-y-2 text-[13px]"}
         [:label.block.text-xs.text-muted "Project"]
         (into [:select {:class "w-full rounded border border-line bg-bg px-2 py-1"
                         :value (or (:project @state) "")
                         :on-change #(swap! state assoc :project (.. % -target -value) :aliases #{})}]
               (for [p projects] [:option {:value (:key p)} (str (:name p) " — " (:file p))]))
         (when (seq (:aliases p))
           [:div
            [:div.text-xs.text-muted.mb-1 (if (= :lein (:type p)) "Profiles" "Aliases")]
            (into [:div.flex.flex-wrap.gap-1]
                  (for [a (:aliases p)]
                    [:label {:class (str "cursor-pointer rounded border px-1.5 py-0.5 font-mono text-xs "
                                         (if (contains? (:aliases @state) a) "border-accent bg-sel" "border-line"))}
                     [:input.hidden {:type "checkbox" :checked (contains? (:aliases @state) a)
                                     :on-change #(swap! state update :aliases (fn [s] (if (s a) (disj s a) (conj s a))))}]
                     (str ":" a)]))])
         [:button {:class "rounded bg-accent px-3 py-1 text-white text-[13px] disabled:opacity-50"
                   :disabled (nil? p)
                   :on-click #(rf/dispatch [:repl/start repo (:project @state) (vec (:aliases @state))])}
          "Start REPL"]]))))

(defn- log-entry [{:keys [kind text ns]}]
  (case kind
    :input [:div {:class "font-mono text-[12px] text-muted border-t border-line/60 pt-1 mt-1 whitespace-pre-wrap"}
            [:span.text-accent (str (or ns "user") "=> ")] text]
    :value [:pre {:class "font-mono text-[12px] whitespace-pre-wrap"} text]
    :ex [:pre {:class "font-mono text-[12px] whitespace-pre-wrap text-red-600 dark:text-red-400"} text]
    :err (into [:pre {:class "font-mono text-[12px] whitespace-pre-wrap text-orange-600 dark:text-orange-300"}] (u/ansi->spans text))
    (into [:pre {:class "font-mono text-[12px] whitespace-pre-wrap text-fg/80"}] (u/ansi->spans text))))

(defn- log-view [repo]
  (let [el (atom nil)
        stick? (atom true)]
    (r/create-class
     {:component-did-update (fn [_] (when (and @stick? @el) (set! (.-scrollTop @el) (.-scrollHeight @el))))
      :reagent-render
      (fn [repo]
        (let [log (:log @(rf/subscribe [:repl repo]))]
          [:div {:class "flex-1 overflow-auto px-3 py-2"
                 :ref #(reset! el %)
                 :on-scroll (fn [e] (let [t (.-target e)]
                                      (reset! stick? (< (- (.-scrollHeight t) (.-scrollTop t) (.-clientHeight t)) 30))))}
           (if (empty? log)
             [:div.text-faint.text-xs "Output from evaluations and the REPL process appears here."]
             (for [e log] ^{:key (:id e)} [log-entry e]))]))})))

(defn repl-panel [repo]
  (let [input (r/atom "")
        ns-input (r/atom nil)]
    (fn [repo]
      (let [{:keys [status lang]} @(rf/subscribe [:repl repo])
            st (:status status)
            connected? (= :connected st)
            shadow? (some #(= :shadow (:type %)) (:projects @(rf/subscribe [:outline repo])))]
        [:div {:class "flex h-full flex-col"}
         [:div {:class "flex items-center gap-2 border-b border-line px-3 py-2 text-[13px]"}
          [:span {:class (str "h-2 w-2 rounded-full " (case st :connected "bg-green-500" (:starting :connecting) "bg-amber-400 animate-pulse" :error "bg-red-500" "bg-gray-400"))}]
          [:span.font-medium (case st :connected (str "Connected · port " (:port status) (when-not (:owned? status) " (external)"))
                                 :starting "Starting…" :connecting "Connecting…" :error "Error" "Not connected")]
          [:span.flex-1]
          [:div {:class "flex rounded border border-line text-xs overflow-hidden"}
           (for [l [:clj :cljs]]
             ^{:key l}
             [:button {:class (str "px-2 py-0.5 " (if (= l (or lang :clj)) "bg-sel" "hover:bg-hover"))
                       :title "Language used for .cljc forms"
                       :on-click #(rf/dispatch [:repl/set-lang repo l])} (name l)])]
          [:button {:class "text-xs text-muted hover:text-fg" :on-click #(rf/dispatch [:repl/clear repo])} "Clear"]
          (when connected?
            [:button {:class "text-xs text-muted hover:text-red-500" :on-click #(rf/dispatch [:repl/disconnect repo])}
             (if (:owned? status) "Stop" "Disconnect")])]
         (when (and connected? shadow?)
           [:div {:class "flex items-center gap-2 border-b border-line px-3 py-1.5 text-xs"}
            [:span.text-muted "cljs build:"]
            (into [:select {:class "rounded border border-line bg-bg px-1"
                            :value (or (first (:cljs-builds status)) "")
                            :on-change #(rf/dispatch [:repl/select-build repo (.. % -target -value)])}
                   [:option {:value ""} "—"]]
                  (for [b @(rf/subscribe [::builds repo])] [:option {:value b} b]))
            (when (:cljs-session? status) [:span.text-green-600 "cljs session active"])])
         (when-not (#{:connected :starting :connecting} st)
           [:div {:class "space-y-3 border-b border-line px-3 py-3"}
            (when-let [port (:detected-port status)]
              [:div {:class "flex items-center gap-2 text-[13px]"}
               [:span (str "Found a running nREPL (.nrepl-port " port ")")]
               [:button {:class "rounded bg-accent px-2 py-0.5 text-white" :on-click #(rf/dispatch [:repl/connect repo port])} "Connect"]])
            [start-form repo]])
         [log-view repo]
         [:div {:class "border-t border-line p-2"}
          [editor/code-editor {:value @input :controlled? true
                               :class "cb-input"
                               :placeholder "Evaluate… (Ctrl+Enter)"
                               :on-change #(reset! input %)
                               :on-submit (fn [code]
                                            (when-not (str/blank? code)
                                              (rf/dispatch [:repl/eval {:repo repo :code code :ns @ns-input
                                                                        :lang (or lang :clj)}])
                                              (reset! input "")))}]
          [:div {:class "mt-1 flex items-center gap-2 text-[11px] text-faint"}
           [:span "ns:"]
           [:input {:class "rounded border border-line bg-bg px-1 font-mono w-48"
                    :placeholder "user" :value (or @ns-input "")
                    :on-change #(reset! ns-input (not-empty (.. % -target -value)))}]]]]))))

(rf/reg-sub ::builds (fn [db [_ repo]] (get-in db [:repl-builds repo] [])))
(rf/reg-event-fx :repl/load-builds (fn [_ [_ repo]] {:ws {:op :repl/builds :params {:repo repo} :on-ok [::builds-loaded repo]}}))
(rf/reg-event-db ::builds-loaded (fn [db [_ repo bs]] (assoc-in db [:repl-builds repo] bs)))
