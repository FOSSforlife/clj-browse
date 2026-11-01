(ns clj-browse.ui.views.bb
  "Babashka tasks panel: list, run, stop, and view output of bb.edn tasks."
  (:require [clj-browse.ui.events :as events]
            [clj-browse.ui.util :as u]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(def max-output 500000)

(defn- append-output [db repo task text]
  (update-in db [:bb-view :output [repo task]]
             (fn [old]
               (let [s (str old text)]
                 (if (> (count s) max-output) (subs s (- (count s) max-output)) s)))))

;; ---------------------------------------------------------------------------
;; events

(rf/reg-event-fx
 ::load
 (fn [_ [_ repo]]
   {:ws {:op :bb/list :params {:repo repo} :on-ok [::loaded repo]}}))

(rf/reg-event-db ::loaded
                 (fn [db [_ repo res]]
                   (-> db
                       (assoc-in [:bb-view :tasks repo] (or res ::none))
                       (update-in [:bb-view :running] merge
                                  (into {} (map (fn [{:keys [id task]}] [id {:repo repo :task task}])) (:running res))))))

(rf/reg-sub ::tasks (fn [db [_ repo]] (get-in db [:bb-view :tasks repo])))
(rf/reg-sub ::running (fn [db _] (get-in db [:bb-view :running])))
(rf/reg-sub ::selected (fn [db [_ repo]] (get-in db [:bb-view :selected repo])))
(rf/reg-sub ::output (fn [db [_ repo task]] (get-in db [:bb-view :output [repo task]])))
(rf/reg-sub ::exit (fn [db [_ repo task]] (get-in db [:bb-view :exit [repo task]])))

(rf/reg-event-fx
 ::select
 (fn [{:keys [db]} [_ repo task]]
   {:db (assoc-in db [:bb-view :selected repo] task)
    :fx [(when-not (get-in db [:bb-view :output [repo task]])
           [:dispatch [::load-output repo task]])]}))

(rf/reg-event-fx
 ::load-output
 (fn [_ [_ repo task]]
   {:ws {:op :bb/output :params {:repo repo :task task} :on-ok [::output-loaded repo task]}}))

(rf/reg-event-db
 ::output-loaded
 (fn [db [_ repo task res]]
   (cond-> db
     (and res (nil? (get-in db [:bb-view :output [repo task]])))
     (assoc-in [:bb-view :output [repo task]] (or (:text res) ""))
     (and res (:exit res) (nil? (get-in db [:bb-view :exit [repo task]])))
     (assoc-in [:bb-view :exit [repo task]] (:exit res)))))

(rf/reg-event-fx
 ::run
 (fn [{:keys [db]} [_ repo task args]]
   {:db (-> db
            (assoc-in [:bb-view :output [repo task]] "")
            (update-in [:bb-view :exit] dissoc [repo task])
            (assoc-in [:bb-view :selected repo] task))
    :ws {:op :bb/run :params {:repo repo :task task :args (or args "")} :on-ok [::started repo task]}}))

(rf/reg-event-db
 ::started
 (fn [db [_ repo task id]]
   (assoc-in db [:bb-view :running id] {:repo repo :task task})))

(rf/reg-event-fx ::stop (fn [_ [_ id]] {:ws {:op :bb/stop :params {:id id}}}))

(defmethod events/server-event :bb/output [{:keys [db]} [_ _ {:keys [repo task id text]}]]
  {:db (-> db
           (assoc-in [:bb-view :running id] {:repo repo :task task})
           (append-output repo task text))})

(defmethod events/server-event :bb/exit [{:keys [db]} [_ _ {:keys [repo task id exit]}]]
  {:db (-> db
           (update-in [:bb-view :running] dissoc id)
           (assoc-in [:bb-view :exit [repo task]] exit))
   :dispatch [::load repo]})

;; ---------------------------------------------------------------------------
;; view

(defn- run-id-for [running repo task]
  (some (fn [[id v]] (when (and (= repo (:repo v)) (= task (:task v))) id)) running))

(defn- status [repo task last-info running-id]
  (let [live-exit @(rf/subscribe [::exit repo task])
        exit (if (some? live-exit) live-exit (:exit last-info))]
    (cond
      running-id [:span {:class "flex items-center gap-1 text-xs text-amber-500"}
                  [:span {:class "h-3 w-3 animate-spin rounded-full border-2 border-amber-400 border-t-transparent"}]
                  "running"]
      (some? exit) [:span {:class (str "font-mono text-xs " (if (zero? exit) "text-green-600 dark:text-green-400" "text-red-600 dark:text-red-400"))}
                    (str "exit " exit)]
      :else nil)))

(defn- task-row [_repo _task _running-id _selected?]
  (let [args (r/atom nil)]
    (fn [repo {:keys [name doc last]} running-id selected?]
      (let [run! #(rf/dispatch [::run repo name (or @args "")])]
        [:div {:class (str "border-b border-line/50 px-3 py-1.5 " (when selected? "bg-sel"))}
         [:div {:class "flex items-center gap-2"}
          [:button {:class "min-w-0 flex-1 text-left" :on-click #(rf/dispatch [::select repo name])}
           [:div {:class "truncate font-mono text-[13px]"} name]
           (when doc [:div {:class "truncate text-xs text-muted"} doc])]
          [status repo name last running-id]
          [:button {:class "text-xs text-faint hover:text-fg" :title "Task arguments"
                    :on-click #(swap! args (fn [a] (if (nil? a) "" nil)))} "args"]
          (if running-id
            [:button {:class "rounded border border-line px-2 py-0.5 text-xs text-red-500 hover:bg-hover"
                      :on-click #(rf/dispatch [::stop running-id])} "Stop"]
            [:button {:class "rounded bg-accent px-2 py-0.5 text-xs text-white" :on-click run!} "Run"])]
         (when (some? @args)
           [:input {:class "mt-1 w-full rounded border border-line bg-bg px-2 py-0.5 font-mono text-xs outline-none focus:border-accent"
                    :placeholder "arguments" :auto-focus true :value @args
                    :on-change #(reset! args (.. % -target -value))
                    :on-key-down #(when (and (= "Enter" (.-key %)) (not running-id)) (run!))}])]))))

(defn- output-view [_repo _task]
  (let [el (atom nil)
        stick? (atom true)
        scroll! #(when (and @stick? @el) (set! (.-scrollTop @el) (.-scrollHeight @el)))]
    (r/create-class
     {:component-did-mount scroll!
      :component-did-update scroll!
      :reagent-render
      (fn [repo task]
        (let [text @(rf/subscribe [::output repo task])]
          [:div {:class "min-h-0 flex-1 overflow-auto bg-bg px-3 py-2"
                 :ref #(reset! el %)
                 :on-scroll (fn [e] (let [t (.-target e)]
                                      (reset! stick? (< (- (.-scrollHeight t) (.-scrollTop t) (.-clientHeight t)) 30))))}
           (if (seq text)
             (into [:pre {:class "whitespace-pre-wrap font-mono text-[12px] text-fg/80"}] (u/ansi->spans text))
             [:div {:class "text-xs text-faint"} "No output yet."])]))})))

(defn bb-panel [_repo]
  (let [show-private? (r/atom false)
        last-repo (atom nil)]
    (fn [repo]
      (when (not= repo @last-repo)
        (reset! last-repo repo)
        (rf/dispatch [::load repo]))
      (let [res @(rf/subscribe [::tasks repo])
            running @(rf/subscribe [::running])
            selected @(rf/subscribe [::selected repo])
            tasks (cond->> (:tasks res) (not @show-private?) (remove :private?))]
        [:div {:class "flex h-full flex-col bg-panel"}
         [:div {:class "flex items-center gap-2 border-b border-line px-3 py-2 text-[13px]"}
          [:span.font-medium "bb tasks"]
          [:span.flex-1]
          [:label {:class "flex cursor-pointer items-center gap-1 text-xs text-muted"}
           [:input {:type "checkbox" :checked @show-private? :on-change #(swap! show-private? not)}]
           "show private"]
          [:button {:class "text-xs text-muted hover:text-fg" :on-click #(rf/dispatch [::load repo])} "Refresh"]]
         (cond
           (nil? res) [:div {:class "p-3 text-xs text-faint"} "Loading…"]
           (= ::none res) [:div {:class "p-3 text-xs text-faint"} "No bb.edn in this repository."]
           :else
           [:<>
            (when (:error res)
              [:pre {:class "whitespace-pre-wrap border-b border-line bg-red-500/10 px-3 py-2 font-mono text-xs text-red-600 dark:text-red-400"}
               (:error res)])
            [:div {:class (str "overflow-auto border-b border-line " (if selected "max-h-[45%]" "flex-1"))}
             (if (empty? tasks)
               [:div {:class "p-3 text-xs text-faint"} "No tasks."]
               (for [t tasks]
                 ^{:key (:name t)}
                 [task-row repo t (run-id-for running repo (:name t)) (= selected (:name t))]))]
            (when selected
              [:<>
               [:div {:class "flex items-center justify-between border-b border-line px-3 py-1 text-xs text-muted"}
                [:span.font-mono selected]
                [:button {:class "hover:text-fg" :on-click #(rf/dispatch [::select repo nil])} "Close"]]
               ^{:key (str repo selected)} [output-view repo selected]])])]))))
