(ns clj-browse.ui.views.repos
  "Repository management: scan folders, add repos, reorder, re-index, remove."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]))

;; ---------------------------------------------------------------------------
;; events

(rf/reg-event-fx
 ::scan
 (fn [_ [_ path]]
   {:ws {:op :roots/scan :params {:path path} :on-ok [::scanned]}}))

(rf/reg-event-fx
 ::scanned
 (fn [{:keys [db]} [_ res]]
   {:db (assoc-in db [:repos-view :scan] res)
    :ws {:op :roots/list :params {} :on-ok [::roots-loaded]}}))

(rf/reg-event-db ::roots-loaded (fn [db [_ roots]] (assoc db :roots roots)))
(rf/reg-sub ::scan-result (fn [db _] (get-in db [:repos-view :scan])))

(rf/reg-event-fx
 ::forget
 (fn [_ [_ path]]
   {:ws {:op :roots/remove :params {:path path} :on-ok [::roots-loaded]}}))

(rf/reg-event-fx
 ::add
 (fn [_ [_ paths]]
   {:ws {:op :repos/add :params {:paths (vec paths)} :on-ok [::added]}}))

(rf/reg-event-fx
 ::added
 (fn [{:keys [db]} [_ added]]
   (let [paths (mapv #(if (string? %) % (:repo/path %)) added)
         pset (set paths)]
     {:db (update-in db [:repos-view :scan :found]
                     (fn [fs] (when fs (mapv #(cond-> % (contains? pset (:path %)) (assoc :added? true)) fs))))
      :fx [[:dispatch [:toast/show {:kind :ok :text (str "Added " (count paths) (if (= 1 (count paths)) " repository" " repositories"))}]]
           (when (and (empty? (:repos db)) (seq paths))
             [:dispatch [:repo/activate (first paths)]])]})))

(rf/reg-event-fx
 ::reorder
 (fn [{:keys [db]} [_ path dir]]
   (let [paths (mapv :repo/path (:repos db))
         i (.indexOf (to-array paths) path)
         j (+ i dir)]
     (when (and (>= i 0) (< -1 j (count paths)))
       {:ws {:op :repos/reorder
             :params {:paths (-> paths (assoc i (paths j)) (assoc j path))}}}))))

(rf/reg-event-fx
 ::reindex
 (fn [_ [_ path]]
   {:ws {:op :repos/reindex :params {:path path} :on-ok [::reindexed]}}))

(rf/reg-event-fx ::reindexed (fn [_ _] {:dispatch [:toast/show {:kind :ok :text "Re-indexing started"}]}))

(rf/reg-event-fx
 ::remove
 (fn [_ [_ path]]
   {:ws {:op :repos/remove :params {:path path}}}))

;; ---------------------------------------------------------------------------
;; views

(def ^:private input-cls "flex-1 rounded border border-line bg-bg px-2 py-1 text-[13px] text-fg outline-none focus:border-accent")
(def ^:private btn "rounded border border-line px-3 py-1 text-xs hover:bg-hover disabled:opacity-50")
(def ^:private btn-primary "rounded bg-accent px-3 py-1 text-xs text-white disabled:opacity-50")

(defn- path-form
  "Text field + submit button. `on-submit` gets the trimmed string."
  [_props]
  (let [v (r/atom "")]
    (fn [{:keys [on-submit placeholder label]}]
      (let [go! #(let [s (.trim @v)] (when (seq s) (on-submit s)))]
        [:div {:class "flex gap-2"}
         [:input {:class input-cls :placeholder placeholder :value @v
                  :on-change #(reset! v (.. % -target -value))
                  :on-key-down #(when (= "Enter" (.-key %)) (go!))}]
         [:button {:class btn-primary :disabled (empty? (.trim @v)) :on-click go!} label]]))))

(defn- scan-results []
  (let [sel (r/atom nil)
        seen (atom nil)]
    (fn []
      (let [res @(rf/subscribe [::scan-result])]
        (when-not (identical? res @seen)
          (reset! seen res)
          (reset! sel (set (map :path (remove :added? (:found res))))))
        (when res
          (let [found (:found res)
                addable (remove :added? found)]
            [:div {:class "mt-3 rounded border border-line"}
             [:div {:class "flex items-center justify-between border-b border-line px-3 py-1.5 text-xs text-muted"}
              [:span (str (count found) " found under ") [:code (:root res)]]
              [:span {:class "flex gap-3"}
               [:button {:class "hover:text-fg" :on-click #(reset! sel (set (map :path addable)))} "All"]
               [:button {:class "hover:text-fg" :on-click #(reset! sel #{})} "None"]]]
             (if (empty? found)
               [:div {:class "px-3 py-3 text-xs text-faint"} "No git repositories found."]
               [:div {:class "max-h-72 overflow-auto"}
                (for [{:keys [path name added?]} found]
                  ^{:key path}
                  [:label {:class (str "flex items-center gap-2 px-3 py-1 text-[13px] "
                                       (if added? "opacity-60" "cursor-pointer hover:bg-hover"))}
                   [:input {:type "checkbox" :disabled added? :checked (or added? (contains? @sel path))
                            :on-change #(swap! sel (fn [s] (if (contains? s path) (disj s path) (conj s path))))}]
                   [:span.font-medium name]
                   [:span {:class "min-w-0 flex-1 truncate font-mono text-xs text-faint"} path]
                   (when added? [:span {:class "rounded bg-sel px-1.5 text-[10px] text-muted"} "added"])])])
             [:div {:class "border-t border-line px-3 py-2"}
              [:button {:class btn-primary
                        :disabled (empty? (filter @sel (map :path addable)))
                        :on-click #(rf/dispatch [::add (filter @sel (map :path addable))])}
               (str "Add selected (" (count (filter @sel (map :path addable))) ")")]]]))))))

(defn- roots-list []
  (let [roots @(rf/subscribe [:roots])]
    (when (seq roots)
      [:div {:class "mt-4"}
       [:div {:class "mb-1 text-xs font-medium text-muted"} "Remembered folders"]
       [:div {:class "rounded border border-line"}
        (for [p roots]
          ^{:key p}
          [:div {:class "flex items-center gap-2 border-b border-line/50 px-3 py-1 last:border-b-0"}
           [:span {:class "min-w-0 flex-1 truncate font-mono text-xs"} p]
           [:button {:class btn :on-click #(rf/dispatch [::scan p])} "Re-scan"]
           [:button {:class (str btn " hover:text-red-500") :on-click #(rf/dispatch [::forget p])} "Forget"]])]])))

(defn- repo-row [idx n {:repo/keys [path name] :keys [exists? bb?]} confirming]
  [:div {:class "flex items-center gap-2 border-b border-line/50 px-3 py-1.5 last:border-b-0"}
   [:div {:class "min-w-0 flex-1"}
    [:div {:class "flex items-center gap-2 text-[13px]"}
     [:span.font-medium name]
     (when bb? [:span {:class "rounded bg-sel px-1.5 text-[10px] text-muted"} "bb"])
     (when (false? exists?) [:span {:class "rounded bg-red-500/15 px-1.5 text-[10px] text-red-600 dark:text-red-400"} "missing directory"])]
    [:div {:class "truncate font-mono text-xs text-faint"} path]]
   [:button {:class btn :disabled (false? exists?) :on-click #(rf/dispatch [:repo/activate path])} "Open"]
   [:button {:class btn :title "Move up" :disabled (zero? idx) :on-click #(rf/dispatch [::reorder path -1])} "↑"]
   [:button {:class btn :title "Move down" :disabled (= idx (dec n)) :on-click #(rf/dispatch [::reorder path 1])} "↓"]
   [:button {:class btn :disabled (false? exists?) :on-click #(rf/dispatch [::reindex path])} "Re-index"]
   (if (= path @confirming)
     [:span {:class "flex gap-1"}
      [:button {:class "rounded bg-red-600 px-3 py-1 text-xs text-white"
                :on-click (fn [] (reset! confirming nil) (rf/dispatch [::remove path]))} "Confirm remove"]
      [:button {:class btn :on-click #(reset! confirming nil)} "Cancel"]]
     [:button {:class (str btn " hover:text-red-500") :on-click #(reset! confirming path)} "Remove"])])

(defn- repos-list []
  (let [confirming (r/atom nil)]
    (fn []
      (let [repos @(rf/subscribe [:repos])]
        [:section {:class "mt-8"}
         [:h2 {:class "mb-2 text-sm font-semibold uppercase tracking-wide text-muted"} "Your repositories"]
         (if (empty? repos)
           [:div {:class "text-xs text-faint"} "No repositories yet. Scan a folder or add one by path above."]
           [:div {:class "rounded border border-line"}
            (map-indexed (fn [i repo] ^{:key (:repo/path repo)} [repo-row i (count repos) repo confirming]) repos)])]))))

(defn repos-view []
  (r/with-let [#_:clj-kondo/ignore init (rf/dispatch [::roots-refresh])]
    [:div {:class "h-full overflow-auto bg-bg text-fg"}
     [:div {:class "mx-auto max-w-3xl px-6 py-6"}
      [:div {:class "mb-6 flex items-start justify-between"}
       [:div
        [:h1 {:class "text-xl font-semibold"} "Repositories"]
        [:p {:class "mt-1 text-[13px] text-muted"}
         "Repositories are never added automatically. Scan a folder to find repositories, then choose which to add. "
         "Indexing of commits and file changes happens automatically."]]
       (when (seq @(rf/subscribe [:repos]))
         [:button {:class btn :on-click #(rf/dispatch [:view/set :outline])} "Back to outline"])]

      [:section
       [:h2 {:class "mb-2 text-sm font-semibold uppercase tracking-wide text-muted"} "Scan a folder"]
       [path-form {:placeholder "~/code" :label "Scan" :on-submit #(rf/dispatch [::scan %])}]
       [scan-results]
       [roots-list]]

      [:section {:class "mt-8"}
       [:h2 {:class "mb-2 text-sm font-semibold uppercase tracking-wide text-muted"} "Add a single repository"]
       [path-form {:placeholder "/path/to/repo" :label "Add" :on-submit #(rf/dispatch [::add [%]])}]]

      [repos-list]]]))

(rf/reg-event-fx ::roots-refresh (fn [_ _] {:ws {:op :roots/list :params {} :on-ok [::roots-loaded]}}))
