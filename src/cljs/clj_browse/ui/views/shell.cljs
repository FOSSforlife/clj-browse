(ns clj-browse.ui.views.shell
  (:require [clj-browse.ui.registry :as registry]
            [clj-browse.ui.util :as u]
            [clj-browse.ui.views.bb :as bb]
            [clj-browse.ui.views.outline :as outline]
            [clj-browse.ui.views.palette :as palette]
            [clj-browse.ui.views.paste :as paste]
            [clj-browse.ui.views.repl :as repl]
            [clj-browse.ui.views.repos :as repos]
            [clj-browse.ui.views.run-modal :as run-modal]
            [clj-browse.ui.views.settings :as settings]
            [clj-browse.ui.views.usages :as usages]
            [re-frame.core :as rf]))

(defn- repo-dot [repo]
  (let [{:keys [running]} @(rf/subscribe [:index])]
    (when (= repo (:repo running))
      [:span {:class "h-1.5 w-1.5 rounded-full bg-amber-400 animate-pulse" :title "Indexing…"}])))

(defn repo-tabs []
  (let [repos @(rf/subscribe [:repos]) active @(rf/subscribe [:active-repo]) view @(rf/subscribe [:view])]
    [:div {:class "flex items-end gap-0.5 overflow-x-auto border-b border-line bg-panel px-2 pt-1.5"}
     (for [{:repo/keys [path name]} repos]
       ^{:key path}
       [:button {:class (str "flex items-center gap-1.5 rounded-t-md border border-b-0 px-3 py-1 text-[13px] "
                             (if (and (= path active) (= view :outline)) "border-line bg-bg font-medium" "border-transparent text-muted hover:text-fg"))
                 :title path
                 :on-click #(rf/dispatch [:repo/activate path])}
        name [repo-dot path]])
     [:button {:class "px-2 py-1 text-muted hover:text-fg" :title "Add repos" :on-click #(rf/dispatch [:view/set :repos])} "+"]]))

(defn repo-sidebar []
  (let [repos @(rf/subscribe [:repos]) active @(rf/subscribe [:active-repo]) view @(rf/subscribe [:view])]
    [:div {:class "flex w-52 shrink-0 flex-col border-r border-line bg-panel"}
     [:div {:class "px-3 pb-1 pt-3 text-[11px] font-semibold uppercase tracking-wide text-faint"} "Repos"]
     [:div {:class "flex-1 overflow-auto px-1.5"}
      (for [{:repo/keys [path name]} repos]
        ^{:key path}
        [:button {:class (str "flex w-full items-center gap-2 rounded-md px-2 py-1 text-left text-[13px] "
                              (if (and (= path active) (= view :outline)) "bg-sel font-medium" "hover:bg-hover"))
                  :title path
                  :on-click #(rf/dispatch [:repo/activate path])}
         [:span.truncate.flex-1 name] [repo-dot path]])]
     [:button {:class "m-2 rounded-md border border-dashed border-line py-1 text-xs text-muted hover:text-fg"
               :on-click #(rf/dispatch [:view/set :repos])} "+ Add repos"]]))

(defn- toolbar [repo]
  (let [config @(rf/subscribe [:config])
        branches @(rf/subscribe [:branches repo])
        panel @(rf/subscribe [:panel])
        repo-ent @(rf/subscribe [:repo repo])
        toggle (fn [k v1 v2] #(rf/dispatch [:config/set {k (if (= v1 (get config k)) v2 v1)}]))
        btn (fn [active? label title on-click]
              [:button {:class (str "whitespace-nowrap rounded-md px-2 py-0.5 text-xs " (if active? "bg-sel text-fg" "text-muted hover:bg-hover"))
                        :title title :on-click on-click} label])]
    [:div {:class "flex items-center gap-1 border-b border-line px-4 py-1.5"}
     [:span {:class "min-w-0 shrink font-mono text-xs text-faint truncate" :title repo} repo]
     (when branches
       [:span {:class "ml-2 shrink-0 whitespace-nowrap text-[11px] text-faint"
               :title "Commits are indexed automatically. Older history beyond the window isn't indexed unless you enable full history in settings."}
        (str (:commits branches) " commits indexed"
             (when (and (pos? (:boundary branches 0)) (not (:repo/full-history? repo-ent))) " · older history not indexed"))])
     [:span.flex-1]
     [btn (:compact-view? config) "Compact" "One line per form, docstring inline" (toggle :compact-view? true false)]
     [btn (= :namespaces (:outline-view config)) "Namespaces" "Show files by namespace instead of directories"
      (toggle :outline-view :namespaces :directories)]
     [:span {:class "mx-1 h-4 w-px bg-line"}]
     (for [[k label] (concat [[:repl "REPL"]]
                             (when (:bb? repo-ent) [[:bb "bb tasks"]])
                             [[:usages "Usages"]]
                             (for [{:keys [id label]} (registry/extensions :panel)] [id label]))]
       ^{:key k} [btn (= panel k) label (str "Toggle " label " panel") #(rf/dispatch [:panel/set k])])
     [:span {:class "mx-1 h-4 w-px bg-line"}]
     [btn false "⌘K" "Search everything (Ctrl+K)" #(rf/dispatch [:palette/open])]
     [btn false "⚙" "Settings" #(rf/dispatch [:view/set :settings])]]))

(defn- outline-pane [repo]
  (let [rows @(rf/subscribe [:visible-rows])
        selected @(rf/subscribe [:selected])
        expanded @(rf/subscribe [:expanded])
        editing @(rf/subscribe [:editing])
        compact? (:compact-view? @(rf/subscribe [:config]))
        outline @(rf/subscribe [:outline repo])]
    [:div {:class "flex-1 overflow-auto px-3 py-2" :id "cb-outline"}
     (cond
       (nil? outline) [:div.p-4.text-muted "Loading outline…"]
       (empty? (:projects outline))
       [:div {:class "p-6 text-muted"}
        [:div.font-medium "No deps.edn, project.clj or shadow-cljs.edn found in this repo (yet)."]
        [:div.text-sm "Indexing may still be running."]]
       :else
       (for [row rows]
         ^{:key (pr-str (:id row))}
         [outline/outline-row {:row row :repo repo :open? (contains? expanded (:id row))
                               :selected? (= selected (:id row)) :compact? compact? :editing editing}]))]))

(defn- side-panel [repo]
  (let [panel @(rf/subscribe [:panel])]
    (when panel
      [:div {:class "w-[min(36rem,42vw)] shrink-0 border-l border-line bg-bg"}
       (case panel
         :repl [repl/repl-panel repo]
         :bb [bb/bb-panel repo]
         :usages [usages/usages-panel]
         (if-let [p (some #(when (= panel (:id %)) %) (registry/extensions :panel))]
           [(:render p) repo]
           [:div]))])))

(defn- status-bar []
  (let [{:keys [running queued]} @(rf/subscribe [:index])
        progress @(rf/subscribe [:progress])
        ws @(rf/subscribe [:ws])
        clip @(rf/subscribe [:clipboard])]
    [:div {:class "flex items-center gap-3 border-t border-line bg-panel px-3 py-0.5 text-[11px] text-muted"}
     [:span {:class (when (= ws :closed) "text-red-500")} (if (= ws :closed) "● disconnected — reconnecting" "●")]
     (if running
       [:span (str "Indexing " (name (:type running)) " · " (u/short-repo (:repo running))
                   (when-let [{:keys [done total]} (get progress (:repo running))]
                     (when (and (= :history (:type running)) (pos? total)) (str " " done "/" total " commits")))
                   (when (pos? queued) (str " · " queued " queued")))]
       [:span "Index up to date"])
     [:span.flex-1]
     (when clip [:span (str "Clipboard: " (:name clip) " — press v on a file or form to paste")])
     [:span "↑↓ move · →← expand · Enter docstring · r run · c copy · v paste · u usages · e open · Ctrl+K search"]]))

(defn- toasts []
  (let [ts @(rf/subscribe [:toasts])]
    [:div {:class "fixed bottom-8 right-4 z-50 flex flex-col gap-2"}
     (for [{:keys [id kind text action]} ts]
       ^{:key id}
       [:div {:class (str "flex max-w-md items-center gap-3 rounded-lg border px-3 py-2 text-[13px] shadow-lg "
                          (if (= kind :error) "border-red-300 bg-red-50 text-red-800 dark:border-red-800 dark:bg-red-950 dark:text-red-200"
                              "border-line bg-bg"))}
        [:span.flex-1 text]
        (when action [:button {:class "font-medium text-accent" :on-click #(do (rf/dispatch (:event action)) (rf/dispatch [:toast/dismiss id]))} (:label action)])
        [:button {:class "text-muted" :on-click #(rf/dispatch [:toast/dismiss id])} "✕"]])]))

(defn- modal-host []
  (let [m @(rf/subscribe [:modal])
        p @(rf/subscribe [:palette])]
    [:<>
     (when p [palette/palette])
     (case (:type m)
       :run [run-modal/run-modal]
       :paste [paste/paste-modal]
       nil)]))

(defn app []
  (let [config @(rf/subscribe [:config])
        repo @(rf/subscribe [:active-repo])
        view @(rf/subscribe [:view])
        sidebar? (= :sidebar (:repo-nav config))]
    [:div {:class "flex h-full flex-col"}
     (when-not sidebar? [repo-tabs])
     [:div {:class "flex min-h-0 flex-1"}
      (when sidebar? [repo-sidebar])
      (case view
        :settings [:div.flex-1.overflow-auto [settings/settings-view]]
        :repos [:div.flex-1.overflow-auto [repos/repos-view]]
        (if repo
          [:div {:class "flex min-w-0 flex-1"}
           [:div {:class "flex min-w-0 flex-1 flex-col"}
            [toolbar repo]
            [outline-pane repo]]
           [side-panel repo]]
          [:div {:class "flex-1 overflow-auto"} [repos/repos-view]]))]
     [status-bar]
     [toasts]
     [modal-host]]))
