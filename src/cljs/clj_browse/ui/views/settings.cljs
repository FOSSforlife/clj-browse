(ns clj-browse.ui.views.settings
  "Preferences page: config.edn editor plus per-repo indexing settings."
  (:require [clj-browse.ui.util :as u]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(def default-keybindings
  {:outline/up "ArrowUp"
   :outline/down "ArrowDown"
   :outline/expand "ArrowRight"
   :outline/collapse "ArrowLeft"
   :form/edit-doc "Enter"
   :form/run "r"
   :form/copy "c"
   :form/paste "v"
   :form/usages "u"
   :file/open-external "e"
   :form/send-repl "Ctrl+Enter"
   :palette/open "Ctrl+k"})

(def default-nrepl
  {:nrepl-version "1.8.0"
   :cider-nrepl-version "0.64.0"
   :middleware ["cider.nrepl/cider-middleware"]})

;; ---------------------------------------------------------------------------
;; events

(rf/reg-event-fx
 ::load-branches
 (fn [_ [_ repo]]
   {:ws {:op :repos/branches :params {:path repo} :on-ok [::branches-loaded repo]}}))

(rf/reg-event-db ::branches-loaded (fn [db [_ repo v]] (assoc-in db [:settings :branches repo] v)))
(rf/reg-sub ::branch-info (fn [db [_ repo]] (get-in db [:settings :branches repo])))

;; ---------------------------------------------------------------------------
;; widgets

(defn- section [title & body]
  (into [:section {:class "mb-8"}
         [:h2 {:class "mb-3 border-b border-line pb-1 text-sm font-semibold uppercase tracking-wide text-muted"} title]]
        body))

(defn- row [label help control]
  [:div {:class "flex items-start justify-between gap-6 py-2"}
   [:div {:class "min-w-0 flex-1"}
    [:div {:class "text-[13px] font-medium"} label]
    (when help [:div {:class "mt-0.5 text-xs text-muted"} help])]
   [:div {:class "shrink-0"} control]])

(defn- segmented [value options on-change]
  (into [:div {:class "flex overflow-hidden rounded border border-line text-xs"}]
        (for [[v label] options]
          ^{:key (str v)}
          [:button {:class (str "px-3 py-1 " (if (= v value) "bg-sel text-fg" "text-muted hover:bg-hover"))
                    :on-click #(on-change v)} label])))

(defn- toggle [on? on-change]
  [:button {:class (str "relative h-5 w-9 rounded-full border border-line transition-colors "
                        (if on? "bg-accent" "bg-hover"))
            :role "switch" :aria-checked (boolean on?)
            :on-click #(on-change (not on?))}
   [:span {:class (str "absolute top-0.5 h-3.5 w-3.5 rounded-full bg-white shadow transition-all "
                       (if on? "left-[1.1rem]" "left-0.5"))}]])

(def ^:private input-cls "rounded border border-line bg-bg px-2 py-1 text-[13px] text-fg outline-none focus:border-accent")

(defn- commit-input
  "Text/number input that dispatches on blur or Enter, keeping local state while editing."
  [_props]
  (let [local (r/atom nil)]
    (fn [{:keys [value on-commit type class]}]
      (let [commit! (fn []
                      (when (some? @local)
                        (on-commit @local)
                        (reset! local nil)))]
        [:input {:type (or type "text")
                 :class (or class input-cls)
                 :value (or @local (str value))
                 :on-change #(reset! local (.. % -target -value))
                 :on-blur commit!
                 :on-key-down #(when (= "Enter" (.-key %)) (commit!))}]))))

(defn- number-input [{:keys [value min max on-commit]}]
  [commit-input {:value value :type "number" :class (str input-cls " w-24")
                 :on-commit (fn [s]
                              (let [n (js/parseInt s)]
                                (when-not (js/isNaN n)
                                  (on-commit (cond-> n min (clojure.core/max min) max (clojure.core/min max))))))}])

(defn- set-config! [m] (rf/dispatch [:config/set m]))

;; ---------------------------------------------------------------------------
;; keybindings

(defn- key-capture [action current capturing]
  (let [active? (= action @capturing)]
    [:button {:class (str "min-w-28 rounded border px-2 py-1 font-mono text-xs "
                          (if active? "border-accent bg-sel" "border-line hover:bg-hover"))
              :on-click #(reset! capturing action)
              :on-blur #(when (= action @capturing) (reset! capturing nil))
              :on-key-down (fn [e]
                             (when active?
                               (.preventDefault e)
                               (.stopPropagation e)
                               (let [k (.-key e)]
                                 (cond
                                   (= k "Escape") (reset! capturing nil)
                                   (#{"Control" "Shift" "Alt" "Meta"} k) nil
                                   :else (do (set-config! {:keybindings {action (u/key-string e)}})
                                             (reset! capturing nil))))))}
     (if active? "press a key…" current)]))

(defn- keybindings-section [_config]
  (let [capturing (r/atom nil)]
    (fn [config]
      (let [kb (merge default-keybindings (:keybindings config))]
        [section "Keybindings"
         [:table {:class "w-full text-[13px]"}
          [:tbody
           (for [[action k] (sort-by (comp str key) kb)]
             ^{:key action}
             [:tr {:class "border-b border-line/50"}
              [:td {:class "py-1.5 font-mono text-xs text-muted"} (subs (str action) 1)]
              [:td {:class "py-1.5 text-right"} [key-capture action k capturing]]])]]
         [:button {:class "mt-3 rounded border border-line px-3 py-1 text-xs hover:bg-hover"
                   :on-click #(set-config! {:keybindings default-keybindings})}
          "Reset to defaults"]]))))

;; ---------------------------------------------------------------------------
;; nrepl

(defn- nrepl-section [config]
  (let [nr (merge default-nrepl (:nrepl config))
        save! (fn [k v] (set-config! {:nrepl (assoc nr k v)}))]
    [section "REPL"
     [row "nREPL version" "Used when clj-browse starts a REPL for a repo."
      [commit-input {:value (:nrepl-version nr) :on-commit #(save! :nrepl-version (str/trim %))}]]
     [row "cider-nrepl version" nil
      [commit-input {:value (:cider-nrepl-version nr) :on-commit #(save! :cider-nrepl-version (str/trim %))}]]
     [row "Middleware" "Comma or space separated."
      [commit-input {:value (str/join ", " (:middleware nr)) :class (str input-cls " w-72")
                     :on-commit #(save! :middleware (vec (remove str/blank? (str/split % #"[,\s]+"))))}]]]))

;; ---------------------------------------------------------------------------
;; per-repo

(defn- ref-kind [{:keys [kind]}] kind)

(defn- repo-section []
  (let [pins (r/atom nil)
        loaded-for (atom nil)]
    (fn []
      (let [repo @(rf/subscribe [:active-repo])
            ent @(rf/subscribe [:repo repo])
            info @(rf/subscribe [::branch-info repo])]
        (when (and repo (not= repo @loaded-for))
          (reset! loaded-for repo)
          (reset! pins nil)
          (rf/dispatch [::load-branches repo]))
        (if-not repo
          [section "Active repository" [:div.text-xs.text-muted "No repository selected."]]
          (let [current (set (:repo/pinned-branches ent))
                sel (or @pins current)
                refs (filter #(#{:branch :remote} (ref-kind %)) (:refs info))]
            [section (str "Repository: " (u/short-repo repo))
             [row "Index full history"
              "Index every commit instead of only the recent window."
              [toggle (:repo/full-history? ent)
               #(rf/dispatch [:repo/update repo {:repo/full-history? %}])]]
             [row "Commit window override"
              "Number of recent commits to index for this repo. Default: global value."
              [number-input {:value (:repo/commit-window ent) :min 1
                             :on-commit #(rf/dispatch [:repo/update repo {:repo/commit-window %}])}]]
             [:div {:class "py-2"}
              [:div {:class "text-[13px] font-medium"} "Pinned branches"]
              [:div {:class "mb-2 text-xs text-muted"}
               "Pinned refs are always indexed in addition to the current and default branch (shown as auto)."
               (when info (str " " (:commits info) " commits indexed."))]
              (if-not info
                [:div.text-xs.text-faint "Loading refs…"]
                [:div {:class "max-h-64 overflow-auto rounded border border-line"}
                 (for [{:keys [name tracked? kind]} refs
                       :let [short (-> name (str/replace #"^refs/heads/" "") (str/replace #"^refs/remotes/" ""))
                             pinned? (contains? sel name)
                             auto? (and tracked? (not pinned?))]]
                   ^{:key name}
                   [:label {:class "flex cursor-pointer items-center gap-2 px-2 py-1 text-[13px] hover:bg-hover"}
                    [:input {:type "checkbox" :checked (boolean (or pinned? auto?)) :disabled auto?
                             :on-change #(reset! pins (if pinned? (disj sel name) (conj sel name)))}]
                    [:span.font-mono short]
                    (when (= kind :remote) [:span.text-xs.text-faint "remote"])
                    (when auto? [:span {:class "rounded bg-sel px-1 text-[10px] text-muted"} "auto"])])])
              [:button {:class "mt-2 rounded bg-accent px-3 py-1 text-xs text-white disabled:opacity-50"
                        :disabled (nil? @pins)
                        :on-click (fn []
                                    (rf/dispatch [:repo/update repo {:repo/pinned-branches (vec (sort @pins))}])
                                    (reset! pins nil))}
               "Save pinned branches"]]])))))) 

;; ---------------------------------------------------------------------------
;; page

(defn settings-view []
  (let [config @(rf/subscribe [:config])]
    [:div {:class "h-full overflow-auto bg-bg text-fg"}
     [:div {:class "mx-auto max-w-3xl px-6 py-6"}
      [:div {:class "mb-6 flex items-center justify-between"}
       [:h1 {:class "text-xl font-semibold"} "Settings"]
       [:button {:class "rounded border border-line px-3 py-1 text-xs hover:bg-hover"
                 :on-click #(rf/dispatch [:view/set :outline])} "Back to outline"]]

      [section "Appearance"
       [row "Theme" nil
        [segmented (:theme config) [[:system "System"] [:light "Light"] [:dark "Dark"]] #(set-config! {:theme %})]]
       [row "Repository navigation" "How open repositories are switched."
        [segmented (:repo-nav config) [[:tabs "Tabs"] [:sidebar "Sidebar"]] #(set-config! {:repo-nav %})]]
       [row "Compact view by default" nil
        [toggle (:compact-view? config) #(set-config! {:compact-view? %})]]
       [row "Outline view" nil
        [segmented (:outline-view config) [[:directories "Directories"] [:namespaces "Namespaces"]]
         #(set-config! {:outline-view %})]]
       [row "Compact folders" "Join single-child folders into one row."
        [toggle (:compact-folders? config) #(set-config! {:compact-folders? %})]]
       [row "Docstring preview lines" "Lines of docstring shown under each form (1-20)."
        [number-input {:value (:docstring-lines config) :min 1 :max 20
                       :on-commit #(set-config! {:docstring-lines %})}]]]

      [section "Editor"
       [row "External editor command"
        [:div
         [:div "Placeholders: " [:code "{file}"] " " [:code "{line}"] ". Use " [:code "$EDITOR"]
          " to run $VISUAL / $EDITOR."]
         [:ul {:class "mt-1 font-mono"}
          (for [ex ["code -g {file}:{line}" "emacsclient -n +{line} {file}"
                    "idea --line {line} {file}" "zed {file}:{line}"]]
            ^{:key ex} [:li [:button {:class "hover:text-accent" :on-click #(set-config! {:editor-command ex})} ex]])]]
        [commit-input {:value (:editor-command config) :class (str input-cls " w-72 font-mono")
                       :on-commit #(set-config! {:editor-command %})}]]]

      [section "Indexing"
       [row "Default commit window"
        "How many recent commits are indexed per repo (default 100). Repos can override this or index their full history below."
        [number-input {:value (:commit-window config) :min 1 :on-commit #(set-config! {:commit-window %})}]]]

      [nrepl-section config]
      [keybindings-section config]

      [section "Server"
       [row "Host" nil [:code (str (get-in config [:server :host]))]]
       [row "Port" "Changes to host/port in config.edn take effect after restart." [:code (str (get-in config [:server :port]))]]
       [:div {:class "text-xs text-muted"}
        "To require an auth token, start the server with " [:code "--token <secret>"] "."]]

      [repo-section]]]))
