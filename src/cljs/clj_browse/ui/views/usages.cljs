(ns clj-browse.ui.views.usages
  (:require [clj-browse.ui.util :as u]
            [re-frame.core :as rf]))

(defn usages-panel []
  (let [{:keys [fqn repo rows loading?]} @(rf/subscribe [:usages])]
    [:div {:class "flex h-full flex-col"}
     [:div {:class "border-b border-line px-3 py-2"}
      [:div.text-xs.text-muted "Usages of"]
      [:div {:class "font-mono text-[13px] font-semibold break-all"} fqn]
      [:div {:class "text-[11px] text-faint"} (str "definition in " (u/short-repo repo)
                                                   " · resolution rules: docs/decisions/0001-usage-resolution.md")]]
     [:div {:class "flex-1 overflow-auto py-1"}
      (cond
        (nil? fqn) [:div.px-3.py-2.text-sm.text-muted "Select a form and press u."]
        loading? [:div.px-3.py-2.text-sm.text-muted "Loading…"]
        (empty? rows) [:div.px-3.py-2.text-sm.text-muted "No usages found in any indexed repo."]
        :else
        (for [[r rs] (group-by :repo rows)]
          ^{:key r}
          [:div.mb-2
           [:div {:class "px-3 py-1 text-xs font-medium text-muted"} (u/short-repo r)]
           (for [{:keys [path row form lang resolution] :as x} rs]
             ^{:key (str path row (:col x) lang)}
             [:div {:class "cursor-pointer px-3 py-1 hover:bg-hover"
                    :on-click #(rf/dispatch [:nav/goto {:repo r :path path :row row}])}
              [:div {:class "font-mono text-[12.5px]"} form
               (when (= :ambiguous resolution)
                 [:span {:class "ml-2 rounded bg-amber-500/15 px-1 text-[10px] text-amber-700 dark:text-amber-300"
                         :title "Several indexed repos define this var; this usage can't be attributed to one of them."}
                  "ambiguous"])
               (when (= :cljs lang) [:span {:class "ml-2 text-[10px] text-faint"} "cljs"])]
              [:div {:class "font-mono text-[11px] text-faint"} (str path ":" row)]])]))]]))
