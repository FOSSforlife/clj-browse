(ns clj-browse.ui.views.palette
  (:require [clj-browse.ui.util :as u]
            [clj-browse.ui.views.outline :as outline]
            [clojure.string :as str]
            [re-frame.core :as rf]))

(defn palette []
  (let [{:keys [q results sel]} @(rf/subscribe [:palette])
        items (vec (concat (map #(assoc % :group :name) (:names results))
                           (map #(assoc % :group :doc) (:docs results))))
        go! (fn [it] (rf/dispatch [:nav/goto (select-keys it [:repo :path :row])]))]
    [:div {:class "fixed inset-0 z-50 flex items-start justify-center bg-black/25 pt-[12vh]"
           :on-click #(rf/dispatch [:palette/close])}
     [:div {:class "w-[min(680px,94vw)] overflow-hidden rounded-xl border border-line bg-bg shadow-2xl"
            :on-click #(.stopPropagation %)}
      [:input {:class "w-full border-b border-line bg-transparent px-4 py-3 text-[15px] outline-none"
               :auto-focus true
               :placeholder "Search functions, namespaces and docstrings across all repos…"
               :value q
               :on-change #(rf/dispatch [:palette/query (.. % -target -value)])
               :on-key-down (fn [e]
                              (case (.-key e)
                                "Escape" (rf/dispatch [:palette/close])
                                "ArrowDown" (do (.preventDefault e) (rf/dispatch [:palette/move 1 (count items)]))
                                "ArrowUp" (do (.preventDefault e) (rf/dispatch [:palette/move -1 (count items)]))
                                "Enter" (when-let [it (get items sel)] (go! it))
                                nil))}]
      [:div {:class "max-h-[56vh] overflow-auto py-1"}
       (cond
         (str/blank? q) [:div.px-4.py-3.text-xs.text-faint "Fuzzy: \"dbsave\" finds my.app.db/save!. Docstring matches are listed below name matches."]
         (and results (empty? items)) [:div.px-4.py-3.text-sm.text-muted "No matches."]
         :else
         (for [[i it] (map-indexed vector items)]
           ^{:key (str (:group it) (:repo it) (:path it) (:row it) (:fqn it))}
           [:div {:class (str "flex cursor-pointer items-center gap-2 px-4 py-1.5 " (if (= i sel) "bg-sel" "hover:bg-hover"))
                  :on-click #(go! it)}
            [outline/kind-badge (:kind it)]
            [:div {:class "min-w-0 flex-1"}
             [:div {:class "truncate font-mono text-[13px]"} (:fqn it)]
             (when (= :doc (:group it))
               [:div {:class "truncate text-xs text-muted"} (first (str/split-lines (str (:doc it))))])]
            [:span {:class "shrink-0 text-xs text-faint"} (u/short-repo (:repo it))]]))]]]))
