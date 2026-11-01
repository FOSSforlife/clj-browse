(ns clj-browse.ui.views.paste
  (:require [clj-browse.ui.editor :as editor]
            [clojure.string :as str]
            [re-frame.core :as rf]))

(defn- spec-str [{:keys [ns as refer]}]
  (str "[" ns (when as (str " :as " as)) (when (seq refer) (str " :refer [" (str/join " " refer) "]")) "]"))

(defn paste-modal []
  (let [{:keys [target plan clip selected]} @(rf/subscribe [:modal])
        {:keys [requires conflicts warnings]} plan]
    [:div {:class "fixed inset-0 z-50 flex items-start justify-center bg-black/30 pt-[10vh]"
           :on-click #(rf/dispatch [:modal/close])}
     [:div {:class "w-[min(680px,94vw)] rounded-xl border border-line bg-bg shadow-2xl"
            :on-click #(.stopPropagation %)}
      [:div {:class "border-b border-line px-5 py-3"}
       [:div.font-semibold (str "Paste " (:name clip))]
       [:div {:class "text-xs text-muted font-mono"} (str "into " (:path target)
                                                         (when (:after target) (str " after line " (:row (:after target)))))]]
      [:div {:class "space-y-3 px-5 py-4 text-[13px]"}
       (when (seq warnings)
         (into [:div.space-y-1] (for [w warnings] [:div {:class "rounded bg-amber-500/10 px-2 py-1"} w])))
       (if (seq requires)
         [:div
          [:div.mb-1.text-xs.text-muted "Add these requires to the target ns:"]
          (for [[i r] (map-indexed vector requires)]
            ^{:key i}
            [:label {:class "flex items-center gap-2 font-mono text-[12.5px]"}
             [:input {:type "checkbox" :checked (contains? selected i)
                      :on-change #(rf/dispatch [:paste/toggle-require i])}]
             (spec-str r)])]
         [:div.text-xs.text-muted "No new requires needed."])
       (when (seq conflicts)
         [:div
          [:div.mb-1.text-xs.text-red-600 "Alias conflicts (existing alias kept — fix the pasted form manually):"]
          (for [{:keys [alias wanted existing]} conflicts]
            ^{:key alias}
            [:div {:class "font-mono text-[12px]"} (str alias " → " existing " (form expects " wanted ")")])])
       [:div {:class "rounded-md border border-line overflow-hidden max-h-[30vh] overflow-y-auto"}
        [editor/code-editor {:value (:text clip)}]]]
      [:div {:class "flex justify-end gap-2 border-t border-line bg-panel/60 px-5 py-2.5"}
       [:button {:class "rounded-md px-3 py-1 hover:bg-hover" :on-click #(rf/dispatch [:modal/close])} "Cancel"]
       [:button {:class "rounded-md bg-accent px-4 py-1 font-medium text-white" :auto-focus true
                 :on-click #(rf/dispatch [:paste/apply])} "Paste"]]]]))
