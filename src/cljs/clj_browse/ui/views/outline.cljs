(ns clj-browse.ui.views.outline
  (:require [clj-browse.ui.editor :as editor]
            [clj-browse.ui.registry :as registry]
            [clj-browse.ui.util :as u]
            [clj-browse.ui.ws :as ws]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(def doc-kinds #{:defn :defn- :defmacro :defmulti :def :defonce :defprotocol :ns})

(def kind-style
  {:defn "bg-blue-500/10 text-blue-700 dark:text-blue-300"
   :defn- "bg-blue-500/10 text-blue-700/70 dark:text-blue-300/70"
   :defmacro "bg-purple-500/10 text-purple-700 dark:text-purple-300"
   :defmulti "bg-indigo-500/10 text-indigo-700 dark:text-indigo-300"
   :defmethod "bg-indigo-500/10 text-indigo-700 dark:text-indigo-300"
   :def "bg-emerald-500/10 text-emerald-700 dark:text-emerald-300"
   :defonce "bg-emerald-500/10 text-emerald-700 dark:text-emerald-300"
   :ns "bg-amber-500/10 text-amber-700 dark:text-amber-300"
   :comment "bg-gray-500/10 text-muted"
   :other "bg-gray-500/10 text-muted"})

(defn kind-badge [kind]
  [:span {:class (str "shrink-0 rounded px-1.5 py-px text-[10.5px] font-mono leading-4 "
                      (get kind-style kind (if (some-> kind name (str/starts-with? "reg-"))
                                             "bg-rose-500/10 text-rose-700 dark:text-rose-300"
                                             "bg-teal-500/10 text-teal-700 dark:text-teal-300")))}
   (if (= kind :reader-conditional) "#?" (some-> kind name))])

(defn lang-badges [langs file-langs]
  (when (and (seq langs) (not= (set langs) (set file-langs)))
    (into [:span.flex.gap-1]
          (for [l langs]
            [:span {:class "rounded border border-line px-1 text-[10px] font-mono text-muted"} (name l)]))))

(defn- file-langs [path]
  (cond (str/ends-with? path ".cljc") [:clj :cljs]
        (str/ends-with? path ".cljs") [:cljs]
        :else [:clj]))

;; ---------------------------------------------------------------------------
;; actions (from the registry)

(defn row-ctx [repo row]
  (case (:type row)
    :form {:repo repo :path (:path row) :form (:form row) :def (first (:defs (:form row)))
           :file-data (:file-data row) :row row}
    :split {:repo repo :path (:path row) :form (:form row) :def (:def row) :file-data (:file-data row) :row row}
    :child {:repo repo :path (:path row) :form (:form row) :child (:child row) :file-data (:file-data row) :row row}
    :file {:repo repo :path (get-in row [:file :path]) :file (:file row) :row row}
    {:repo repo :row row}))

(defn action-buttons [ctx always?]
  (into [:div {:class (str "flex items-center gap-0.5 shrink-0 "
                           (if always? "opacity-100" "opacity-0 group-hover:opacity-100"))}]
        (for [{:keys [id label icon run]} (remove #(and (:hidden? %) ((:hidden? %) ctx))
                                                    (registry/applicable :form-action ctx))]
          ^{:key id}
          [:button {:class "h-6 min-w-6 px-1 rounded text-muted hover:text-fg hover:bg-hover text-[13px] leading-none"
                    :title label
                    :on-click (fn [e] (.stopPropagation e) (run ctx))}
           icon])))

;; ---------------------------------------------------------------------------
;; docstrings

(defn docstring-view [{:keys [doc lines on-edit compact?]}]
  (cond
    (and (str/blank? doc) compact?) nil
    (str/blank? doc)
    (when on-edit
      [:div {:class "text-faint text-[13px] italic cursor-text hover:text-muted"
             :on-click (fn [e] (.stopPropagation e) (on-edit))}
       "Add docstring…"])
    compact?
    [:span {:class "truncate text-muted text-[13px] min-w-0 cursor-text"
            :title doc
            :on-click (when on-edit (fn [e] (.stopPropagation e) (on-edit)))}
     (first (str/split-lines (u/dedent doc)))]
    :else
    [u/html-div {:class "cb-md cb-clamp text-[13.5px] leading-[1.45] text-fg/85 cursor-text"
                 :style {"WebkitLineClamp" (or lines 3)}
                 :title (when (> (count (str/split-lines doc)) (or lines 3)) "Click to edit / see full")
                 :on-click (when on-edit (fn [e]
                                           (when-not (= "A" (.. e -target -tagName))
                                             (.stopPropagation e) (on-edit))))}
     (u/markdown-html doc)]))

(defn doc-editor [{:keys [initial on-save on-cancel]}]
  (let [text (r/atom (u/dedent (or initial "")))]
    (fn [_]
      [:div {:class "mt-1" :on-click #(.stopPropagation %)}
       [:textarea {:class "w-full min-h-[4.5rem] rounded-md border border-accent bg-bg p-2 text-[13.5px] leading-snug outline-none ring-2 ring-accent/20"
                   :auto-focus true
                   :value @text
                   :rows (max 3 (inc (count (str/split-lines @text))))
                   :on-change #(reset! text (.. % -target -value))
                   :on-key-down (fn [e]
                                  (case (u/key-string e)
                                    "Escape" (do (.preventDefault e) (on-cancel))
                                    "Ctrl+Enter" (do (.preventDefault e) (on-save @text))
                                    nil))}]
       [:div {:class "flex items-center gap-2 text-xs text-muted mt-1"}
        [:button {:class "rounded bg-accent px-2 py-0.5 text-white" :on-click #(on-save @text)} "Save"]
        [:button {:class "rounded px-2 py-0.5 hover:bg-hover" :on-click on-cancel} "Cancel"]
        [:span "Ctrl+Enter to save · Esc to cancel · Markdown supported · empty removes the docstring"]]])))

;; ---------------------------------------------------------------------------
;; inline source editor

(defn- split-around
  "File text → [before after] around a top-level form (by row range), so the
  edited form can be linted in its real file context."
  [file-text {:keys [row end-row]}]
  (let [lines (vec (str/split file-text #"\n" -1))]
    [(str/join "\n" (subvec lines 0 (min (count lines) (dec row))))
     (str/join "\n" (subvec lines (min (count lines) end-row)))]))

(defn source-editor [{:keys [repo path form file-data]}]
  (let [state (r/atom {:loading? true})]
    (-> (js/Promise.all #js [(ws/request :form/source {:repo repo :path path
                                                        :pos (select-keys form [:row :col :end-row :end-col])})
                             (ws/request :file/text {:repo repo :path path})])
        (.then (fn [[src file]]
                 (let [[before after] (split-around (:text file) form)]
                   (reset! state {:text (:text src) :before before :after after})))))
    (fn [{:keys [repo path form file-data]}]
      (let [{:keys [loading? text before after]} @state
            plines (dec (:row form))
            lang (if (str/ends-with? path ".cljs") :cljs :clj)]
        [:div {:class "my-1 rounded-md border border-line overflow-hidden" :on-click #(.stopPropagation %)}
         (if loading?
           [:div.p-2.text-muted.text-xs "Loading…"]
           [editor/code-editor
            {:value text
             :autofocus? true
             :line-numbers? true
             :on-change #(swap! state assoc :text %)
             :on-save (fn [t] (rf/dispatch [:form/save-source {:repo repo :path path :form form :text t}]))
             :on-cancel #(rf/dispatch [:edit/cancel])
             :lint-fn (fn [t]
                        (let [n (count (str/split-lines t))]
                          (-> (ws/request :lint {:repo repo :path path
                                                 :text (str before (when (pos? plines) "\n") t "\n" after)})
                              (.then (fn [fs] (->> fs
                                                   (filter #(< plines (:row %) (+ plines n 1)))
                                                   (map #(-> % (update :row - plines)
                                                             (update :end-row (fn [r] (when r (- r plines))))))))))))
             :complete-fn (fn [prefix*] (ws/request :repl/complete {:repo repo :prefix prefix* :ns (:ns file-data)}))
             :info-fn (fn [sym] (ws/request :repl/info {:repo repo :sym sym :ns (:ns file-data) :lang lang}))}])
         [:div {:class "flex items-center gap-2 border-t border-line bg-panel px-2 py-1 text-xs text-muted"}
          [:button {:class "rounded bg-accent px-2 py-0.5 text-white"
                    :on-click #(rf/dispatch [:form/save-source {:repo repo :path path :form form :text (:text @state)}])}
           "Save"]
          [:button {:class "rounded px-2 py-0.5 hover:bg-hover" :on-click #(rf/dispatch [:edit/cancel])} "Close"]
          [:span "Ctrl+S save · Esc close · paredit enabled · completions/hover docs when a REPL is connected"]]]))))

;; ---------------------------------------------------------------------------
;; rows

(defn- guides [depth]
  (into [:<>] (for [i (range depth)] ^{:key i} [:span {:class "w-[1.15rem] shrink-0 self-stretch border-l border-line ml-[0.55rem] -mr-[0.55rem]"}])))

(defn- bullet [{:keys [id expandable?]} open?]
  [:span {:class (str "cb-bullet cursor-pointer " (when (and expandable? (not open?)) "collapsed"))
          :on-click (fn [e] (.stopPropagation e) (when expandable? (rf/dispatch [:outline/toggle id])))}
   [:span]])

(defn- chevron [{:keys [id expandable?]} open?]
  [:span {:class (str "w-4 shrink-0 text-faint text-[10px] cursor-pointer select-none text-center "
                      (when-not expandable? "invisible"))
          :on-click (fn [e] (.stopPropagation e) (rf/dispatch [:outline/toggle id]))}
   (if open? "▼" "▶")])

(defn- arglists-str [arglists]
  (when (seq arglists) (str/join " " arglists)))

(defn- def-line [{:keys [def form path compact? usage-count ctx]}]
  [:div {:class "flex items-center gap-2 min-w-0"}
   [kind-badge (or (:kind def) (:kind form))]
   [:span {:class (str "font-mono text-[13.5px] font-semibold shrink-0 " (when (:private? def) "opacity-75"))}
    (or (:name def) (:name form))]
   (when-let [a (arglists-str (:arglists def))]
     [:span {:class "font-mono text-[12.5px] text-faint truncate min-w-0 shrink"} a])
   [lang-badges (if (> (count (:defs form)) 1) (:langs form) (or (:langs def) (:langs form))) (file-langs path)]
   (when (and usage-count (pos? usage-count))
     [:button {:class "shrink-0 rounded-full bg-panel border border-line px-1.5 text-[10.5px] text-muted hover:text-fg"
               :title "Usages"
               :on-click (fn [e] (.stopPropagation e) (rf/dispatch [:usages/show {:fqn (:fqn def) :repo (:repo ctx)}]))}
      (str usage-count " use" (when (not= 1 usage-count) "s"))])
   (into [:<>] (for [{:keys [id render]} (registry/applicable :outline-decoration ctx)]
                 ^{:key id} [render ctx]))])

(defn form-body [{:keys [row repo compact? editing selected?]}]
  (let [{:keys [form path file-data]} row
        ctx (row-ctx repo row)
        d (or (:def row) (first (:defs form)))
        editing-doc? (and (= (:id editing) (:id row)) (= :doc (:mode editing)))
        editing-src? (and (= (:id editing) (:id row)) (= :source (:mode editing)))
        doc (if (= :ns (:kind form)) (:ns-doc file-data) (:doc d))
        can-doc? (and (doc-kinds (:kind form)) (not= :split (:type row)))
        on-edit (when can-doc? #(rf/dispatch [:edit/start {:id (:id row) :mode :doc}]))
        renderer (first (registry/applicable :form-renderer ctx))]
    [:div {:class "min-w-0 flex-1 py-[3px]"}
     [:div {:class "flex items-center gap-2 min-w-0"}
      [:div {:class "min-w-0 flex-1 flex items-center gap-2"}
       (cond
         (or d (= :ns (:kind form)))
         [def-line {:def d :form form :path path :compact? compact? :ctx ctx
                    :usage-count (when (= :form (:type row)) (:usage-count form))}]
         :else
         [:div {:class "flex items-center gap-2 min-w-0"}
          [kind-badge (:kind form)]
          [:span {:class (str "font-mono truncate " (if (:summary form) "text-[12.5px] text-muted" "text-[13.5px] font-semibold"))}
           (or (:summary form) (:name form))]])
       (when (and compact? (not editing-doc?))
         [docstring-view {:doc doc :compact? true :on-edit on-edit}])]
      [action-buttons ctx selected?]]
     (cond
       editing-doc? [doc-editor {:initial doc
                                 :on-cancel #(rf/dispatch [:edit/cancel])
                                 :on-save #(rf/dispatch [:doc/save {:repo repo :path path :form form :doc %}])}]
       renderer [(:render renderer) ctx]
       (not compact?) [docstring-view {:doc doc :lines (:docstring-lines @(rf/subscribe [:config])) :on-edit on-edit}])
     (when editing-src? [source-editor {:repo repo :path path :form form :file-data file-data}])]))

(defn- child-body [{:keys [row repo selected?]}]
  [:div {:class "min-w-0 flex-1 flex items-center gap-2 py-[3px]"}
   [:span {:class "font-mono text-[12.5px] truncate min-w-0 flex-1"} (get-in row [:child :summary])]
   [action-buttons (row-ctx repo row) selected?]])

(defn- file-body [{:keys [row repo selected?]}]
  (let [{:keys [file label]} row]
    [:div {:class "min-w-0 flex-1 flex items-center gap-2 py-[3px]"}
     [:span {:class "font-mono text-[13px]"} label]
     (when (and (:ns file) (not= label (:ns file)))
       [:span {:class "text-faint text-xs font-mono truncate"} (:ns file)])
     (when (:error file)
       [:span {:class "rounded bg-red-500/10 text-red-600 dark:text-red-300 px-1.5 text-[10.5px]" :title (:error file)}
        "parse error"])
     [:span.flex-1]
     [action-buttons (row-ctx repo row) selected?]]))

(defn- tag-badges [tags]
  (into [:<>] (for [t tags :when (not= t "default")]
                ^{:key t} [:span {:class "rounded border border-line px-1 text-[10.5px] font-mono text-muted"} t])))

(defn outline-row [{:keys [row open? selected? repo compact? editing]}]
  (let [{:keys [id type depth]} row]
    [:div {:class (str "group flex items-start pr-2 rounded-md "
                       (if selected? "bg-sel" "hover:bg-hover/60"))
           :data-row-id (pr-str id)
           :on-click #(rf/dispatch [:outline/select id])
           :on-double-click #(when (:expandable? row) (rf/dispatch [:outline/toggle id]))}
     [guides depth]
     [:div {:class "flex items-center h-[26px]"}
      [chevron row open?]
      (when (#{:form :child :split} type) [bullet row open?])]
     (case type
       :project (let [p (:project row)]
                  [:div {:class "flex items-center gap-2 py-[3px] min-w-0"}
                   [:span {:class "font-semibold"} (:name p)]
                   [:span {:class "rounded bg-panel border border-line px-1.5 text-[10.5px] text-muted"} (name (:type p))]
                   [:span {:class "text-faint text-xs font-mono"} (:file p)]
                   (when (:error p) [:span {:class "text-red-500 text-xs" :title (:error p)} "⚠ couldn't read"])])
       :path [:div {:class "flex items-center gap-2 py-[3px]"}
              [:span {:class "font-mono text-[13px] font-medium"} (u/basename (get-in row [:path :dir]))]
              [:span {:class "text-faint text-xs font-mono"} (get-in row [:path :dir])]
              [tag-badges (get-in row [:path :tags])]]
       :dir [:div {:class "py-[3px] font-mono text-[13px] text-muted"} (str (:label row) "/")]
       :file [file-body {:row row :repo repo :selected? selected?}]
       :form [form-body {:row row :repo repo :compact? compact? :editing editing :selected? selected?}]
       :split [form-body {:row row :repo repo :compact? compact? :editing editing :selected? selected?}]
       :child [child-body {:row row :repo repo :selected? selected?}]
       [:div (str type)])]))

;; ---------------------------------------------------------------------------
;; keyboard

(defn- parent-index [rows i]
  (let [d (:depth (nth rows i))]
    (loop [j (dec i)] (cond (neg? j) nil (< (:depth (nth rows j)) d) j :else (recur (dec j))))))

(defn run-action! [action-kw ctx]
  (when-let [a (first (filter #(= action-kw (:action %)) (registry/applicable :form-action ctx)))]
    ((:run a) ctx)
    true))

(defn handle-key! [^js e {:keys [rows selected expanded repo config]}]
  (let [k (u/key-string e)
        bindings (into {} (map (fn [[action key]] [key action])) (:keybindings config))
        action (get bindings k)
        i (or (first (keep-indexed (fn [i r] (when (= (:id r) selected) i)) rows)) -1)
        row (when (>= i 0) (nth rows i))
        sel! (fn [j] (when-let [r (get rows j)]
                       (rf/dispatch-sync [:outline/select (:id r)])
                       (js/requestAnimationFrame
                        #(some-> (js/document.querySelector (str "[data-row-id='" (pr-str (:id r)) "']"))
                                 (.scrollIntoView #js {:block "nearest"})))))
        ctx (when row (row-ctx repo row))
        handled
        (case action
          :outline/down (do (sel! (min (dec (count rows)) (inc i))) true)
          :outline/up (do (sel! (max 0 (dec i))) true)
          :outline/expand (when row
                            (if (and (:expandable? row) (not (contains? expanded (:id row))))
                              (rf/dispatch-sync [:outline/toggle (:id row) true])
                              (sel! (inc i)))
                            true)
          :outline/collapse (when row
                              (if (contains? expanded (:id row))
                                (rf/dispatch-sync [:outline/toggle (:id row) false])
                                (some-> (parent-index rows i) sel!))
                              true)
          :form/edit-doc (when row
                           (cond
                             (and (#{:form} (:type row)) (doc-kinds (get-in row [:form :kind])))
                             (rf/dispatch [:edit/start {:id (:id row) :mode :doc}])
                             (:expandable? row) (rf/dispatch-sync [:outline/toggle (:id row)]))
                           true)
          (:form/run :form/copy :form/paste :form/usages :file/open-external :form/send-repl :form/edit-source)
          (when ctx (run-action! action ctx))
          nil)]
    (when handled (.preventDefault e))))
