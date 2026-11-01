(ns clj-browse.ui.builtins
  "Built-in form actions, registered through the extension registry."
  (:require [clj-browse.schema :as schema]
            [clj-browse.ui.registry :as registry]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]))

(defn- form-row? [{:keys [row]}] (#{:form :split} (:type row)))
(defn- kind [ctx] (or (get-in ctx [:def :kind]) (get-in ctx [:form :kind])))

(def run-kinds (into schema/runnable-kinds #{:def :defonce :defmacro}))

(defn register-all! []
  (registry/register!
   :form-action
   {:id :run :action :form/run :order 10 :icon "▶"
    :label "Run… (r) — send an invocation to the REPL"
    :applies? #(and (form-row? %) (:def %) (run-kinds (kind %)))
    :run #(rf/dispatch [:run/open %])})
  (registry/register!
   :form-action
   {:id :send-repl :action :form/send-repl :order 20 :icon "⏎"
    :label "Send to REPL (Ctrl+Enter)"
    :applies? #(#{:form :child :file} (get-in % [:row :type]))
    :run (fn [ctx] (if (= :file (get-in ctx [:row :type]))
                     (rf/dispatch [:repl/load-file ctx])
                     (rf/dispatch [:repl/eval-form ctx])))})
  (registry/register!
   :form-action
   {:id :copy :action :form/copy :order 30 :icon "⧉"
    :label "Copy form (c) — then paste into any file with v"
    :applies? #(= :form (get-in % [:row :type]))
    :run #(rf/dispatch [:form/copy %])})
  (registry/register!
   :form-action
   {:id :paste :action :form/paste :order 35 :icon "⇣"
    :label "Paste copied form here (v)"
    :applies? #(and (:clipboard @rdb/app-db) (#{:form :file} (get-in % [:row :type])))
    :run (fn [{:keys [repo path form row]}]
           (rf/dispatch [:form/paste {:repo repo :path path
                                      :after (when (= :form (:type row)) (select-keys form [:row :col]))}]))})
  (registry/register!
   :form-action
   {:id :usages :action :form/usages :order 40 :icon "⌕"
    :label "Usages (u)"
    :applies? #(and (form-row? %) (get-in % [:def :fqn]))
    :run #(rf/dispatch [:usages/show {:fqn (get-in % [:def :fqn]) :repo (:repo %)}])})
  (registry/register!
   :form-action
   {:id :copy-fqn :order 50 :icon "#"
    :label "Copy fully-qualified name"
    :applies? #(and (form-row? %) (get-in % [:def :fqn]))
    :run (fn [{:keys [def]}]
           (rf/dispatch [:clipboard/copy-text (:fqn def) "Copied FQN"]))})
  (registry/register!
   :form-action
   {:id :copy-require :order 51 :icon "⊕"
    :label "Copy require snippet"
    :applies? #(and (form-row? %) (get-in % [:def :fqn]) (not (get-in % [:def :keyword?]))
                    (not (re-find #"^:" (str (get-in % [:def :fqn])))))
    :run (fn [{:keys [def]}]
           (let [fqn (:fqn def)
                 ns (subs fqn 0 (.indexOf fqn "/"))
                 alias (last (.split ns "."))]
             (rf/dispatch [:clipboard/copy-text (str "[" ns " :as " alias "]  ;; " alias "/" (:name def))
                           "Copied require snippet"])))})
  (registry/register!
   :form-action
   {:id :edit-source :action :form/edit-source :order 60 :icon "✎"
    :label "Edit source inline"
    :applies? #(= :form (get-in % [:row :type]))
    :run #(rf/dispatch [:edit/start {:id (get-in % [:row :id]) :mode :source}])})
  (registry/register!
   :form-action
   {:id :open-external :action :file/open-external :order 70 :icon "↗"
    :label "Open file in external editor (e)"
    ;; file-wide: the button only shows on file rows; the key also works from a form row (opens its file)
    :applies? #(#{:file :form :child :split} (get-in % [:row :type]))
    :hidden? #(not= :file (get-in % [:row :type]))
    :run (fn [{:keys [repo path form]}]
           (rf/dispatch [:file/open-external {:repo repo :path path :line (or (:row form) 1)}]))}))

(rf/reg-event-fx
 :clipboard/copy-text
 (fn [_ [_ text msg]]
   {:clipboard/write text
    :dispatch [:toast/show {:kind :ok :text msg}]}))
