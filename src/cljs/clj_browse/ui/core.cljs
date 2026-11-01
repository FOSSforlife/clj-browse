(ns clj-browse.ui.core
  (:require [clj-browse.ui.builtins :as builtins]
            [clj-browse.ui.events]
            [clj-browse.ui.subs :as subs]
            [clj-browse.ui.util :as u]
            [clj-browse.ui.views.outline :as outline]
            [clj-browse.ui.views.repos]
            [clj-browse.ui.views.shell :as shell]
            [clj-browse.ui.ws :as ws]
            [re-frame.core :as rf]
            [re-frame.db :as rdb]
            [reagent.dom.client :as rdc]))

(defonce root (atom nil))

(defn- global-keydown [^js e]
  (let [db @rdb/app-db
        k (u/key-string e)
        palette-key (get-in db [:config :keybindings :palette/open] "Ctrl+k")]
    (cond
      (= k palette-key) (do (.preventDefault e) (rf/dispatch [:palette/open]))
      (or (:modal db) (:palette db) (u/editable-target? e) (not= :outline (:view db))) nil
      :else
      (let [repo (:active-repo db)]
        (outline/handle-key! e {:rows (subs/visible-rows-of db)
                                :selected (get-in db [:selected repo])
                                :expanded (get-in db [:expanded repo] #{})
                                :repo repo
                                :config (:config db)})))))

(defn- escape-capture
  "Escape always closes the open modal/palette, even when an editor has focus."
  [^js e]
  (when (= "Escape" (.-key e))
    (let [db @rdb/app-db]
      (cond
        (:palette db) (do (.stopPropagation e) (.preventDefault e) (rf/dispatch [:palette/close]))
        (:modal db) (do (.stopPropagation e) (.preventDefault e) (rf/dispatch [:modal/close]))))))

(defn mount! []
  (when-not @root (reset! root (rdc/create-root (js/document.getElementById "app"))))
  (rdc/render @root [shell/app]))

(defn ^:dev/after-load reload! []
  (rf/clear-subscription-cache!)
  (builtins/register-all!)
  (mount!))

(defn- install-desktop-hooks!
  "Called by the Electron shell's native menu (see clj-browse.electron.main)."
  []
  (set! (.-cljBrowse js/window)
        #js {:addRepo (fn [path] (rf/dispatch [:clj-browse.ui.views.repos/add [path]]))
             :scanFolder (fn [path]
                           (rf/dispatch [:view/set :repos])
                           (rf/dispatch [:clj-browse.ui.views.repos/scan path]))}))

(defn init []
  (install-desktop-hooks!)
  (builtins/register-all!)
  (rf/dispatch-sync [:app/init])
  (ws/connect!)
  (js/document.addEventListener "keydown" escape-capture true)
  (js/document.addEventListener "keydown" global-keydown)
  (mount!))
