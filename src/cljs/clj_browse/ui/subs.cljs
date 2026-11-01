(ns clj-browse.ui.subs
  (:require [clj-browse.ui.tree :as tree]
            [re-frame.core :as rf]))

(doseq [k [:config :repos :roots :active-repo :panel :modal :palette :clipboard :editing
           :toasts :view :ws :index :usages :progress]]
  (rf/reg-sub k (fn [db _] (get db k))))

(rf/reg-sub :repo (fn [db [_ path]] (some #(when (= path (:repo/path %)) %) (:repos db))))
(rf/reg-sub :outline (fn [db [_ repo]] (get-in db [:outlines repo])))
(rf/reg-sub :file (fn [db [_ repo path]] (get-in db [:files [repo path]])))
(rf/reg-sub :branches (fn [db [_ repo]] (get-in db [:branches repo])))
(rf/reg-sub :repl (fn [db [_ repo]] (get-in db [:repl repo])))
(rf/reg-sub :bb (fn [db [_ repo]] (get-in db [:bb repo])))

(rf/reg-sub :expanded (fn [db _] (get-in db [:expanded (:active-repo db)] #{})))
(rf/reg-sub :selected (fn [db _] (get-in db [:selected (:active-repo db)])))

(defn visible-rows-of [db]
  (let [repo (:active-repo db)]
    (tree/visible-rows {:outline (get-in db [:outlines repo])
                        :files (:files db)
                        :repo repo
                        :expanded (get-in db [:expanded repo] #{})
                        :namespace-view? (= :namespaces (get-in db [:config :outline-view]))
                        :compact-folders? (get-in db [:config :compact-folders?] true)})))

(rf/reg-sub :visible-rows (fn [db _] (visible-rows-of db)))
