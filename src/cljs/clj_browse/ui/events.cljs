(ns clj-browse.ui.events
  (:require [clj-browse.ui.tree :as tree]
            [clojure.string :as str]
            [re-frame.core :as rf]))

(def initial-db
  {:ws :connecting
   :config {}
   :repos []
   :roots []
   :active-repo nil
   :outlines {}      ; repo -> outline
   :files {}         ; [repo path] -> file-forms
   :expanded {}      ; repo -> #{ids}
   :selected {}      ; repo -> id
   :editing nil      ; {:id :mode :doc|:source ...}
   :panel nil        ; :repl :bb :usages
   :usages nil
   :repl {}          ; repo -> {:status .. :output [..]}
   :bb {}            ; repo -> {:tasks .. :output {task text}}
   :modal nil
   :palette nil
   :clipboard nil
   :index {}
   :progress {}
   :toasts []
   :view :outline})  ; :outline :settings :repos

;; ---------------------------------------------------------------------------
;; boot

(rf/reg-event-fx
 :app/init
 (fn [_ _]
   {:db initial-db
    :ws {:op :app/init :params {} :on-ok [:app/initialized]}}))

(rf/reg-event-fx
 :app/reconnected
 (fn [{:keys [db]} _]
   {:ws {:op :app/init :params {} :on-ok [:app/initialized]}
    :fx (for [[repo path] (keys (:files db))] [:dispatch [:file/load repo path]])}))

(rf/reg-event-fx
 :app/initialized
 (fn [{:keys [db]} [_ {:keys [config repos roots index]}]]
   (let [stored (some-> (js/localStorage.getItem "cb.active-repo"))
         active (or (some #(when (= stored (:repo/path %)) stored) repos)
                    (:active-repo db)
                    (:repo/path (first repos)))]
     {:db (assoc db :config config :repos repos :roots roots :index index :active-repo active
                 :view (if (empty? repos) :repos (:view db)))
      :fx [(when active [:dispatch [:repo/activate active]])
           [:theme/apply (:theme config)]]})))

(rf/reg-event-db :ws/status (fn [db [_ s]] (assoc db :ws s)))

;; ---------------------------------------------------------------------------
;; theme

(defonce ^:private media (js/window.matchMedia "(prefers-color-scheme: dark)"))

(rf/reg-fx
 :theme/apply
 (fn [theme]
   (let [dark? (case theme :dark true :light false (.-matches media))]
     (.toggle (.-classList js/document.documentElement) "dark" dark?))))

(rf/reg-event-fx :theme/system-changed (fn [{:keys [db]} _] {:theme/apply (get-in db [:config :theme])}))

(defonce ^:private _media-listener
  (.addEventListener media "change" #(rf/dispatch [:theme/system-changed])))

;; ---------------------------------------------------------------------------
;; server push events

(defmulti server-event (fn [_cofx [_ type _]] type))
(defmethod server-event :default [_ _] {})

(rf/reg-event-fx :server/event (fn [cofx ev] (server-event cofx ev)))

(defmethod server-event :repos/changed [{:keys [db]} [_ _ repos]]
  (let [active (:active-repo db)
        still? (some #(= active (:repo/path %)) repos)]
    {:db (cond-> (assoc db :repos repos) (not still?) (assoc :active-repo (:repo/path (first repos))))
     :fx [(when (and (not still?) (seq repos)) [:dispatch [:repo/activate (:repo/path (first repos))]])]}))

(defmethod server-event :repo/changed [{:keys [db]} [_ _ {:keys [repo changed removed]}]]
  (let [loaded (set (keep (fn [[r p]] (when (= r repo) p)) (keys (:files db))))
        reload (filter loaded changed)]
    {:db (update db :files #(apply dissoc % (map (fn [p] [repo p]) removed)))
     :fx (concat
          (when (contains? (:outlines db) repo) [[:dispatch [:outline/load repo]]])
          (for [p reload] [:dispatch [:file/load repo p]]))}))

(defmethod server-event :repo/history-changed [{:keys [db]} [_ _ {:keys [repo]}]]
  {:fx [(when (= repo (:active-repo db)) [:dispatch [:repo/load-branches repo]])]})

(defmethod server-event :index/status [{:keys [db]} [_ _ st]] {:db (assoc db :index st)})

(defmethod server-event :index/progress [{:keys [db]} [_ _ {:keys [repo] :as p}]]
  {:db (assoc-in db [:progress repo] p)})

(defmethod server-event :index/error [_ [_ _ {:keys [repo message]}]]
  {:dispatch [:toast/show {:kind :error :text (str "Indexing failed for " repo ": " message)}]})

(defmethod server-event :config/changed [{:keys [db]} [_ _ cfg]]
  {:db (assoc db :config cfg) :theme/apply (:theme cfg)})

;; ---------------------------------------------------------------------------
;; toasts

(rf/reg-event-fx
 :toast/show
 (fn [{:keys [db]} [_ toast]]
   (let [id (random-uuid)]
     {:db (update db :toasts conj (assoc toast :id id))
      :dispatch-later {:ms (or (:ms toast) (if (= :error (:kind toast)) 8000 4000))
                       :dispatch [:toast/dismiss id]}})))

(rf/reg-event-db :toast/dismiss (fn [db [_ id]] (update db :toasts (fn [ts] (vec (remove #(= id (:id %)) ts))))))

;; ---------------------------------------------------------------------------
;; repos & outline

(rf/reg-event-fx
 :repo/activate
 (fn [{:keys [db]} [_ repo]]
   (js/localStorage.setItem "cb.active-repo" repo)
   {:db (assoc db :active-repo repo :view :outline)
    :fx [[:dispatch [:outline/load repo]]
         [:dispatch [:repl/refresh-status repo]]
         [:dispatch [:repo/load-branches repo]]]}))

(rf/reg-event-fx
 :repo/load-branches
 (fn [_ [_ repo]]
   {:ws {:op :repos/branches :params {:path repo} :on-ok [:repo/branches-loaded repo]}}))

(rf/reg-event-db :repo/branches-loaded (fn [db [_ repo b]] (assoc-in db [:branches repo] b)))

(rf/reg-event-fx
 :outline/load
 (fn [_ [_ repo]]
   {:ws {:op :outline/get :params {:repo repo} :on-ok [:outline/loaded repo]}}))

(defn- default-expanded [outline]
  ;; first project and its default paths open on first load
  (let [p (first (:projects outline))]
    (into (if p #{[:project (:key p)]} #{})
          (for [path (:paths p) :when (some #{"default"} (:tags path))] [:path (:key p) (:dir path)]))))

(rf/reg-event-fx
 :outline/loaded
 (fn [{:keys [db]} [_ repo outline]]
   (let [first? (not (contains? (:expanded db) repo))
         stored (when first?
                  (some-> (js/localStorage.getItem (str "cb.expanded." repo)) js/JSON.parse
                          (js->clj) (->> (map (fn [v] (into [(keyword (first v))] (rest v))))) set))
         db (cond-> (assoc-in db [:outlines repo] outline)
              first? (assoc-in [:expanded repo] (or (not-empty stored) (default-expanded outline))))
         open-files (for [[t _ path] (get-in db [:expanded repo]) :when (= t :file)] path)]
     {:db db
      :fx (concat
           (for [p (distinct open-files) :when (not (get-in db [:files [repo p]]))]
             [:dispatch [:file/load repo p]])
           [[:dispatch [:nav/resolve-pending]]])})))

(rf/reg-event-fx
 :file/load
 (fn [_ [_ repo path]]
   {:ws {:op :file/forms :params {:repo repo :path path} :on-ok [:file/loaded repo path]}}))

(rf/reg-event-fx
 :file/loaded
 (fn [{:keys [db]} [_ repo path data]]
   {:db (if data (assoc-in db [:files [repo path]] data) (update db :files dissoc [repo path]))
    :dispatch [:nav/resolve-pending]}))

(defn- persist-expanded! [repo ids]
  (js/localStorage.setItem (str "cb.expanded." repo)
                           (js/JSON.stringify (clj->js (map (fn [id] (into [(name (first id))] (rest id))) ids)))))

(rf/reg-event-fx
 :outline/toggle
 (fn [{:keys [db]} [_ id force]]
   (let [repo (:active-repo db)
         open? (contains? (get-in db [:expanded repo]) id)
         want (if (nil? force) (not open?) force)
         db (update-in db [:expanded repo] (fnil (if want conj disj) #{}) id)]
     (persist-expanded! repo (get-in db [:expanded repo]))
     {:db db
      :fx [(when (and want (= :file (first id)) (not (get-in db [:files [repo (nth id 2)]])))
             [:dispatch [:file/load repo (nth id 2)]])]})))

(rf/reg-event-fx
 :outline/expand-all-forms
 (fn [{:keys [db]} [_ file-id]]
   {:dispatch [:outline/toggle file-id true]
    :db db}))

(rf/reg-event-db :outline/select (fn [db [_ id]] (assoc-in db [:selected (:active-repo db)] id)))

;; ---------------------------------------------------------------------------
;; navigation to a location (search results, usages)

(rf/reg-event-fx
 :nav/goto
 (fn [{:keys [db]} [_ {:keys [repo] :as target}]]
   {:db (assoc db :nav-pending target :palette nil :modal nil)
    :fx [(if (not= repo (:active-repo db))
           [:dispatch [:repo/activate repo]]
           [:dispatch [:nav/resolve-pending]])]}))

(rf/reg-event-fx
 :nav/resolve-pending
 (fn [{:keys [db]} _]
   (if-let [{:keys [repo path row]} (:nav-pending db)]
     (let [outline (get-in db [:outlines repo])
           compact? (get-in db [:config :compact-folders?] true)
           anc (when outline (tree/ancestors-of outline path compact?))
           data (get-in db [:files [repo path]])]
       (cond
         (or (not outline) (not= repo (:active-repo db))) {}
         (not anc) {:db (dissoc db :nav-pending)
                    :dispatch [:toast/show {:kind :error :text (str path " is not under any source path")}]}
         (not data)
         {:db (update-in db [:expanded repo] (fnil into #{}) (conj (:ids anc) (:file-id anc)))
          :dispatch [:file/load repo path]}
         :else
         (let [form (or (some #(when (and row (<= (:row %) row (:end-row %))) %) (:forms data))
                        (first (:forms data)))
               id (if form [:form (:pkey anc) path (:index form)] (:file-id anc))]
           (persist-expanded! repo (into (get-in db [:expanded repo] #{}) (conj (:ids anc) (:file-id anc))))
           {:db (-> db
                    (dissoc :nav-pending)
                    (update-in [:expanded repo] (fnil into #{}) (conj (:ids anc) (:file-id anc)))
                    (assoc-in [:selected repo] id))
            :scroll-to id})))
     {})))

(rf/reg-fx
 :scroll-to
 (fn [id]
   (js/setTimeout
    (fn [] (some-> (js/document.querySelector (str "[data-row-id='" (pr-str id) "']"))
                   (.scrollIntoView #js {:block "center" :behavior "smooth"})))
    60)))

;; ---------------------------------------------------------------------------
;; views & panels

(rf/reg-event-db :view/set (fn [db [_ v]] (assoc db :view v)))
(rf/reg-event-db :panel/set (fn [db [_ p]] (assoc db :panel (when (not= p (:panel db)) p))))
(rf/reg-event-db :panel/open (fn [db [_ p]] (assoc db :panel p)))
(rf/reg-event-db :modal/close (fn [db _] (assoc db :modal nil)))
(rf/reg-event-db :modal/open (fn [db [_ m]] (assoc db :modal m)))

;; ---------------------------------------------------------------------------
;; config

(rf/reg-event-fx
 :config/set
 (fn [{:keys [db]} [_ partial]]
   {:db (update db :config merge partial)
    :theme/apply (get partial :theme (get-in db [:config :theme]))
    :ws {:op :config/set :params {:config partial} :on-ok [:config/saved]}}))

(rf/reg-event-db :config/saved (fn [db [_ cfg]] (assoc db :config cfg)))

;; ---------------------------------------------------------------------------
;; docstring editing

(rf/reg-event-db :edit/start (fn [db [_ e]] (assoc db :editing e)))
(rf/reg-event-db :edit/cancel (fn [db _] (assoc db :editing nil)))

(rf/reg-event-fx
 :doc/save
 (fn [{:keys [db]} [_ {:keys [repo path form doc]}]]
   (let [data (get-in db [:files [repo path]])]
     {:db (assoc db :editing nil)
      :ws {:op :doc/set
           :params {:repo repo :path path :pos (select-keys form [:row :col]) :doc doc :sha (:sha data)}
           :on-ok [:edit/saved repo path "Docstring saved"]}})))

(rf/reg-event-fx
 :form/save-source
 (fn [{:keys [db]} [_ {:keys [repo path form text]}]]
   (let [data (get-in db [:files [repo path]])]
     {:ws {:op :form/replace
           :params {:repo repo :path path :pos (select-keys form [:row :col]) :text text :sha (:sha data)}
           :on-ok [:edit/saved repo path "Form saved"]}})))

(rf/reg-event-fx
 :edit/saved
 (fn [{:keys [db]} [_ repo path msg {:keys [ok file message]}]]
   {:db (cond-> db
          file (assoc-in [:files [repo path]] file)
          ok (assoc :editing nil))
    :dispatch [:toast/show (if ok
                             {:kind :ok :text msg :action {:label "Undo" :event [:edit/undo repo path]}}
                             {:kind :error :text message})]}))

(rf/reg-event-fx
 :edit/undo
 (fn [_ [_ repo path]]
   {:ws {:op :edit/undo :params {:repo repo :path path} :on-ok [:edit/undone repo path]}}))

(rf/reg-event-fx
 :edit/undone
 (fn [{:keys [db]} [_ repo path {:keys [ok file message]}]]
   {:db (cond-> db file (assoc-in [:files [repo path]] file))
    :dispatch [:toast/show {:kind (if ok :ok :error) :text (if ok "Undone" message)}]}))

;; ---------------------------------------------------------------------------
;; copy / paste

(rf/reg-event-fx
 :form/copy
 (fn [_ [_ {:keys [repo path form file-data]}]]
   {:ws {:op :form/source :params {:repo repo :path path :pos (select-keys form [:row :col :end-row :end-col])}
         :on-ok [:form/copied {:repo repo :path path :form form :ns (:ns file-data)}]}}))

(rf/reg-fx :clipboard/write (fn [text] (some-> js/navigator.clipboard (.writeText text))))

(rf/reg-event-fx
 :form/copied
 (fn [{:keys [db]} [_ {:keys [repo path form ns]} {:keys [text]}]]
   {:db (assoc db :clipboard {:repo repo :path path :ns ns :text text :hash (:hash form)
                              :name (:name form) :pos (select-keys form [:row :col])})
    :clipboard/write text
    :dispatch [:toast/show {:kind :ok :text (str "Copied " (:name form) " — select a file or form and press v to paste")}]}))

(rf/reg-event-fx
 :form/paste
 (fn [{:keys [db]} [_ {:keys [repo path after]}]]
   (if-let [clip (:clipboard db)]
     {:ws {:op :paste/plan
           :params {:source clip :target {:repo repo :path path :after after}}
           :on-ok [:paste/planned {:repo repo :path path :after after}]}}
     {:dispatch [:toast/show {:kind :error :text "Nothing copied yet (press c on a form)"}]})))

(rf/reg-event-db
 :paste/planned
 (fn [db [_ target plan]]
   (assoc db :modal {:type :paste :target target :plan plan :clip (:clipboard db)
                     :selected (set (range (count (:requires plan))))})))

(rf/reg-event-db
 :paste/toggle-require
 (fn [db [_ i]] (update-in db [:modal :selected] #(if (contains? % i) (disj % i) (conj % i)))))

(rf/reg-event-fx
 :paste/apply
 (fn [{:keys [db]} _]
   (let [{:keys [target plan clip selected]} (:modal db)
         reqs (vec (keep-indexed (fn [i r] (when (contains? selected i) r)) (:requires plan)))]
     {:db (assoc db :modal nil)
      :ws {:op :paste/apply
           :params {:target target :text (:text clip) :requires reqs :sha (:sha plan)}
           :on-ok [:edit/saved (:repo target) (:path target) (str "Pasted " (:name clip))]}})))

;; ---------------------------------------------------------------------------
;; usages panel

(rf/reg-event-fx
 :usages/show
 (fn [{:keys [db]} [_ {:keys [fqn repo]}]]
   {:db (assoc db :usages {:fqn fqn :repo repo :loading? true} :panel :usages)
    :ws {:op :form/usages :params {:fqn fqn :repo repo} :on-ok [:usages/loaded fqn]}}))

(rf/reg-event-db
 :usages/loaded
 (fn [db [_ fqn rows]]
   (if (= fqn (get-in db [:usages :fqn]))
     (update db :usages assoc :rows rows :loading? false)
     db)))

;; ---------------------------------------------------------------------------
;; external editor

(rf/reg-event-fx
 :file/open-external
 (fn [_ [_ {:keys [repo path line]}]]
   {:ws {:op :file/open-external :params {:repo repo :path path :line line} :on-ok [:noop]}}))

(rf/reg-event-db :noop (fn [db _] db))

;; ---------------------------------------------------------------------------
;; palette

(rf/reg-event-db :palette/open (fn [db _] (assoc db :palette {:q "" :results nil :sel 0})))
(rf/reg-event-db :palette/close (fn [db _] (assoc db :palette nil)))

(rf/reg-event-fx
 :palette/query
 (fn [{:keys [db]} [_ q]]
   {:db (update db :palette assoc :q q :sel 0)
    :ws (when-not (str/blank? q) {:op :search :params {:q q :limit 40} :on-ok [:palette/results q]})}))

(rf/reg-event-db
 :palette/results
 (fn [db [_ q res]]
   (if (= q (get-in db [:palette :q])) (assoc-in db [:palette :results] res) db)))

(rf/reg-event-db :palette/move (fn [db [_ d n]] (update-in db [:palette :sel] #(max 0 (min (dec n) (+ % d))))))

;; ---------------------------------------------------------------------------
;; settings for the current repo

(rf/reg-event-fx
 :repo/update
 (fn [_ [_ path settings]]
   {:ws {:op :repos/update :params {:path path :settings settings} :on-ok [:noop]}}))
