(ns clj-browse.api.core
  "Repos, scan roots, outline, forms, usages, search, settings."
  (:require [babashka.fs :as fs]
            [clj-browse.api :refer [handle]]
            [clj-browse.config :as config]
            [clj-browse.db :as db]
            [clj-browse.events :as events]
            [clj-browse.gc :as gc]
            [clj-browse.index :as index]
            [clj-browse.jobs :as jobs]
            [clj-browse.projects :as projects]
            [clj-browse.queries :as queries]
            [clj-browse.watch :as watch]
            [datalevin.core :as d]))

(defn- repos-changed! [] (events/broadcast! [:repos/changed (queries/repos)]))

(defmethod handle :app/init [_ _ _]
  {:config (config/get-config)
   :repos (queries/repos)
   :roots (queries/scan-roots)
   :index @jobs/status
   :home (str (fs/home))})

(defmethod handle :repos/list [_ _ _] (queries/repos))

(defmethod handle :roots/list [_ _ _] (queries/scan-roots))

(defn- scan-root [path]
  (let [known (set (d/q '[:find [?p ...] :where [_ :repo/path ?p]] (db/db)))]
    (mapv (fn [p] {:path p :name (str (fs/file-name p)) :added? (contains? known p)})
          (projects/find-repos path))))

(defmethod handle :roots/scan [_ {:keys [path]} _]
  (let [path (str (fs/canonicalize (fs/expand-home path)))]
    (when-not (fs/directory? path) (throw (ex-info (str "Not a directory: " path) {})))
    (db/transact! [{:root/path path}])
    {:root path :found (scan-root path)}))

(defmethod handle :roots/remove [_ {:keys [path]} _]
  (db/transact! [[:db/retractEntity [:root/path path]]])
  (queries/scan-roots))

(defmethod handle :repos/add [_ {:keys [paths]} _]
  (let [added (vec (for [p paths
                         :let [p (str (fs/canonicalize (fs/expand-home p)))]]
                     (do (when-not (fs/exists? (fs/path p ".git"))
                           (throw (ex-info (str "Not a git repo: " p) {})))
                         (let [r (index/add-repo! p)]
                           (jobs/enqueue-full! r)
                           r))))]
    (repos-changed!)
    added))

(defmethod handle :repos/remove [_ {:keys [path]} _]
  (watch/unwatch! path)
  (gc/remove-repo! path)
  (queries/invalidate-search!)
  (repos-changed!)
  true)

(defmethod handle :repos/reorder [_ {:keys [paths]} _]
  (db/transact! (map-indexed (fn [i p] {:repo/path p :repo/position i}) paths))
  (repos-changed!)
  true)

(defmethod handle :repos/update [_ {:keys [path settings]} _]
  (let [allowed (select-keys settings [:repo/full-history? :repo/commit-window
                                       :repo/pinned-branches :repo/repl-config])
        rid (index/repo-eid path)]
    (when (contains? allowed :repo/pinned-branches)
      (db/transact! [[:db/retract rid :repo/pinned-branches]]))
    (db/transact! [(merge {:db/id rid} (index/strip-nils allowed))])
    (when (some #(contains? allowed %) [:repo/full-history? :repo/commit-window :repo/pinned-branches])
      (jobs/enqueue! {:type :history :repo path}))
    (repos-changed!)
    true))

(defmethod handle :repos/reindex [_ {:keys [path]} _]
  (jobs/enqueue-full! path) true)

(defmethod handle :repos/branches [_ {:keys [path]} _]
  {:refs (index/sync-refs! path)
   :commits (d/q '[:find (count ?c) . :in $ ?r :where [?c :commit/repos ?r] [?c :commit/indexed? true]]
                 (db/db) (index/repo-eid path))
   :boundary (d/q '[:find (count ?c) . :in $ ?r :where [?c :commit/repos ?r] [?c :commit/boundary? true]]
                  (db/db) (index/repo-eid path))})

(defmethod handle :outline/get [_ {:keys [repo]} _] (queries/outline repo))

(defmethod handle :file/forms [_ {:keys [repo path]} _] (queries/file-forms repo path))

(defmethod handle :file/text [_ {:keys [repo path]} _]
  {:text (queries/file-text repo path)})

(defmethod handle :form/source [_ {:keys [repo path pos]} _]
  {:text (queries/form-source repo path pos)})

(defmethod handle :form/usages [_ {:keys [fqn repo]} _] (queries/usages fqn repo))

(defmethod handle :var/definitions [_ {:keys [fqn]} _] (queries/definitions fqn))

(defmethod handle :search [_ {:keys [q limit]} _] (queries/search q {:limit (or limit 50)}))

(defmethod handle :index/status [_ _ _] @jobs/status)

(defmethod handle :config/get [_ _ _] (config/get-config))

(defmethod handle :config/set [_ {:keys [config]} _]
  (config/write! (config/deep-merge (config/read-config) config)))
