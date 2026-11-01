(ns clj-browse.watch
  "Keeps the index fresh without user action:
   * beholder watches each repo's source dirs → :files jobs (debounced)
   * a 2s poller watches HEAD, refs, packed-refs, project files and the
     clj-kondo config → :history / :worktree jobs."
  (:require [babashka.fs :as fs]
            [clj-browse.db :as db]
            [clj-browse.git :as git]
            [clj-browse.index :as index]
            [clj-browse.jobs :as jobs]
            [clj-browse.projects :as projects]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [datalevin.core :as d]
            [nextjournal.beholder :as beholder]))

(defonce ^:private watchers (atom {}))   ; repo -> {:beholder w :dirs [...]}
(defonce ^:private poll-state (atom {})) ; repo -> fingerprint map
(defonce ^:private poller (atom nil))
(defonce ^:private debounce (atom {}))   ; repo -> {:paths #{} :future f}

(def debounce-ms 250)

(defn- flush-files! [repo]
  (let [{:keys [paths]} (get @debounce repo)]
    (swap! debounce dissoc repo)
    (when (seq paths)
      (jobs/enqueue! {:type :files :repo repo :paths (vec paths)}))))

(defn- on-file-event [repo {:keys [path]}]
  (let [rel (str (fs/relativize repo path))]
    (when (and (projects/clojure-file? rel)
               (not (some projects/skip-dir? (str/split rel #"/"))))
      (swap! debounce (fn [m]
                        (some-> (get-in m [repo :future]) future-cancel)
                        (-> m
                            (update-in [repo :paths] (fnil conj #{}) rel)
                            (assoc-in [repo :future]
                                      (future (Thread/sleep debounce-ms) (flush-files! repo)))))))))

(defn- source-dirs [repo]
  (let [rid (index/repo-eid repo)]
    (->> (d/q '[:find [?paths ...] :in $ ?r :where [?p :project/repo ?r] [?p :project/paths ?paths]]
              (db/db) rid)
         (mapcat #(map :path/dir %))
         distinct
         (map #(str (fs/path repo %)))
         (filter fs/directory?)
         sort vec)))

(defn- watch-dirs! [repo]
  (let [dirs (source-dirs repo)
        old (get @watchers repo)]
    (when (not= dirs (:dirs old))
      (some-> (:beholder old) beholder/stop)
      (swap! watchers assoc repo
             {:dirs dirs
              :beholder (when (seq dirs)
                          (try (apply beholder/watch #(on-file-event repo %) dirs)
                               (catch Exception e (log/warn e "watch failed" repo) nil)))}))))

(defn- mtime [p] (when (fs/exists? p) (.toMillis (fs/last-modified-time p))))

(defn- fingerprint [repo]
  (let [gd (or (git/git-dir repo) (str (fs/path repo ".git")))
        ref-files (when (fs/directory? (fs/path gd "refs"))
                    (->> (fs/glob (fs/path gd "refs") "**") (filter fs/regular-file?)))]
    {:git (into {} (map (fn [p] [(str p) (mtime p)]))
                (concat [(fs/path gd "HEAD") (fs/path gd "packed-refs")] ref-files))
     :projects (->> (d/q '[:find [?f ...] :in $ ?r :where [?p :project/repo ?r] [?p :project/file ?f]]
                         (db/db) (index/repo-eid repo))
                    (map (fn [f] [f (mtime (fs/path repo f))]))
                    (into {}))
     :root-projects (into {} (for [f ["deps.edn" "project.clj" "shadow-cljs.edn" "bb.edn"]]
                               [f (mtime (fs/path repo f))]))
     :kondo (when-let [d (index/kondo-config-dir repo)]
              (into {} (map (fn [p] [(str p) (mtime p)]))
                    (remove #(str/includes? (str %) ".cache") (fs/glob d "**.{edn,clj,cljc,bb}"))))}))

(defn- poll-repo! [repo]
  (when (fs/directory? repo)
    (let [fp (fingerprint repo)
          old (get @poll-state repo)]
      (swap! poll-state assoc repo fp)
      (when old
        (when (not= (:git fp) (:git old))
          (jobs/enqueue! {:type :history :repo repo}))
        (when (or (not= (:projects fp) (:projects old))
                  (not= (:root-projects fp) (:root-projects old))
                  (not= (:kondo fp) (:kondo old)))
          (jobs/enqueue! {:type :worktree :repo repo})))
      (watch-dirs! repo))))

(defn all-repos [] (d/q '[:find [?p ...] :where [_ :repo/path ?p]] (db/db)))

(defn unwatch! [repo]
  (some-> (get-in @watchers [repo :beholder]) beholder/stop)
  (swap! watchers dissoc repo)
  (swap! poll-state dissoc repo))

(defn start! []
  (when-not @poller
    (reset! poller
            (future
              (loop []
                (doseq [r (all-repos)]
                  (try (poll-repo! r) (catch Exception e (log/warn e "poll failed" r))))
                (doseq [r (keys @watchers) :when (not (index/repo-eid r))] (unwatch! r))
                (Thread/sleep 2000)
                (when @poller (recur)))))))

(defn stop! []
  (when-let [p @poller] (reset! poller nil) (future-cancel p))
  (doseq [r (keys @watchers)] (unwatch! r)))
