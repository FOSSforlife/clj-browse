(ns clj-browse.jobs
  "Single background worker for indexing. Jobs are deduplicated by key and
  prioritized: changed files < full worktree < refs/history."
  (:require [clj-browse.config :as config]
            [clj-browse.events :as events]
            [clj-browse.index :as index]
            [clj-browse.queries :as queries]
            [clojure.tools.logging :as log])
  (:import (java.util.concurrent PriorityBlockingQueue TimeUnit)))

(def priorities {:files 0 :worktree 1 :history 2})

(defonce ^:private queue
  (PriorityBlockingQueue. 64 (comparator (fn [a b] (< (compare [(:prio a) (:seq a)] [(:prio b) (:seq b)]) 0)))))
(defonce ^:private pending (atom {}))      ; job-key -> job (for merging :files jobs)
(defonce ^:private seq-no (atom 0))
(defonce ^:private worker (atom nil))
(defonce status (atom {:running nil :queued 0}))

(defn- job-key [{:keys [type repo]}] [type repo])

(defn- publish-status! []
  (swap! status assoc :queued (count @pending))
  (events/broadcast! [:index/status @status]))

(defn enqueue!
  "job: {:type :files|:worktree|:history :repo path :paths [...]}"
  [job]
  (let [k (job-key job)]
    (locking pending
      (if (contains? @pending k)
        (when (= :files (:type job))
          (swap! pending update-in [k :paths] (fnil into #{}) (:paths job)))
        (let [j (assoc job :prio (priorities (:type job)) :seq (swap! seq-no inc)
                       :paths (some-> (:paths job) set))]
          (swap! pending assoc k j)
          (.put queue j))))
    (publish-status!)))

(defn enqueue-full! [repo]
  (enqueue! {:type :worktree :repo repo})
  (enqueue! {:type :history :repo repo}))

(defn- run-job [{:keys [type repo] :as job}]
  (let [job (locking pending (let [j (get @pending (job-key job) job)]
                               (swap! pending dissoc (job-key job)) j))]
    (swap! status assoc :running (select-keys job [:type :repo]))
    (publish-status!)
    (try
      (when (index/repo-eid repo)
        (case type
          :files (index/index-worktree! repo (vec (:paths job)))
          :worktree (do (index/index-worktree! repo)
                        ;; repo-level facts (bb.edn presence, projects) may have changed
                        (events/broadcast! [:repos/changed (queries/repos)]))
          :history (index/index-history!
                    repo {:window (:commit-window (config/get-config))
                          :on-progress (fn [done total]
                                         (events/broadcast! [:index/progress {:repo repo :done done :total total}]))}))
        (queries/invalidate-search!))
      (catch Throwable e
        (log/error e "Index job failed" (select-keys job [:type :repo]))
        (events/broadcast! [:index/error {:repo repo :type type :message (ex-message e)}]))
      (finally
        (swap! status assoc :running nil)
        (publish-status!)))))

(defn start! []
  (when-not @worker
    (reset! worker
            (doto (Thread.
                   (fn []
                     (loop []
                       (when-let [j (try (.poll queue 1 TimeUnit/SECONDS) (catch InterruptedException _ ::stop))]
                         (when-not (= j ::stop) (run-job j)))
                       (when @worker (recur))))
                   "clj-browse-indexer")
              (.setDaemon true)
              (.start)))))

(defn stop! []
  (when-let [^Thread t @worker]
    (reset! worker nil)
    (.interrupt t)))

(defn idle? [] (and (zero? (count @pending)) (nil? (:running @status))))

(defn await-idle!
  "For tests/CLI: block until the queue drains."
  ([] (await-idle! 600000))
  ([timeout-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (cond (idle?) true
             (> (System/currentTimeMillis) deadline) false
             :else (do (Thread/sleep 50) (recur)))))))
