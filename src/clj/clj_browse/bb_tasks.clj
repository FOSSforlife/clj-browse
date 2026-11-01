(ns clj-browse.bb-tasks
  "Listing and running babashka tasks of a repo (bb.edn is read as data only,
  never evaluated)."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p])
  (:import (java.io InputStreamReader)
           (java.lang ProcessHandle)
           (java.nio.charset StandardCharsets)
           (java.util UUID)))

(set! *warn-on-reflection* true)

;; ---------------------------------------------------------------------------
;; Listing

(def ^:private special-keys #{"requires" "init" "enter" "leave"})

(defn- significant [nodes]
  (remove n/whitespace-or-comment? nodes))

(defn- node-key
  "Plain name of a keyword/symbol/string node, else nil."
  [node]
  (case (n/tag node)
    (:token :quote) (let [v (try (n/sexpr node) (catch Exception _ nil))]
                      (cond (keyword? v) (name v)
                            (symbol? v) (str v)
                            (string? v) v))
    :multi-line nil
    nil))

(defn- node-key-full
  "Like node-key but strings from :token nodes."
  [node]
  (or (node-key node)
      (when (string? (try (n/sexpr node) (catch Exception _ nil)))
        (n/sexpr node))))

(defn- map-pairs [map-node]
  (partition-all 2 (significant (n/children map-node))))

(defn- find-value [map-node key-name]
  (some (fn [[k v]] (when (= key-name (node-key k)) v)) (map-pairs map-node)))

(defn- task-doc [v]
  (when (and v (= :map (n/tag v)))
    (let [d (find-value v "doc")]
      (when (and d (= :token (n/tag d)))
        (let [s (try (n/sexpr d) (catch Exception _ nil))]
          (when (string? s) s))))))

(defn- task-private? [nm v]
  (or (str/starts-with? nm "-")
      (boolean
       (when (and v (= :map (n/tag v)))
         (let [pv (find-value v "private")]
           (and pv (= "true" (str/trim (n/string pv)))))))))

(defn list-tasks
  "Read <repo>/bb.edn. Returns nil if no bb.edn, else
  {:tasks [{:name :doc :private?}] :error nil}. Order as in the file."
  [repo-path]
  (let [f (fs/file repo-path "bb.edn")]
    (when (fs/exists? f)
      (try
        (let [forms (p/parse-string-all (slurp f))
              top   (first (filter #(= :map (n/tag %)) (significant (n/children forms))))
              tasks (when top (find-value top "tasks"))]
          {:tasks (if (and tasks (= :map (n/tag tasks)))
                    (vec (for [[k v] (map-pairs tasks)
                               :let [nm (node-key-full k)]
                               :when (and nm (not (special-keys nm)))]
                           {:name nm
                            :doc (task-doc v)
                            :private? (task-private? nm v)}))
                    [])
           :error nil})
        (catch Throwable e
          {:tasks [] :error (or (ex-message e) (str e))})))))

;; ---------------------------------------------------------------------------
;; Arg splitting

(defn split-args
  "Split a string shell-style, respecting single and double quotes and
  backslash escapes (outside single quotes)."
  [s]
  (let [s (or s "")
        n (count s)]
    (loop [i 0, cur nil, q nil, out []]
      (if (>= i n)
        (if cur (conj out (str cur)) out)
        (let [c (.charAt ^String s i)]
          (cond
            (= q \') (if (= c \')
                       (recur (inc i) cur nil out)
                       (recur (inc i) (str cur c) q out))
            (= q \") (cond
                       (= c \") (recur (inc i) cur nil out)
                       (and (= c \\) (< (inc i) n)
                            (#{\" \\ \$ \`} (.charAt ^String s (inc i))))
                       (recur (+ i 2) (str cur (.charAt ^String s (inc i))) q out)
                       :else (recur (inc i) (str cur c) q out))
            (= c \') (recur (inc i) (or cur "") \' out)
            (= c \") (recur (inc i) (or cur "") \" out)
            (and (= c \\) (< (inc i) n))
            (recur (+ i 2) (str cur (.charAt ^String s (inc i))) nil out)
            (Character/isWhitespace c)
            (recur (inc i) nil nil (if cur (conj out (str cur)) out))
            :else (recur (inc i) (str cur c) nil out)))))))

;; ---------------------------------------------------------------------------
;; Running

(def ^:private max-output-chars (* 2 1024 1024))

(defonce ^:private runs (atom {}))       ; id -> run map (running only)
(defonce ^:private last-runs (atom {}))  ; [repo task] -> state atom

(defn- append-bounded! [^StringBuilder sb ^String text]
  (locking sb
    (.append sb text)
    (let [over (- (.length sb) max-output-chars)]
      (when (pos? over) (.delete sb 0 (int over))))))

(defn- pump
  "Start a thread reading stream, calling (f text) on each chunk."
  ^Thread [^java.io.InputStream in f]
  (doto (Thread.
         ^Runnable
         (fn []
           (try
             (with-open [r (InputStreamReader. in StandardCharsets/UTF_8)]
               (let [buf (char-array 8192)]
                 (loop []
                   (let [k (.read r buf)]
                     (when (pos? k)
                       (f (String. buf 0 (int k)))
                       (recur))))))
             (catch Throwable _ nil))))
    (.setDaemon true)
    (.start)))

(defn run-task!
  "Start `bb <task> <args...>` in repo-path. Returns a run id string."
  [repo-path task-name args-str {:keys [on-output on-exit]}]
  (let [id      (str (UUID/randomUUID))
        repo    (str repo-path)
        args    (split-args args-str)
        pb      (doto (ProcessBuilder. ^java.util.List (into ["bb" (str task-name)] args))
                  (.directory (fs/file repo)))
        _       (doto (.environment pb)
                  (.put "FORCE_COLOR" "1"))
        sb      (StringBuilder.)
        started (System/currentTimeMillis)
        state   (atom {:id id :started-at started :running? true :exit nil
                       :finished-at nil :sb sb})
        proc    (try (.start pb)
                     (catch Throwable e
                       (append-bounded! sb (str "Failed to start bb: " (ex-message e) "\n"))
                       nil))]
    (swap! last-runs assoc [repo (str task-name)] state)
    (if-not proc
      (do (swap! state assoc :running? false :exit -1 :finished-at (System/currentTimeMillis))
          (when on-output (on-output {:stream :err :text (str sb)}))
          (when on-exit (on-exit -1))
          id)
      (let [^Process proc proc
            emit (fn [stream]
                   (fn [^String text]
                     (append-bounded! sb text)
                     (when on-output (try (on-output {:stream stream :text text})
                                          (catch Throwable _ nil)))))]
        (try (.close (.getOutputStream proc)) (catch Exception _ nil))
        (swap! runs assoc id {:id id :repo repo :task (str task-name)
                              :started-at started :proc proc})
        (let [t-out (pump (.getInputStream proc) (emit :out))
              t-err (pump (.getErrorStream proc) (emit :err))]
          (doto (Thread.
                 ^Runnable
                 (fn []
                   (let [code (.waitFor proc)]
                     (.join t-out 5000)
                     (.join t-err 5000)
                     (swap! runs dissoc id)
                     (swap! state assoc :running? false :exit code
                            :finished-at (System/currentTimeMillis))
                     (when on-exit (try (on-exit code) (catch Throwable _ nil))))))
            (.setDaemon true)
            (.start)))
        id))))

(defn stop!
  "Destroy the process tree of a run. Returns true if it was running."
  [run-id]
  (if-let [{:keys [^Process proc]} (get @runs run-id)]
    (let [ph (.toHandle proc)]
      (doseq [^ProcessHandle d (iterator-seq (.iterator (.descendants ph)))]
        (.destroyForcibly d))
      (.destroyForcibly proc)
      true)
    false))

(defn running
  "-> [{:id :repo :task :started-at}]"
  []
  (->> (vals @runs)
       (map #(select-keys % [:id :repo :task :started-at]))
       (sort-by :started-at)
       vec))

(defn last-output
  "-> {:id :text :exit :started-at :finished-at :running?} or nil"
  [repo-path task-name]
  (when-let [st (get @last-runs [(str repo-path) (str task-name)])]
    (let [{:keys [^StringBuilder sb] :as m} @st]
      (-> (dissoc m :sb)
          (assoc :text (locking sb (str sb)))))))
