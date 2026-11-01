(ns clj-browse.main
  "CLI entry point.

    clj-browse serve [--host 127.0.0.1] [--port 7878] [--open] [--token T]
    clj-browse add <repo-dir>...          register repos and index them, then exit
    clj-browse scan <dir>                 list repos found under dir (doesn't add)"
  (:gen-class)
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clj-browse.api.all]
            [clj-browse.config :as config]
            [clj-browse.db :as db]
            [clj-browse.events :as events]
            [clj-browse.git :as git]
            [clj-browse.index :as index]
            [clj-browse.jobs :as jobs]
            [clj-browse.paths :as paths]
            [clj-browse.projects :as projects]
            [clj-browse.repl :as repl]
            [clj-browse.server :as server]
            [clj-browse.watch :as watch]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [datalevin.core :as d]))

(def cli-opts
  [[nil "--host HOST" "Interface to bind" :default nil]
   ["-p" "--port PORT" "Port, 0 = random (default from config.edn :server :port, 7878)" :parse-fn parse-long :default nil]
   [nil "--open" "Open the UI in a browser"]
   [nil "--token TOKEN" "Require ?token=TOKEN (for non-localhost access)"]
   [nil "--data-dir DIR" "Override config/data directory"]
   ["-h" "--help"]])

(defonce ^:private config-poller (atom nil))

(defn- watch-config! []
  (reset! config-poller
          (future
            (loop [last nil]
              (let [f (paths/config-file)
                    m (when (fs/exists? f) (.toMillis (fs/last-modified-time f)))]
                (when (and last (not= m last)) (config/reload!))
                (Thread/sleep 1500)
                (recur (or m 0)))))))

(defn start-system! [{:keys [host port token]}]
  (when-not (git/git-available?)
    (throw (ex-info "git was not found on PATH. clj-browse needs the git CLI." {})))
  (paths/ensure-dirs!)
  (config/reload!)
  (config/on-change! ::broadcast (fn [cfg] (events/broadcast! [:config/changed cfg])))
  (when token (swap! config/state assoc-in [:server :token] token))
  (db/open!)
  (jobs/start!)
  (doseq [r (d/q '[:find [?p ...] :where [_ :repo/path ?p]] (db/db))]
    (when (fs/directory? r) (jobs/enqueue-full! r)))
  (watch/start!)
  (watch-config!)
  (let [cfg (:server (config/get-config))
        port (server/start! {:host (or host (:host cfg)) :port (if (some? port) port (:port cfg))})]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable (fn []
                                           (try (repl/stop-all-owned!) (catch Throwable _))
                                           (server/stop!) (watch/stop!) (jobs/stop!) (db/close!))))
    port))

(defn- open-browser [url]
  (let [cmd (if (str/includes? (str/lower-case (System/getProperty "os.name")) "mac") "open" "xdg-open")]
    (try (p/process {:out :discard :err :discard} cmd url) (catch Exception _))))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-opts)
        [cmd & more] arguments]
    (when (:data-dir options) (alter-var-root #'paths/*home-override* (constantly (:data-dir options))))
    (cond
      (or errors (:help options))
      (do (println (str/join "\n" errors)) (println "Usage: clj-browse [serve|add|scan] [options]") (println summary))

      (= cmd "scan")
      (doseq [r (projects/find-repos (first more))] (println r))

      (= cmd "add")
      (do (paths/ensure-dirs!) (db/open!)
          (doseq [r more]
            (let [r (index/add-repo! r)]
              (println "Indexing" r)
              (index/index-repo! r {:window (:commit-window (config/get-config))})))
          (db/close!)
          (shutdown-agents))

      :else
      (let [port (start-system! options)
            host (or (:host options) (get-in (config/get-config) [:server :host]))
            url (str "http://" (if (= host "0.0.0.0") "localhost" host) ":" port
                     (when-let [t (:token options)] (str "/?token=" t)))]
        (println "clj-browse running at" url)
        (when (and (not (#{"127.0.0.1" "localhost" "::1"} host))
                   (str/blank? (get-in (config/get-config) [:server :token])))
          (println (str "WARNING: listening on " host " without a token. Anyone who can reach this port can "
                        "run code via the REPL and bb tasks. Use --token or set :server {:token ...} in config.edn.")))
        (when (:open options) (open-browser url))
        @(promise)))))
