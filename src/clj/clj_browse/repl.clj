(ns clj-browse.repl
  "nREPL manager: one REPL connection per repo (keyed by absolute repo path).

  A connection is either *started* by us (`start!`, we own the process and
  kill its process tree on `disconnect!`) or *connected* to an existing server
  (`connect!`, typically via `.nrepl-port` / `.shadow-cljs/nrepl.port`).

  Transport: `nrepl.core/connect` gives us a bencode transport. Instead of the
  lazy-seq based `nrepl.core/client` (which ends response seqs on read timeouts
  and cannot route output that arrives after `done`), each connection runs one
  reader thread that dispatches responses by message id to per-request queues.
  Messages with no pending request (e.g. output from futures after an eval
  finished) go straight to `on-output`. Evaluations on the same session are
  serialized with a per-session lock (so the `*e` lookup after an error always
  sees the right exception); `interrupt!` bypasses the lock.

  Each connection has a clj session and, for shadow-cljs, optionally a cljs
  session created by `select-cljs-build!`."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edamame.core :as e]
            [nrepl.core :as nrepl]
            [nrepl.transport :as transport])
  (:import (java.util UUID)
           (java.util.concurrent ConcurrentHashMap LinkedBlockingQueue TimeUnit)
           (java.util.concurrent.locks ReentrantLock)))

(set! *warn-on-reflection* true)

(defonce ^{:doc "repo-path string -> connection map. Connection maps hold
  mutable parts (transport, process, pending ConcurrentHashMap, locks)."}
  connections (atom {}))

(def default-opts
  {:nrepl-version "1.8.0"
   :cider-nrepl-version "0.64.0"
   :middleware ["cider.nrepl/cider-middleware"]})

;; ---------------------------------------------------------------------------
;; helpers

(defn- repo-key [repo-path] (str (fs/absolutize (fs/normalize repo-path))))

(defn- uuid [] (str (UUID/randomUUID)))

(defn- read-port-file [f]
  (when (fs/regular-file? f)
    (try (let [s (str/trim (slurp (str f)))]
           (when (re-matches #"\d+" s) (parse-long s)))
         (catch Exception _ nil))))

(defn- port-files [dir]
  [(fs/path dir ".nrepl-port") (fs/path dir ".shadow-cljs" "nrepl.port")])

(defn- port-in-dir [dir] (some read-port-file (port-files dir)))

(defn detect-port
  "Port from `<repo>/.nrepl-port` or `<repo>/.shadow-cljs/nrepl.port`, or nil."
  [repo-path]
  (port-in-dir repo-path))

;; ---------------------------------------------------------------------------
;; start command (pure)

(defn- project-dir [repo-path project]
  (let [d (:project/dir project)]
    (str (fs/normalize (fs/absolutize (if (str/blank? d) (fs/path repo-path) (fs/path repo-path d)))))))

(defn- deps-command [{:keys [aliases nrepl-version cider-nrepl-version middleware]}]
  (let [deps (cond-> {}
               nrepl-version (assoc 'nrepl/nrepl {:mvn/version nrepl-version})
               cider-nrepl-version (assoc 'cider/cider-nrepl {:mvn/version cider-nrepl-version}))
        aliases (map #(str/replace (name %) #"^:" "") aliases)]
    (cond-> ["clojure"]
      (seq deps) (into ["-Sdeps" (binding [*print-namespace-maps* false] (pr-str {:deps deps}))])
      true (conj (apply str "-M" (map #(str ":" %) aliases)))
      true (into ["-m" "nrepl.cmdline"])
      (seq middleware) (into ["--middleware" (str "[" (str/join " " middleware) "]")])
      true (into ["--port" "0"]))))

(defn- lein-command
  "lein: inject deps with chained `update-in ... --` (like CIDER jack-in):
   - nrepl/nrepl into :dependencies
   - cider/cider-nrepl as a lein plugin (the plugin adds the cider middleware)
   - any non-cider middleware into [:repl-options :nrepl-middleware]
   then `with-profile +a,+b repl :headless` (or just `repl :headless`)."
  [{:keys [aliases nrepl-version cider-nrepl-version middleware]}]
  (let [extra-mw (remove #(str/starts-with? % "cider.") middleware)
        profiles (map #(str "+" (str/replace (name %) #"^:" "")) aliases)]
    (cond-> ["lein"]
      nrepl-version
      (into ["update-in" ":dependencies" "conj" (pr-str ['nrepl/nrepl nrepl-version]) "--"])
      cider-nrepl-version
      (into ["update-in" ":plugins" "conj" (pr-str ['cider/cider-nrepl cider-nrepl-version]) "--"])
      (seq extra-mw)
      (into ["update-in" ":repl-options" "assoc" ":nrepl-middleware"
             (str "[" (str/join " " extra-mw) "]") "--"])
      (seq profiles) (into ["with-profile" (str/join "," profiles)])
      true (into ["repl" ":headless"]))))

(defn- shadow-command
  "shadow-cljs: `npx shadow-cljs [-d nrepl/nrepl:V] [-d cider/cider-nrepl:V] server`.
   The -d flags are how CIDER injects deps; shadow picks up cider middleware
   automatically when it is on the classpath. (They are ignored by shadow when
   it delegates deps to deps.edn/lein.) Aliases are not used."
  [{:keys [nrepl-version cider-nrepl-version]}]
  (cond-> ["npx" "shadow-cljs"]
    nrepl-version (into ["-d" (str "nrepl/nrepl:" nrepl-version)])
    cider-nrepl-version (into ["-d" (str "cider/cider-nrepl:" cider-nrepl-version)])
    true (conj "server")))

(defn start-command
  "Build the command (pure) to start an nREPL for `project` in `repo-path`.
   project keys: :project/type (:deps :lein :shadow), :project/dir (repo-relative, \"\" = root).
   opts: {:aliases [\"dev\"] :nrepl-version \"1.8.0\" :cider-nrepl-version \"0.64.0\"
          :middleware [\"cider.nrepl/cider-middleware\"]}; missing version/middleware keys
   default to `default-opts` (pass nil explicitly to omit).
   :deps   -> clojure -Sdeps '{:deps {...}}' -M:a:b -m nrepl.cmdline --middleware '[...]' --port 0
   :lein   -> see `lein-command`
   :shadow -> see `shadow-command`
   Returns {:cmd [...] :dir abs-dir}."
  [repo-path project opts]
  (let [opts (merge default-opts opts)]
    {:cmd (case (:project/type project)
            :deps (deps-command opts)
            :lein (lein-command opts)
            :shadow (shadow-command opts))
     :dir (project-dir repo-path project)}))

;; ---------------------------------------------------------------------------
;; status / callbacks

(defn- status-of [conn]
  (when conn
    (cond-> {:repo (:repo conn)
             :port (:port conn)
             :owned? (boolean (:owned? conn))
             :type (:type conn)
             :status (:status conn)
             :cljs-builds (vec (:cljs-builds conn))
             :cljs-session? (boolean (:cljs-session conn))}
      (:cljs-build conn) (assoc :cljs-build (:cljs-build conn))
      (:error conn) (assoc :error (:error conn))
      (:ops conn) (assoc :cider? (contains? (:ops conn) "info")))))

(defn status
  "nil or {:repo :port :owned? :type :status (:starting :connected :error)
   :cljs-builds [..] :cljs-session? bool [:cljs-build :error :cider?]}."
  [repo-path]
  (status-of (get @connections (repo-key repo-path))))

(defn- safe-call [f & args]
  (when f (try (apply f args) (catch Throwable _ nil))))

(defn- emit-output [conn stream text]
  (when (seq text)
    (safe-call (get-in conn [:callbacks :on-output]) {:repo (:repo conn) :stream stream :text text})))

(defn- update-conn!
  "Apply f to the connection for repo (only if it is still the same connection,
   identified by :conn-id) and fire on-status when the status map changed."
  [repo conn-id f & args]
  (let [[old new] (swap-vals! connections
                              (fn [m]
                                (if (= conn-id (get-in m [repo :conn-id]))
                                  (update m repo #(apply f % args))
                                  m)))
        o (status-of (get old repo)) n (status-of (get new repo))]
    (when (and n (not= o n))
      (safe-call (get-in new [repo :callbacks :on-status]) n))
    (get new repo)))

(defn- conn! [repo-path]
  (let [conn (get @connections (repo-key repo-path))]
    (when-not (and conn (:transport conn))
      (throw (ex-info "No REPL connected" {:repo (repo-key repo-path)})))
    conn))

;; ---------------------------------------------------------------------------
;; transport / dispatch

(defn- reader-loop [repo conn-id t ^ConcurrentHashMap pending closing]
  (try
    (loop []
      (when-not @closing
        (when-let [msg (transport/recv t 500)]
          (if-let [^LinkedBlockingQueue q (some->> (:id msg) (.get pending))]
            (.put q msg)
            (let [conn (get @connections repo)]
              (when (= conn-id (:conn-id conn))
                (emit-output conn :out (:out msg))
                (emit-output conn :err (:err msg))))))
        (recur)))
    (catch Throwable ex
      (when-not @closing
        (update-conn! repo conn-id assoc :status :error
                      :error (str "Connection lost: " (or (ex-message ex) (class ex)))))))
  (doseq [^LinkedBlockingQueue q (vals pending)]
    (.put q {::closed true})))

(declare interrupt-eval!)

(defn- done? [m] (some #{"done"} (:status m)))

(defn- request!
  "Send msg (an :id is added unless present) and collect responses until status
   done. Calls (on-msg m) for each response. Returns {:responses [...] :id id}
   or {:timed-out? true ...}. With :interrupt? true, a timed-out request is
   interrupted and its remaining responses drained (max 5s) so the session is
   free again. Throws if the connection closes."
  [conn msg {:keys [timeout-ms on-msg interrupt?] :or {timeout-ms 30000}}]
  (let [id (or (:id msg) (uuid))
        q (LinkedBlockingQueue.)
        ^ConcurrentHashMap pending (:pending conn)
        collect (fn [acc deadline]
                  (loop [acc acc]
                    (let [remaining (- deadline (System/currentTimeMillis))
                          m (when (pos? remaining) (.poll q remaining TimeUnit/MILLISECONDS))]
                      (cond
                        (nil? m) {:timed-out? true :id id :responses acc}
                        (::closed m) (throw (ex-info "REPL connection closed" {:repo (:repo conn)}))
                        :else (do (when on-msg (on-msg m))
                                  (let [acc (conj acc m)]
                                    (if (done? m) {:id id :responses acc} (recur acc))))))))]
    (.put pending id q)
    (try
      (transport/send (:transport conn) (assoc msg :id id))
      (let [r (collect [] (+ (System/currentTimeMillis) timeout-ms))]
        (if (and (:timed-out? r) interrupt?)
          (do (interrupt-eval! conn (:session msg) id #(not-any? done? (seq q)))
              (assoc (collect (:responses r) (+ (System/currentTimeMillis) 5000)) :timed-out? true))
          r))
      (finally (.remove pending id)))))

(defn- send-only! [conn msg]
  (transport/send (:transport conn) (assoc msg :id (uuid))))

(defn- interrupt-eval!
  "Interrupt request `id` on `session`. nREPL answers session-idle /
   interrupt-id-mismatch when the eval has not started yet (it is queued), so
   retry (max ~3s) while (running?) is true."
  [conn session id running?]
  (loop [n 0]
    (let [st (try (->> (request! conn {:op "interrupt" :session session :interrupt-id id}
                                 {:timeout-ms 2000})
                       :responses (mapcat :status) set)
                  (catch Throwable _ #{}))]
      (if (and (some st ["session-idle" "interrupt-id-mismatch"]) (< n 30) (running?))
        (do (Thread/sleep 100) (recur (inc n)))
        (contains? st "done")))))

(defn- open-transport [port]
  (let [deadline (+ (System/currentTimeMillis) 10000)]
    (loop []
      (let [r (try (nrepl/connect :host "localhost" :port port)
                   (catch Exception ex ex))]
        (cond
          (not (instance? Exception r)) r
          (< (System/currentTimeMillis) deadline) (do (Thread/sleep 200) (recur))
          :else (throw (ex-info (str "Could not connect to nREPL on port " port) {:port port} r)))))))

(defn- shadow-dir? [dir] (fs/exists? (fs/path dir "shadow-cljs.edn")))

(defn- read-builds [dir]
  (let [f (fs/path dir "shadow-cljs.edn")]
    (when (fs/regular-file? f)
      (try (->> (e/parse-string (slurp (str f)) {:all true :readers (fn [_] identity)})
                :builds keys (map #(if (keyword? %) (subs (str %) 1) (str %))) sort vec)
           (catch Exception _ [])))))

(defn- attach!
  "Open transport to port, start reader, clone clj session, describe ops.
   Registers/updates the connection for repo and returns it."
  [repo conn-id port]
  (let [t (open-transport port)
        pending (ConcurrentHashMap.)
        closing (atom false)
        thread (doto (Thread. ^Runnable #(reader-loop repo conn-id t pending closing)
                              (str "nrepl-reader-" port))
                 (.setDaemon true) (.start))
        tmp {:repo repo :transport t :pending pending}]
    (try
      (let [session (->> (request! tmp {:op "clone"} {:timeout-ms 30000})
                         :responses (some :new-session))
            ops (->> (request! tmp {:op "describe"} {:timeout-ms 30000})
                     :responses (some :ops) keys (map name) set)]
        (when-not session (throw (ex-info "Could not create nREPL session" {:port port})))
        (or (update-conn! repo conn-id assoc
                          :port port :transport t :pending pending :closing closing
                          :reader thread :session session :ops ops
                          :status :connected :error nil)
            (throw (ex-info "Connection was removed while connecting" {:repo repo}))))
      (catch Throwable ex
        (reset! closing true)
        (.close ^java.io.Closeable t)
        (throw ex)))))

;; ---------------------------------------------------------------------------
;; lifecycle

(defn- kill-tree! [proc]
  (when proc
    (let [^Process jp (:proc proc)
          handles (cons (.toHandle jp) (iterator-seq (.iterator (.descendants (.toHandle jp)))))]
      (run! #(.destroy ^java.lang.ProcessHandle %) handles)
      (let [deadline (+ (System/currentTimeMillis) 10000)]
        (while (and (some #(.isAlive ^java.lang.ProcessHandle %) handles)
                    (< (System/currentTimeMillis) deadline))
          (Thread/sleep 100)))
      (run! #(when (.isAlive ^java.lang.ProcessHandle %) (.destroyForcibly ^java.lang.ProcessHandle %)) handles))))

(defn disconnect!
  "Close the transport; if we own the process, destroy its process tree.
   Idempotent. Calls on-status with {:repo :status :disconnected}."
  [repo-path]
  (let [repo (repo-key repo-path)
        [old _] (swap-vals! connections dissoc repo)]
    (when-let [conn (get old repo)]
      (when-let [t (:transport conn)]
        (when (= :connected (:status conn))
          (doseq [session (keep conn [:cljs-session :session])]
            (try (request! conn {:op "close" :session session} {:timeout-ms 1000})
                 (catch Throwable _ nil))))
        (some-> (:closing conn) (reset! true))
        (try (.close ^java.io.Closeable t) (catch Throwable _ nil)))
      (some-> (:closing conn) (reset! true))
      (when (:owned? conn) (kill-tree! (:process conn)))
      (safe-call (get-in conn [:callbacks :on-status]) {:repo repo :status :disconnected})
      nil)))

(defn stop-all-owned!
  "On app shutdown: disconnect! every connection (kills owned processes,
   leaves servers we only connected to running)."
  []
  (run! disconnect! (keys @connections)))

(defn- new-conn [repo type owned? callbacks]
  {:repo repo :conn-id (uuid) :type type :owned? owned? :status :starting
   :callbacks callbacks
   :current (atom {})                      ; session -> id of running request
   :locks {:clj (ReentrantLock.) :cljs (ReentrantLock.)}})

(defn connect!
  "Connect to an existing nREPL on `port` (default `detect-port`). owned? false.
   Returns the status map."
  ([repo-path] (connect! repo-path nil {}))
  ([repo-path port {:keys [on-output on-status]}]
   (let [repo (repo-key repo-path)
         port (or port (detect-port repo)
                  (throw (ex-info "No nREPL port file found" {:repo repo})))
         shadow? (or (fs/exists? (fs/path repo ".shadow-cljs" "nrepl.port")) (shadow-dir? repo))
         conn (assoc (new-conn repo (if shadow? :shadow :unknown) false
                               {:on-output on-output :on-status on-status})
                     :port port :dir repo
                     :cljs-builds (when shadow? (read-builds repo)))]
     (disconnect! repo)
     (swap! connections assoc repo conn)
     (try
       (status-of (attach! repo (:conn-id conn) port))
       (catch Throwable ex
         (swap! connections #(if (= (:conn-id conn) (get-in % [repo :conn-id])) (dissoc % repo) %))
         (throw ex))))))

(def ^:private started-re #"nREPL server started on port (\d+)")

(defn- pump! [conn-ref stream ^java.io.InputStream in port-promise]
  (doto (Thread. ^Runnable
         (fn []
           (try
             (with-open [r (io/reader in)]
               (doseq [line (line-seq r)]
                 (when-let [[_ n] (re-find started-re line)]
                   (deliver port-promise (parse-long n)))
                 (emit-output @conn-ref stream (str line "\n"))))
             (catch Throwable _ nil))))
    (.setDaemon true) (.start)))

(defn start!
  "Start a REPL process for repo using `start-command`, stream its stdout/stderr
   lines to (on-output {:repo :stream :out|:err :text s}), wait until the port is
   known (\"nREPL server started on port N\" on stdout, or a new/changed
   .nrepl-port / .shadow-cljs/nrepl.port in the project dir; opts :timeout-ms
   default 180000), then connect. Returns the status map (owned? true).
   Throws ex-info on failure/timeout (and kills the process)."
  [repo-path project opts {:keys [on-output on-status]}]
  (let [repo (repo-key repo-path)
        {:keys [cmd dir]} (start-command repo project opts)
        timeout-ms (or (:timeout-ms opts) 180000)
        type (:project/type project)
        snapshot (fn [] (into {} (map (fn [f] [f (when (fs/exists? f)
                                                     [(fs/last-modified-time f) (read-port-file f)])])
                                      (port-files dir))))
        before (snapshot)
        conn (assoc (new-conn repo type true {:on-output on-output :on-status on-status})
                    :dir dir :cmd cmd
                    :cljs-builds (when (= :shadow type) (read-builds dir)))
        conn-id (:conn-id conn)
        _ (disconnect! repo)
        _ (swap! connections assoc repo conn)
        _ (safe-call on-status (status-of conn))
        fail! (fn [proc msg data cause]
                (kill-tree! proc)
                (update-conn! repo conn-id assoc :status :error :error msg)
                (swap! connections #(if (= conn-id (get-in % [repo :conn-id])) (dissoc % repo) %))
                (throw (ex-info msg (merge {:repo repo :cmd cmd} data) cause)))
        proc (try (p/process {:cmd cmd :dir dir :in :pipe :out :pipe :err :pipe})
                  (catch Throwable ex (fail! nil (str "Could not start: " (ex-message ex)) {} ex)))
        port-p (promise)
        conn-ref (delay (or (get @connections repo) conn))]
    (swap! connections #(if (= conn-id (get-in % [repo :conn-id])) (assoc-in % [repo :process] proc) %))
    (.close ^java.io.OutputStream (:in proc))
    (pump! conn-ref :out (:out proc) port-p)
    (pump! conn-ref :err (:err proc) (promise))
    (let [deadline (+ (System/currentTimeMillis) timeout-ms)
          port (loop []
                 (let [file-port (some (fn [[f v]]
                                         (when (and (fs/exists? f)
                                                    (not= v [(fs/last-modified-time f) (read-port-file f)]))
                                           (read-port-file f)))
                                       before)
                       port (or (deref port-p 0 nil) file-port)]
                   (cond
                     port port
                     (not= conn-id (get-in @connections [repo :conn-id])) ::cancelled
                     (not (p/alive? proc)) ::exited
                     (> (System/currentTimeMillis) deadline) ::timeout
                     :else (do (Thread/sleep 100) (recur)))))]
      (case port
        ::cancelled (do (kill-tree! proc) (throw (ex-info "REPL start cancelled" {:repo repo})))
        ::exited (fail! proc (str "REPL process exited with code " (.exitValue ^Process (:proc proc))) {} nil)
        ::timeout (fail! proc (str "Timed out waiting for nREPL port after " timeout-ms "ms") {} nil)
        (try (status-of (attach! repo conn-id port))
             (catch Throwable ex
               (fail! proc (str "Could not connect: " (ex-message ex)) {:port port} ex)))))))

;; ---------------------------------------------------------------------------
;; evaluation

(defn- session-for [conn lang]
  (if (= :cljs lang)
    (or (:cljs-session conn)
        (throw (ex-info "No cljs session; select a cljs build first" {:repo (:repo conn)})))
    (:session conn)))

(defn- lock-for [conn lang]
  (get-in conn [:locks lang]))

(def ^:private ex-form
  "(clojure.core/let [e *e
                      e (if (clojure.core/and (clojure.core/instance? clojure.lang.Compiler$CompilerException e)
                                              (.getCause ^Throwable e))
                          (.getCause ^Throwable e) e)]
     {:class (.getName (clojure.core/class e))
      :message (clojure.core/ex-message e)
      :data (clojure.core/pr-str (clojure.core/ex-data e))})")

(defn- fetch-ex [conn session]
  (try
    (let [{:keys [responses]} (request! conn {:op "eval" :code ex-form :session session}
                                        {:timeout-ms 10000})
          v (some :value responses)
          m (when v (edn/read-string v))]
      (when (map? m)
        {:class (:class m) :message (:message m) :data-str (:data m)}))
    (catch Throwable _ nil)))

(defn- run-request!
  "Shared eval / load-file implementation."
  [repo-path msg {:keys [lang timeout-ms] :or {lang :clj timeout-ms 120000}}]
  (let [conn (conn! repo-path)
        session (session-for conn lang)
        cider? (contains? (:ops conn) "info")
        msg (cond-> (assoc msg :session session)
              (and cider? (= :clj lang)) (assoc :nrepl.middleware.print/print "cider.nrepl.pprint/pprint"))
        out (StringBuilder.) err (StringBuilder.)
        value (volatile! nil) ex-v (volatile! nil)
        on-msg (fn [m]
                 (when-let [o (:out m)] (.append out ^String o) (emit-output conn :out o))
                 (when-let [o (:err m)] (.append err ^String o) (emit-output conn :err o))
                 (when (contains? m :value) (vreset! value (:value m)))
                 (when (or (:ex m) (some #{"eval-error"} (:status m)))
                   (vreset! ex-v (or @ex-v {:class (:ex m)}))))
        ^ReentrantLock lock (lock-for conn lang)]
    (.lock lock)
    (try
      (let [current (:current conn)
            id (uuid)
            _ (swap! current assoc session id)
            r (try (request! conn (assoc msg :id id)
                             {:timeout-ms timeout-ms :on-msg on-msg :interrupt? true})
                   (finally (swap! current dissoc session)))
            interrupted? (some #(some #{"interrupted"} (:status %)) (:responses r))
            ex (cond
                 (:timed-out? r) {:message "Timed out"}
                 interrupted? {:message "Interrupted"}
                 @ex-v (let [fallback {:class (:class @ex-v) :message (str/trim (str err)) :data-str nil}]
                         (or (when (= :clj lang) (fetch-ex conn session)) fallback))
                 :else nil)]
        {:value (when-not ex (some-> @value str/trim-newline))
         :out (str out)
         :err (str err)
         :ex ex})
      (finally (.unlock lock)))))

(defn eval!
  "Evaluate a code string. opts {:ns \"my.ns\" :lang :clj|:cljs :file \"abs path\"
   :line n :column n :timeout-ms 120000}.
   Returns {:value \"<last value, pretty printed when cider-nrepl is present>\"
            :out \"...\" :err \"...\" :ex nil | {:class :message :data-str}}.
   :out/:err are also streamed to on-output. On timeout the eval is interrupted
   and {:ex {:message \"Timed out\"}} is returned."
  ([repo-path code] (eval! repo-path code {}))
  ([repo-path code {:keys [ns file line column] :as opts}]
   (run-request! repo-path
                 (cond-> {:op "eval" :code code}
                   ns (assoc :ns ns)
                   file (assoc :file file)
                   line (assoc :line line)
                   column (assoc :column column))
                 opts)))

(defn load-file!
  "nREPL load-file op for `abs-file-path` (content, :file-path, :file-name).
   Same return shape and opts (:lang :timeout-ms) as `eval!`."
  ([repo-path abs-file-path] (load-file! repo-path abs-file-path {}))
  ([repo-path abs-file-path opts]
   (let [f (fs/absolutize abs-file-path)]
     (run-request! repo-path
                   {:op "load-file" :file (slurp (str f))
                    :file-path (str f) :file-name (str (fs/file-name f))}
                   opts))))

(defn interrupt!
  "Interrupt the running evaluations (clj and cljs sessions) of repo."
  [repo-path]
  (when-let [conn (get @connections (repo-key repo-path))]
    (when (:transport conn)
      (let [current (:current conn)]
        (doseq [[session id] @current]
          (interrupt-eval! conn session id #(= id (get @current session))))))
    nil))

(defn complete
  "Completions for prefix in ns-name: [{:candidate :type :ns}], [] if unsupported.
   Uses cider `complete`, falling back to nREPL's builtin `completions`."
  [repo-path prefix ns-name]
  (try
    (let [conn (conn! repo-path)
          ops (:ops conn)
          op (cond (contains? ops "complete") "complete"
                   (contains? ops "completions") "completions")]
      (if-not op
        []
        (let [{:keys [responses]} (request! conn (cond-> {:op op :prefix prefix :session (:session conn)}
                                                   ns-name (assoc :ns ns-name))
                                            {:timeout-ms 10000})]
          (->> responses (mapcat :completions)
               (mapv #(select-keys % [:candidate :type :ns]))))))
    (catch Throwable _ [])))

(defn info
  "Symbol info: {:doc :arglists-str :ns :name :file :line} or nil.
   Uses cider `info`, falling back to nREPL's builtin `lookup`."
  [repo-path sym ns-name]
  (try
    (let [conn (conn! repo-path)
          ops (:ops conn)
          cider? (contains? ops "info")
          op (cond cider? "info" (contains? ops "lookup") "lookup")]
      (when op
        (let [{:keys [responses]} (request! conn (cond-> {:op op :sym (str sym) :session (:session conn)}
                                                   ns-name (assoc :ns ns-name))
                                            {:timeout-ms 10000})
              m (if cider? (first (filter :name responses)) (some :info responses))]
          (when (seq (:name m))
            (cond-> (select-keys m [:doc :arglists-str :ns :name :file :line])
              (and (:arglists m) (not (:arglists-str m))) (assoc :arglists-str (str (:arglists m)))
              (string? (:line m)) (update :line parse-long))))))
    (catch Throwable _ nil)))

(defn cljs-builds
  "Build ids (strings) from the project's shadow-cljs.edn :builds."
  [repo-path]
  (let [conn (get @connections (repo-key repo-path))]
    (or (read-builds (or (:dir conn) (repo-key repo-path))) [])))

(defn select-cljs-build!
  "Create (or replace) the cljs session: clone a session and evaluate
   (shadow.cljs.devtools.api/repl :build-id) in it. Returns the status map.
   Throws ex-info if switching to the build fails."
  [repo-path build-id]
  (let [conn (conn! repo-path)
        repo (:repo conn)
        new-session (->> (request! conn {:op "clone"} {:timeout-ms 30000}) :responses (some :new-session))
        _ (when-not new-session (throw (ex-info "Could not clone session" {:repo repo})))
        r (request! conn {:op "eval" :session new-session
                          :code (str "(shadow.cljs.devtools.api/repl " (keyword (str/replace (str build-id) #"^:" "")) ")")}
                    {:timeout-ms 60000})
        failed? (or (:timed-out? r)
                    (some #(or (:ex %) (some #{"eval-error"} (:status %))) (:responses r)))]
    (if failed?
      (do (try (send-only! conn {:op "close" :session new-session}) (catch Throwable _ nil))
          (throw (ex-info (str "Could not select cljs build " build-id ": "
                               (str/trim (apply str (keep :err (:responses r)))))
                          {:repo repo :build build-id})))
      (let [old (:cljs-session conn)]
        (when old (try (send-only! conn {:op "close" :session old}) (catch Throwable _ nil)))
        (status-of (update-conn! repo (:conn-id conn) assoc
                                 :cljs-session new-session :cljs-build (str build-id)))))))
