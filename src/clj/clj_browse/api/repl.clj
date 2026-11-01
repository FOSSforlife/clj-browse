(ns clj-browse.api.repl
  "nREPL per repo + the Run modal backend."
  (:require [babashka.fs :as fs]
            [clj-browse.api :refer [handle]]
            [clj-browse.config :as config]
            [clj-browse.db :as db]
            [clj-browse.events :as events]
            [clj-browse.index :as index]
            [clj-browse.repl :as repl]
            [clojure.string :as str]
            [datalevin.core :as d]))

(defn- callbacks [repo]
  {:on-output (fn [{:keys [stream text]}]
                (events/broadcast! [:repl/output {:repo repo :stream stream :text text}]))
   :on-status (fn [st] (events/broadcast! [:repl/status (assoc st :repo repo)]))})

(defn- status* [repo]
  (or (repl/status repo) {:repo repo :status :disconnected :port (repl/detect-port repo)}))

(defmethod handle :repl/status [_ {:keys [repo]} _]
  (assoc (status* repo) :detected-port (repl/detect-port repo)))

(defmethod handle :repl/connect [_ {:keys [repo port]} _]
  (repl/connect! repo (or port (repl/detect-port repo)) (callbacks repo))
  (status* repo))

(defmethod handle :repl/start [_ {:keys [repo project-key aliases]} _]
  (let [project (d/pull (db/db) '[*] [:project/key project-key])
        nrepl-cfg (:nrepl (config/get-config))]
    (db/transact! [{:repo/path repo :repo/repl-config {:project-key project-key :aliases (vec aliases)}}])
    (events/broadcast! [:repl/status {:repo repo :status :starting}])
    (repl/start! repo project (assoc nrepl-cfg :aliases (vec aliases)) (callbacks repo))
    (status* repo)))

(defmethod handle :repl/disconnect [_ {:keys [repo]} _]
  (repl/disconnect! repo)
  (events/broadcast! [:repl/status {:repo repo :status :disconnected}])
  true)

(defmethod handle :repl/eval [_ {:keys [repo code ns lang path line column]} _]
  (repl/eval! repo code (cond-> {:ns ns :lang (or lang :clj)}
                          path (assoc :file (str (fs/path repo path)))
                          line (assoc :line line)
                          column (assoc :column column))))

(defmethod handle :repl/load-file [_ {:keys [repo path lang]} _]
  (repl/load-file! repo (str (fs/path repo path)) {:lang (or lang :clj)}))

(defmethod handle :repl/interrupt [_ {:keys [repo]} _] (repl/interrupt! repo) true)

(defmethod handle :repl/complete [_ {:keys [repo prefix ns]} _]
  (if (repl/status repo) (repl/complete repo prefix ns) []))

(defmethod handle :repl/info [_ {:keys [repo sym ns]} _]
  (when (repl/status repo) (repl/info repo sym ns)))

(defmethod handle :repl/builds [_ {:keys [repo]} _] (repl/cljs-builds repo))

(defmethod handle :repl/select-build [_ {:keys [repo build]} _]
  (repl/select-cljs-build! repo build))

;; ---------------------------------------------------------------------------
;; Run modal

(defn invocation-code
  "Code for a Run: mode :call (default), :eval-var or :macroexpand."
  [{:keys [fqn args mode]}]
  (let [args (str/join " " (map str/trim (remove str/blank? args)))]
    (case (or mode :call)
      :call (str "(#'" fqn (when (seq args) (str " " args)) ")")
      :eval-var (str "@#'" fqn)
      :macroexpand (str "(macroexpand-1 '(" fqn (when (seq args) (str " " args)) "))"))))

(defmethod handle :run/invoke [_ {:keys [repo fqn ns args arity lang] :as params} _]
  (let [lang (or lang :clj)]
    (when (= lang :clj)
      (repl/eval! repo (str "(when-not (find-ns '" ns ") (require '" ns "))") {:ns "user" :lang :clj}))
    (let [code (invocation-code params)
          t0 (System/currentTimeMillis)
          res (repl/eval! repo code {:ns ns :lang lang})
          rid (index/repo-eid repo)]
      (db/transact! [(index/strip-nils
                      {:run/repo rid :run/fqn fqn :run/arity arity :run/args (vec args)
                       :run/time t0 :run/result (some-> (:value res) (subs 0 (min 4000 (count (:value res)))))
                       :run/error? (boolean (:ex res))})])
      (assoc res :code code :elapsed-ms (- (System/currentTimeMillis) t0)))))

(defmethod handle :run/history [_ {:keys [repo fqn]} _]
  (->> (d/q '[:find [(pull ?e [:run/arity :run/args :run/time :run/result :run/error?]) ...]
              :in $ ?r ?fqn
              :where [?e :run/repo ?r] [?e :run/fqn ?fqn]]
            (db/db) (index/repo-eid repo) fqn)
       (sort-by :run/time >)
       (take 20)
       vec))
