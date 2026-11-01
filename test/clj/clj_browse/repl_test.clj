(ns clj-browse.repl-test
  (:require [babashka.fs :as fs]
            [clj-browse.repl :as repl]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [nrepl.server :as server])
  (:import (java.net Socket)))

(def ^:private opts {:nrepl-version "1.8.0" :cider-nrepl-version "0.64.0"
                     :middleware ["cider.nrepl/cider-middleware"]})

(deftest start-command-deps
  (let [{:keys [cmd dir]} (repl/start-command "/tmp/r" {:project/type :deps :project/dir ""}
                                              (assoc opts :aliases ["dev" "test"]))]
    (is (= "/tmp/r" dir))
    (is (= ["clojure" "-Sdeps"
            "{:deps {nrepl/nrepl {:mvn/version \"1.8.0\"}, cider/cider-nrepl {:mvn/version \"0.64.0\"}}}"
            "-M:dev:test" "-m" "nrepl.cmdline"
            "--middleware" "[cider.nrepl/cider-middleware]" "--port" "0"]
           cmd)))
  (testing "no aliases, sub dir"
    (let [{:keys [cmd dir]} (repl/start-command "/tmp/r" {:project/type :deps :project/dir "sub/x"} opts)]
      (is (= "/tmp/r/sub/x" dir))
      (is (= "-M" (nth cmd 3)))
      (is (= ["-m" "nrepl.cmdline"] (subvec cmd 4 6)))))
  (testing "Sdeps is readable edn"
    (let [{:keys [cmd]} (repl/start-command "/tmp/r" {:project/type :deps :project/dir ""} {})]
      (is (= '{:deps {nrepl/nrepl {:mvn/version "1.8.0"} cider/cider-nrepl {:mvn/version "0.64.0"}}}
             (read-string (nth cmd 2)))))))

(deftest start-command-lein
  (let [inject ["lein" "update-in" ":dependencies" "conj" "[nrepl/nrepl \"1.8.0\"]" "--"
                "update-in" ":plugins" "conj" "[cider/cider-nrepl \"0.64.0\"]" "--"]]
    (is (= {:cmd (into inject ["with-profile" "+dev,+test" "repl" ":headless"]) :dir "/tmp/r"}
           (repl/start-command "/tmp/r" {:project/type :lein :project/dir ""}
                               (assoc opts :aliases ["dev" "test"]))))
    (is (= (into inject ["repl" ":headless"])
           (:cmd (repl/start-command "/tmp/r" {:project/type :lein :project/dir ""} opts))))
    (is (= ["lein" "repl" ":headless"]
           (:cmd (repl/start-command "/tmp/r" {:project/type :lein :project/dir ""}
                                     {:nrepl-version nil :cider-nrepl-version nil :middleware nil}))))
    (testing "extra middleware"
      (is (= (into inject ["update-in" ":repl-options" "assoc" ":nrepl-middleware" "[my.mw/wrap]" "--"
                           "repl" ":headless"])
             (:cmd (repl/start-command "/tmp/r" {:project/type :lein :project/dir ""}
                                       (update opts :middleware conj "my.mw/wrap"))))))))

(deftest start-command-shadow
  (is (= {:cmd ["npx" "shadow-cljs" "-d" "nrepl/nrepl:1.8.0" "-d" "cider/cider-nrepl:0.64.0" "server"]
          :dir "/tmp/r/app"}
         (repl/start-command "/tmp/r" {:project/type :shadow :project/dir "app"}
                             (assoc opts :aliases ["dev"]))))
  (is (= ["npx" "shadow-cljs" "server"]
         (:cmd (repl/start-command "/tmp/r" {:project/type :shadow :project/dir ""}
                                   {:nrepl-version nil :cider-nrepl-version nil})))))

(deftest detect-port-and-builds
  (let [d (fs/create-temp-dir {:prefix "repl-test"})]
    (try
      (is (nil? (repl/detect-port (str d))))
      (fs/create-dirs (fs/path d ".shadow-cljs"))
      (spit (str (fs/path d ".shadow-cljs" "nrepl.port")) "1234")
      (is (= 1234 (repl/detect-port (str d))))
      (spit (str (fs/path d ".nrepl-port")) "4321\n")
      (is (= 4321 (repl/detect-port (str d))))
      (spit (str (fs/path d "shadow-cljs.edn"))
            "{:builds {:app {:target :browser :x #shadow/env \"X\"} :test {:target :node-test}}}")
      (is (= ["app" "test"] (repl/cljs-builds (str d))))
      (finally (fs/delete-tree d)))))

;; ---------------------------------------------------------------------------
;; integration

(defn- port-open? [port]
  (try (with-open [_ (Socket. "localhost" (int port))] true)
       (catch Exception _ false)))

(defn- wait-until [pred ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 100) (recur))))))

(defn- demo-repo! []
  (let [d (fs/create-temp-dir {:prefix "repl-it"})]
    (spit (str (fs/path d "deps.edn")) (pr-str {:paths ["src"]}))
    (fs/create-dirs (fs/path d "src" "demo"))
    (spit (str (fs/path d "src" "demo" "core.clj"))
          (str "(ns demo.core)\n\n"
               "(defn add \"Adds two numbers.\" [a b] (+ a b))\n\n"
               "(defn boom [] (throw (ex-info \"boom\" {:k 1})))\n"))
    (spit (str (fs/path d "src" "demo" "other.clj"))
          "(ns demo.other)\n\n(defn twice [x] (* 2 x))\n\n(twice 21)\n")
    d))

(deftest ^:integration start-deps-repl
  (let [d (demo-repo!)
        repo (str d)
        output (atom [])
        statuses (atom [])]
    (try
      (let [info (repl/start! repo {:project/type :deps :project/dir ""} {}
                              {:on-output #(swap! output conj %)
                               :on-status #(swap! statuses conj %)})
            port (:port info)]
        (is (= :connected (:status info)))
        (is (true? (:owned? info)))
        (is (pos-int? port))
        (is (some #(= :starting (:status %)) @statuses))
        (is (some #(str/includes? (:text %) "nREPL server started") @output)
            "process output streamed")

        (testing "eval"
          (is (= "3" (:value (repl/eval! repo "(require 'demo.core) (demo.core/add 1 2)" {}))))
          (is (= "5" (:value (repl/eval! repo "(add 2 3)" {:ns "demo.core"})))))

        (testing "pretty printed value"
          (let [v (:value (repl/eval! repo "(zipmap (range 30) (range 30))" {}))]
            (is (str/includes? v "\n"))))

        (testing "out captured and streamed"
          (reset! output [])
          (let [r (repl/eval! repo "(println \"hello there\") :ok" {})]
            (is (= ":ok" (:value r)))
            (is (= "hello there\n" (:out r)))
            (is (some #(and (= :out (:stream %)) (str/includes? (:text %) "hello there")) @output))))

        (testing "exception"
          (let [r (repl/eval! repo "(boom)" {:ns "demo.core"})]
            (is (= "boom" (get-in r [:ex :message])))
            (is (= "clojure.lang.ExceptionInfo" (get-in r [:ex :class])))
            (is (str/includes? (get-in r [:ex :data-str]) ":k 1"))
            (is (seq (:err r)))))

        (testing "timeout interrupts"
          (let [r (repl/eval! repo "(Thread/sleep 60000)" {:timeout-ms 500})]
            (is (= "Timed out" (get-in r [:ex :message]))))
          (is (= "2" (:value (repl/eval! repo "(+ 1 1)" {})))))

        (testing "interrupt!"
          (let [f (future (repl/eval! repo "(Thread/sleep 60000)" {}))]
            (Thread/sleep 500)
            (repl/interrupt! repo)
            (let [r (deref f 10000 ::timeout)]
              (is (not= ::timeout r))
              (is (some? (:ex r))))))

        (testing "concurrent evals"
          (let [fs (doall (for [i (range 8)] (future (:value (repl/eval! repo (str "(* " i " 10)") {})))))]
            (is (= (map #(str (* % 10)) (range 8)) (map deref fs)))))

        (testing "load-file"
          (let [r (repl/load-file! repo (str (fs/path d "src" "demo" "other.clj")) {})]
            (is (= "42" (:value r)))
            (is (nil? (:ex r))))
          (is (= "8" (:value (repl/eval! repo "(demo.other/twice 4)" {})))))

        (testing "complete"
          (is (some #(= "add" (:candidate %)) (repl/complete repo "ad" "demo.core"))))

        (testing "info"
          (let [i (repl/info repo "add" "demo.core")]
            (is (= "add" (:name i)))
            (is (= "demo.core" (:ns i)))
            (is (= "Adds two numbers." (:doc i)))
            (is (str/includes? (:arglists-str i) "[a b]"))
            (is (= 3 (:line i))))
          (is (nil? (repl/info repo "nope-nope" "demo.core"))))

        (testing "status"
          (is (= {:repo repo :port port :owned? true :type :deps :status :connected
                  :cljs-builds [] :cljs-session? false :cider? true}
                 (repl/status repo))))

        (testing "cljs without session"
          (is (thrown? clojure.lang.ExceptionInfo (repl/eval! repo "1" {:lang :cljs}))))

        (testing "disconnect kills process"
          (is (port-open? port))
          (repl/disconnect! repo)
          (is (nil? (repl/status repo)))
          (is (wait-until #(not (port-open? port)) 15000))
          (is (= :disconnected (:status (last @statuses))))
          (repl/disconnect! repo)))
      (finally
        (repl/disconnect! repo)
        (fs/delete-tree d)))))

(deftest ^:integration start-failure
  (let [d (fs/create-temp-dir {:prefix "repl-fail"})]
    (try
      (spit (str (fs/path d "deps.edn")) "{:deps {no-such/lib-xyz {:mvn/version \"0.0.0-nope\"}}}")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exited|Timed out"
                            (repl/start! (str d) {:project/type :deps :project/dir ""}
                                         {:timeout-ms 120000} {})))
      (is (nil? (repl/status (str d))))
      (finally
        (repl/disconnect! (str d))
        (fs/delete-tree d)))))

(deftest ^:integration connect-existing
  (let [d (fs/create-temp-dir {:prefix "repl-connect"})
        srv (server/start-server :port 0)
        port (:port srv)
        repo (str d)]
    (try
      (spit (str (fs/path d ".nrepl-port")) (str port))
      (is (= port (repl/detect-port repo)))
      (let [info (repl/connect! repo nil {})]
        (is (= :connected (:status info)))
        (is (false? (:owned? info)))
        (is (= port (:port info))))
      (is (= "3" (:value (repl/eval! repo "(+ 1 2)" {}))))
      (testing "without cider: builtin completions/lookup"
        (is (some #(= "println" (:candidate %)) (repl/complete repo "printl" "user")))
        (is (= "println" (:name (repl/info repo "println" "user")))))
      (testing "exception without cider"
        (is (= "x" (get-in (repl/eval! repo "(throw (ex-info \"x\" {:a 2}))" {}) [:ex :message]))))
      (repl/disconnect! repo)
      (is (nil? (repl/status repo)))
      (is (port-open? port) "server we only connected to keeps running")
      (finally
        (repl/disconnect! repo)
        (server/stop-server srv)
        (fs/delete-tree d)))))
