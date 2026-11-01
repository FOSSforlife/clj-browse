(ns clj-browse.bb-tasks-test
  (:require [babashka.fs :as fs]
            [clj-browse.bb-tasks :as bb]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- temp-repo [bb-edn]
  (let [d (str (fs/create-temp-dir {:prefix "bbt"}))]
    (when bb-edn (spit (str d "/bb.edn") bb-edn))
    d))

(defn- await-run
  "Run a task, return {:out :err :exit :id}."
  [repo task args]
  (let [out (atom []) p (promise)
        id (bb/run-task! repo task args
                         {:on-output #(swap! out conj %)
                          :on-exit #(deliver p %)})
        exit (deref p 60000 :timeout)]
    {:id id :exit exit
     :out (apply str (map :text (filter #(= :out (:stream %)) @out)))
     :err (apply str (map :text (filter #(= :err (:stream %)) @out)))}))

(def edn "{:paths [\"src\"]
 :tasks
 {:requires ([clojure.string :as str])
  :init (def x 1)
  zeta \"echo\"
  -helper (println \"h\")
  alpha {:doc \"Alpha doc\" :task (println \"a\")}
  hidden {:private true :task (println 1)}
  hi (println \"hi\") ; comment
  err (binding [*out* *err*] (println \"err\"))
  args (prn *command-line-args*)
  fail (System/exit 1)
  sleepy (Thread/sleep 60000)
  tagged {:doc \"T\" :task #unknown/tag 1}}}")

(deftest list-tasks-test
  (is (nil? (bb/list-tasks (temp-repo nil))))
  (let [{:keys [tasks error]} (bb/list-tasks (temp-repo edn))]
    (is (nil? error))
    (is (= ["zeta" "-helper" "alpha" "hidden" "hi" "err" "args" "fail" "sleepy" "tagged"]
           (mapv :name tasks)))
    (is (= [false true false true]
           (mapv :private? (take 4 tasks))))
    (is (= "Alpha doc" (:doc (nth tasks 2))))
    (is (nil? (:doc (first tasks))))
    (is (= "T" (:doc (last tasks)))))
  (is (= {:tasks [] :error nil} (bb/list-tasks (temp-repo "{:paths []}")))))

(deftest malformed-test
  (let [r (bb/list-tasks (temp-repo "{:tasks {a (println"))]
    (is (= [] (:tasks r)))
    (is (string? (:error r)))))

(deftest split-args-test
  (is (= [] (bb/split-args nil)))
  (is (= [] (bb/split-args "   ")))
  (is (= ["a" "b" "c"] (bb/split-args " a  b c ")))
  (is (= ["a b" "c"] (bb/split-args "\"a b\" c")))
  (is (= ["a b" "c d"] (bb/split-args "'a b' \"c d\"")))
  (is (= ["" "x"] (bb/split-args "\"\" x")))
  (is (= ["ab c"] (bb/split-args "a\"b c\"")))
  (is (= ["a\"b"] (bb/split-args "\"a\\\"b\"")))
  (is (= ["--k=v w"] (bb/split-args "--k='v w'"))))

(deftest run-streams-test
  (let [repo (temp-repo edn)]
    (testing "stdout"
      (let [r (await-run repo "hi" "")]
        (is (= 0 (:exit r)))
        (is (= "hi" (str/trim (:out r))))))
    (testing "stderr"
      (let [r (await-run repo "err" "")]
        (is (= 0 (:exit r)))
        (is (= "err" (str/trim (:err r))))))
    (testing "args"
      (let [r (await-run repo "args" "one \"two three\" 'four'")]
        (is (= 0 (:exit r)))
        (is (= "(\"one\" \"two three\" \"four\")" (str/trim (:out r))))))
    (testing "failure"
      (is (= 1 (:exit (await-run repo "fail" "")))))
    (testing "last-output"
      (let [lo (bb/last-output repo "args")]
        (is (= 0 (:exit lo)))
        (is (false? (:running? lo)))
        (is (str/includes? (:text lo) "two three"))
        (is (:finished-at lo)))
      (is (nil? (bb/last-output repo "nope"))))))

(deftest stop-and-concurrent-test
  (let [repo (temp-repo edn)
        p1 (promise) p2 (promise) p3 (promise)
        id1 (bb/run-task! repo "sleepy" "" {:on-exit #(deliver p1 %)})
        id2 (bb/run-task! repo "sleepy" "" {:on-exit #(deliver p2 %)})
        outs (atom [])
        id3 (bb/run-task! repo "hi" "" {:on-output #(swap! outs conj %)
                                        :on-exit #(deliver p3 %)})]
    (is (not= id1 id2))
    (is (= 0 (deref p3 60000 :timeout)))
    (is (= "hi" (str/trim (:text (first @outs)))))
    (let [ids (set (map :id (bb/running)))]
      (is (contains? ids id1))
      (is (contains? ids id2))
      (is (not (contains? ids id3))))
    (is (true? (bb/stop! id1)))
    (is (not= :timeout (deref p1 20000 :timeout)))
    (is (not= 0 (deref p1 0 0)))
    (is (true? (bb/stop! id2)))
    (is (not= :timeout (deref p2 20000 :timeout)))
    (is (false? (bb/stop! id1)))
    (is (empty? (filter #{id1 id2} (map :id (bb/running)))))))
