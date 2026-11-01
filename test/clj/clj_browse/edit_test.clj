(ns clj-browse.edit-test
  (:require [clj-browse.edit :as e]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- p [r c] {:row r :col c})

(defn- unsupported? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo ex
         (= :unsupported-form (:type (ex-data ex))))))

(def src
  (str "(ns demo.core)\n"
       "\n"
       ";; a comment\n"
       "(defn foo [x]\n"
       "  x)   ; trailing\n"
       "\n"
       "(def   bar 1)\n"))

(deftest locate-test
  (is (= "(ns demo.core)" (e/form-text src (p 1 1))))
  (is (= "(defn foo [x]\n  x)" (e/form-text src (p 4 1))))
  (is (= "(def   bar 1)" (e/form-text src (p 7 1))))
  (is (nil? (e/find-top-level src (p 2 1))))
  (is (some? (e/find-top-level src (p 4 1)))))

(deftest get-docstring-test
  (let [s "(defn a \"Doc A.\" [x] x)\n(defn b [x] x)\n(def c \"C doc\" 1)\n(def d \"just value\")"]
    (is (= "Doc A." (e/get-docstring s (p 1 1))))
    (is (nil? (e/get-docstring s (p 2 1))))
    (is (= "C doc" (e/get-docstring s (p 3 1))))
    (is (nil? (e/get-docstring s (p 4 1))))))

(deftest add-docstring-defn-test
  (testing "arglist on same line moves to its own line; rest preserved"
    (is (= (str "(ns demo.core)\n\n;; a comment\n"
                "(defn foo\n  \"Does foo.\"\n  [x]\n  x)   ; trailing\n\n(def   bar 1)\n")
           (e/set-docstring src (p 4 1) "Does foo."))))
  (testing "multi-arity"
    (is (= "(defn foo\n  \"Doc.\"\n  ([] 1)\n  ([x] x))"
           (e/set-docstring "(defn foo\n  ([] 1)\n  ([x] x))" (p 1 1) "Doc."))))
  (testing "^:private meta on name"
    (is (= "(defn ^:private foo\n  \"Doc.\"\n  [x]\n  x)"
           (e/set-docstring "(defn ^:private foo [x]\n  x)" (p 1 1) "Doc."))))
  (testing "defn-"
    (is (= "(defn- foo\n  \"Doc.\"\n  [x]\n  x)"
           (e/set-docstring "(defn- foo [x]\n  x)" (p 1 1) "Doc."))))
  (testing "qualified head"
    (is (= "(s/defn foo\n  \"Doc.\"\n  [x]\n  x)"
           (e/set-docstring "(s/defn foo [x]\n  x)" (p 1 1) "Doc."))))
  (testing "attr-map without docstring: docstring goes before the map"
    (is (= "(defn foo\n  \"Doc.\"\n  {:a 1}\n  [x] x)"
           (e/set-docstring "(defn foo {:a 1}\n  [x] x)" (p 1 1) "Doc."))))
  (testing "defmacro"
    (is (= "(defmacro m\n  \"Doc.\"\n  [x]\n  `(do ~x))"
           (e/set-docstring "(defmacro m [x]\n  `(do ~x))" (p 1 1) "Doc."))))
  (testing "defmulti"
    (is (= "(defmulti area\n  \"Doc.\"\n  :shape)"
           (e/set-docstring "(defmulti area :shape)" (p 1 1) "Doc."))))
  (testing "indentation follows the form column"
    (is (= "(do)\n   (defn foo\n     \"Doc.\"\n     [x]\n     x)"
           (e/set-docstring "(do)\n   (defn foo [x]\n     x)" (p 2 4) "Doc.")))))

(deftest def-docstring-test
  (is (= "(def x\n  \"Doc.\"\n  1)" (e/set-docstring "(def x 1)" (p 1 1) "Doc.")))
  (is (= "(defonce x\n  \"Doc.\"\n  (atom 0))" (e/set-docstring "(defonce x (atom 0))" (p 1 1) "Doc.")))
  (is (= "(def x \"New\" 1)" (e/set-docstring "(def x \"Old\" 1)" (p 1 1) "New")))
  (is (= "(def x 1)" (e/set-docstring "(def x \"Old\" 1)" (p 1 1) nil)))
  (is (unsupported? #(e/set-docstring "(def x)" (p 1 1) "Doc."))))

(deftest ns-docstring-test
  (is (= "(ns demo.core\n  \"Doc.\"\n  (:require [a.b :as b]))"
         (e/set-docstring "(ns demo.core\n  (:require [a.b :as b]))" (p 1 1) "Doc.")))
  (is (= "(ns demo.core\n  \"Doc.\")"
         (e/set-docstring "(ns demo.core)" (p 1 1) "Doc.")))
  (is (= "(ns demo.core\n  \"New.\"\n  (:require [a.b :as b]))"
         (e/set-docstring "(ns demo.core\n  \"Old.\"\n  (:require [a.b :as b]))" (p 1 1) "New.")))
  (is (= "(ns demo.core\n  (:require [a.b :as b]))"
         (e/set-docstring "(ns demo.core\n  \"Old.\"\n  (:require [a.b :as b]))" (p 1 1) "")))
  (is (= "Old." (e/get-docstring "(ns demo.core\n  \"Old.\"\n  (:require [a.b :as b]))" (p 1 1)))))

(deftest defprotocol-test
  (is (= "(defprotocol P\n  \"Doc.\"\n  (go [this]))"
         (e/set-docstring "(defprotocol P\n  (go [this]))" (p 1 1) "Doc.")))
  (is (= "(defprotocol P\n  \"Doc.\")"
         (e/set-docstring "(defprotocol P)" (p 1 1) "Doc."))))

(deftest unsupported-test
  (is (unsupported? #(e/set-docstring "(deftest t (is true))" (p 1 1) "x")))
  (is (unsupported? #(e/set-docstring "(defrecord R [a])" (p 1 1) "x")))
  (is (unsupported? #(e/set-docstring "(deftype T [a])" (p 1 1) "x")))
  (is (unsupported? #(e/set-docstring "(foo bar)" (p 1 1) "x")))
  (is (unsupported? #(e/set-docstring "{:a 1}" (p 1 1) "x"))))

(deftest replace-docstring-test
  (testing "single to multi line: continuation aligned one column past the opening quote"
    (is (= "(defn foo\n  \"Line one.\n   Line two.\n\n   Line four.\"\n  [x]\n  x)"
           (e/set-docstring "(defn foo\n  \"Old.\"\n  [x]\n  x)" (p 1 1)
                            "Line one.\nLine two.\n\nLine four."))))
  (testing "caller indentation is stripped then re-applied"
    (is (= "(defn foo\n  \"One.\n   Two.\n     Nested.\"\n  [x]\n  x)"
           (e/set-docstring "(defn foo\n  \"Old.\"\n  [x]\n  x)" (p 1 1)
                            "One.\n      Two.\n        Nested."))))
  (testing "multi to single line, same-line doc"
    (is (= "(defn foo \"New.\" [x] x)"
           (e/set-docstring "(defn foo \"Old\n   more\" [x] x)" (p 1 1) "New."))))
  (testing "multi-line doc inserted into def on a deeper column"
    (is (= "  (defn foo\n    \"A.\n     B.\"\n    [x]\n    x)"
           (e/set-docstring "  (defn foo [x]\n    x)" (p 1 3) "A.\nB.")))))

(deftest escaping-test
  (let [out (e/set-docstring "(defn foo [x] x)" (p 1 1) "Say \"hi\" with \\ backslash.")]
    (is (= "(defn foo\n  \"Say \\\"hi\\\" with \\\\ backslash.\"\n  [x] x)" out))
    (is (= "Say \"hi\" with \\ backslash." (e/get-docstring out (p 1 1))))))

(deftest remove-docstring-test
  (is (= "(defn foo\n  [x]\n  x)"
         (e/set-docstring "(defn foo\n  \"Doc.\"\n  [x]\n  x)" (p 1 1) nil)))
  (is (= "(defn foo [x] x)"
         (e/set-docstring "(defn foo \"Doc.\" [x] x)" (p 1 1) "  ")))
  (testing "no docstring and blank doc: unchanged"
    (is (= "(defn foo [x] x)" (e/set-docstring "(defn foo [x] x)" (p 1 1) nil)))))

(deftest preservation-test
  (let [s (str "#!/usr/bin/env bb\n"
               "(ns a)\n\n\n"
               ";; header\n"
               "(defn one [] 1) ; one\n"
               "\t\n"
               "(defn two\n"
               "  \"Two.\"\n"
               "  [] 2)\n"
               ";; footer  \n")
        out (e/set-docstring s (p 8 1) "Second.")]
    (is (= (.replace ^String s "\"Two.\"" "\"Second.\"") out))
    (is (= s (e/set-docstring out (p 8 1) "Two.")))))

(deftest replace-form-test
  (is (= (str "(ns demo.core)\n\n;; a comment\n"
              "(defn foo [] :new)   ; trailing\n\n(def   bar 1)\n")
         (e/replace-form src (p 4 1) "(defn foo [] :new)"))))

(deftest insert-form-test
  (testing "after a middle form"
    (is (= "(a)\n\n(new)\n\n(b)\n\n(c)\n"
           (e/insert-form "(a)\n\n(b)\n\n(c)\n" (p 1 1) "(new)\n"))))
  (testing "after middle form with sloppy spacing"
    (is (= "(a)\n\n(new)\n\n(b)\n"
           (e/insert-form "(a)\n(b)\n" (p 1 1) "(new)"))))
  (testing "keeps trailing line comment attached"
    (is (= "(a) ; hi\n\n(new)\n\n(b)\n"
           (e/insert-form "(a) ; hi\n\n\n\n(b)\n" (p 1 1) "(new)"))))
  (testing "after the last form"
    (is (= "(a)\n\n(new)\n"
           (e/insert-form "(a)\n" (p 1 1) "(new)"))))
  (testing "append at end"
    (is (= "(a)\n\n(b)\n\n(new)\n" (e/insert-form "(a)\n\n(b)\n\n\n" nil "(new)")))
    (is (= "(a)\n\n(new)\n" (e/insert-form "(a)" nil "(new)")))
    (is (= "(new)\n" (e/insert-form "" nil "(new)")))))

(def ns-src
  (str "(ns demo.core\n"
       "  (:require [clojure.string :as str]\n"
       "            [clojure.set :as set]\n"
       "            [my.other]))\n"
       "\n(defn f [])\n"))

(deftest ns-aliases-test
  (is (= {"str" "clojure.string" "set" "clojure.set"} (e/ns-aliases ns-src)))
  (is (= {} (e/ns-aliases "(ns x)")))
  (is (= {"a" "p.a"} (e/ns-aliases "(ns x (:require (p [a :as a] b)))")))
  (is (= {} (e/ns-aliases "(def x 1)"))))

(deftest add-requires-existing-clause-test
  (let [r (e/add-requires ns-src [{:ns "my.lib" :as "lib"}
                                  {:ns "x.y" :refer ["a" "b"]}
                                  {:ns "z"}])]
    (is (= (str "(ns demo.core\n"
                "  (:require [clojure.string :as str]\n"
                "            [clojure.set :as set]\n"
                "            [my.other]\n"
                "            [my.lib :as lib]\n"
                "            [x.y :refer [a b]]\n"
                "            [z]))\n"
                "\n(defn f [])\n")
           (:source r)))
    (is (= ["my.lib" "x.y" "z"] (:added r)))
    (is (= [] (:conflicts r)))))

(deftest add-requires-no-clause-test
  (is (= "(ns demo.core\n  (:require [a.b :as b]\n            [c.d]))\n\n(def x 1)\n"
         (:source (e/add-requires "(ns demo.core)\n\n(def x 1)\n"
                                  [{:ns "a.b" :as "b"} {:ns "c.d"}]))))
  (is (= "(ns demo.core\n  \"Doc.\"\n  (:gen-class)\n  (:require [a.b :as b]))"
         (:source (e/add-requires "(ns demo.core\n  \"Doc.\"\n  (:gen-class))"
                                  [{:ns "a.b" :as "b"}]))))
  (testing "empty clause"
    (is (= "(ns demo.core (:require [a.b :as b]))"
           (:source (e/add-requires "(ns demo.core (:require))" [{:ns "a.b" :as "b"}]))))))

(deftest add-requires-skip-and-conflict-test
  (let [s "(ns demo.core\n  (:require [clojure.string :as str]\n            clojure.set))\n"
        r (e/add-requires s [{:ns "clojure.string" :as "s"}
                             {:ns "clojure.string" :as "str"}
                             {:ns "clojure.set" :as "set"}])]
    (is (= s (:source r)))
    (is (= [] (:added r)))
    (is (= [{:ns "clojure.string" :wanted-as "s" :existing-as "str"}] (:conflicts r))))
  (testing "duplicate specs in one call are only added once"
    (let [r (e/add-requires "(ns x\n  (:require [a]))" [{:ns "b"} {:ns "b"}])]
      (is (= "(ns x\n  (:require [a]\n            [b]))" (:source r)))
      (is (= ["b"] (:added r)))))
  (testing "no ns form"
    (is (thrown? clojure.lang.ExceptionInfo (e/add-requires "(def x 1)" [{:ns "a"}])))))

(deftest blob-sha-test
  ;; `printf 'hello\n' | git hash-object --stdin`
  (is (= "ce013625030ba8dba906f756967f9e9ca394464a" (e/blob-sha (.getBytes "hello\n" "UTF-8"))))
  (is (= "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391" (e/blob-sha (byte-array 0)))))

(deftest write-file-checked-test
  (let [f (.toFile (Files/createTempFile "edit-test" ".clj" (make-array FileAttribute 0)))
        path (.getPath f)]
    (try
      (spit f "hello\n")
      (let [sha (e/blob-sha (.getBytes "hello\n" "UTF-8"))
            ok (e/write-file-checked! path sha "new content\n")]
        (is (true? (:ok ok)))
        (is (= "new content\n" (slurp f)))
        (is (= (e/blob-sha (.getBytes "new content\n" "UTF-8")) (:sha ok)))
        (testing "stale sha is refused and file untouched"
          (let [r (e/write-file-checked! path sha "other\n")]
            (is (= {:ok false :reason :conflict :sha (:sha ok)} r))
            (is (= "new content\n" (slurp f))))))
      (finally (io/delete-file f true)))))
