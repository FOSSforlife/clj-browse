(ns clj-browse.test-fixtures-test
  {:clj-kondo/config '{:linters {:unresolved-symbol {:exclude [tmp a b]}}}}
  (:require [babashka.fs :as fs]
            [clj-browse.test-fixtures :as fx :refer [git! with-temp-dir]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest every-fixture-builds
  (with-temp-dir [tmp]
    (doseq [k (keys fx/fixtures)]
      (testing k
        (let [dir (fx/make-fixture! tmp k)
              files (->> (fs/glob dir "**" {:hidden false})
                         (filter fs/regular-file?)
                         (remove #(str/includes? (str %) "/.git/")))]
          (is (fs/absolute? dir))
          (is (re-matches #"[0-9a-f]{40}" (git! dir "rev-parse" "HEAD")))
          (is (<= 2 (count files) 20)))))))

(deftest merge-history-test
  (with-temp-dir [tmp]
    (let [dir (fx/make-fixture! tmp :merge-history)]
      (is (= 3 (count (str/split (git! dir "rev-list" "--parents" "-n1" "HEAD") #" "))))
      (is (= "main" (git! dir "rev-parse" "--abbrev-ref" "HEAD"))))))

(deftest many-commits-test
  (with-temp-dir [tmp]
    (is (= "130" (git! (fx/make-fixture! tmp :many-commits) "rev-list" "--count" "HEAD")))))

(deftest dirty-test
  (with-temp-dir [tmp]
    (let [dir (fx/make-fixture! tmp :dirty)]
      (is (seq (git! dir "status" "--porcelain")))
      (is (= "1" (git! dir "rev-list" "--count" "HEAD"))))))

(deftest deps-app-test
  (with-temp-dir [tmp]
    (let [dir (fx/make-fixture! tmp :deps-app)]
      (is (= "3" (git! dir "rev-list" "--count" "HEAD")))
      (is (= (git! dir "rev-parse" "HEAD") (git! dir "rev-parse" "v1^{commit}")))))
  (testing "monorepo keeps node_modules junk in tree"
    (with-temp-dir [tmp]
      (is (fs/exists? (fs/path (fx/make-fixture! tmp :monorepo)
                               "modules/api/node_modules/junk/deps.edn"))))))

(deftest determinism-test
  (with-temp-dir [a]
    (with-temp-dir [b]
      (is (= (git! (fx/make-fixture! a :deps-app) "rev-parse" "HEAD")
             (git! (fx/make-fixture! b :deps-app) "rev-parse" "HEAD"))))))

(deftest git-errors-test
  (with-temp-dir [tmp]
    (is (thrown? clojure.lang.ExceptionInfo (git! tmp "log")))))
