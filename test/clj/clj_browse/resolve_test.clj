(ns clj-browse.resolve-test
  (:require [clj-browse.db :as db]
            [clj-browse.index :as index]
            [clj-browse.queries :as queries]
            [clj-browse.resolve :as resolve]
            [clj-browse.test-fixtures :as tf]
            [clojure.test :refer [deftest is testing]]))

(def lib-v {:steps [{:commit "init" :files {"deps.edn" "{:paths [\"src\"]}"
                                            "src/lib/core.clj" "(ns lib.core)\n\n(defn f \"F.\" [x] x)\n"}}]})

(defn app [lib-dir]
  {:steps [{:commit "init"
            :files {"deps.edn" (pr-str {:paths ["src"] :deps {'lib/lib {:local/root lib-dir}}})
                    "src/app/main.clj" "(ns app.main (:require [lib.core :as l]))\n\n(defn run [] (l/f 1))\n"}}]})

(deftest normalize-urls
  (is (= "github.com/a/b" (resolve/normalize-git-url "git@github.com:a/b.git")))
  (is (= "github.com/a/b" (resolve/normalize-git-url "https://github.com/a/b.git")))
  (is (= "github.com/a/b" (resolve/normalize-git-url "https://github.com/a/b/"))))

(deftest resolution-rules
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [lib (index/add-repo! (tf/make-repo! (str tmp "/lib") lib-v))
            fork (index/add-repo! (tf/make-repo! (str tmp "/fork") lib-v))
            other (index/add-repo! (tf/make-repo! (str tmp "/other")
                                                  {:steps [{:commit "i" :files {"deps.edn" "{:paths [\"src\"]}"
                                                                                "src/o/x.clj" "(ns o.x (:require [lib.core :as l]))\n(defn y [] (l/f 2))\n"}}]}))
            app* (index/add-repo! (tf/make-repo! (str tmp "/app") (app lib)))]
        (doseq [r [lib fork other app*]] (index/index-worktree! r))
        (testing "own definition wins"
          (is (= {:status :own :repos #{lib}} (resolve/resolve-fqn "lib.core/f" lib))))
        (testing "dependency tie-break"
          (is (= {:status :dependency :repos #{lib}} (resolve/resolve-fqn "lib.core/f" app*))))
        (testing "ambiguous without dependency info"
          (is (= {:status :ambiguous :repos #{lib fork}} (resolve/resolve-fqn "lib.core/f" other))))
        (testing "unresolved stub"
          (is (= :unresolved (:status (resolve/resolve-fqn "nope/x" app*)))))
        (testing "usages-of per defining repo"
          (is (= #{app* other} (set (map :repo (resolve/usages-of "lib.core/f" lib)))))
          (is (= #{other} (set (map :repo (resolve/usages-of "lib.core/f" fork))))))
        (testing "file-forms carries usage counts"
          (let [ff (queries/file-forms lib "src/lib/core.clj")]
            (is (= 2 (:usage-count (first (filter #(= "f" (:name %)) (:forms ff))))))))
        (testing "search"
          (is (= "lib.core/f" (-> (queries/search "lcf" {}) :names first :fqn)))
          (is (seq (:docs (queries/search "F" {})))))))))
