(ns clj-browse.index-test
  (:require [clj-browse.db :as db]
            [clj-browse.index :as index]
            [clj-browse.test-fixtures :as tf]
            [clojure.test :refer [deftest is testing]]
            [datalevin.core :as d]))

(defn- forms-in [repo path]
  (d/q '[:find ?name ?kind
         :in $ ?k
         :where [?f :file/key ?k] [?f :file/blob ?b] [?b :blob/occs ?o]
         [?o :occ/form ?fm] [?fm :form/name ?name] [?fm :form/kind ?kind]]
       (db/db) (str repo "|" path)))

(deftest worktree-index
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [repo (index/add-repo! (tf/make-fixture! tmp :deps-app))]
        (index/index-worktree! repo)
        (testing "forms are indexed"
          (let [fs (forms-in repo "src/acme/app/core.clj")]
            (is (contains? fs ["greet" :defn]))
            (is (contains? fs ["acme.app.core" :ns]))
            (is (some #(= :comment (second %)) fs))))
        (testing "usages resolve to vars"
          (is (seq (d/q '[:find ?caller :where [?v :var/fqn "acme.app.db/save!"]
                          [?u :usage/var ?v] [?f :form/usages ?u] [?f :form/name ?caller]]
                        (db/db)))))
        (testing "re-index is a no-op"
          (is (= {:changed [] :removed []} (index/index-worktree! repo))))))))

(deftest broken-files-do-not-stop-indexing
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [repo (index/add-repo! (tf/make-fixture! tmp :broken))]
        (index/index-worktree! repo)
        (is (seq (forms-in repo "src/b/ok.clj")))
        (is (some? (d/q '[:find ?e . :where [?b :blob/error ?e]] (db/db))))))))

(deftest history-window
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [repo (index/add-repo! (tf/make-fixture! tmp :many-commits))]
        (index/index-repo! repo {:window 100})
        (is (= 100 (count (d/q '[:find [?c ...] :where [?c :commit/indexed? true]] (db/db)))))
        (is (= 1 (count (d/q '[:find [?c ...] :where [?c :commit/boundary? true]] (db/db)))))
        (testing "resume is a no-op"
          (is (zero? (index/index-history! repo {:window 100}))))))))

(deftest merge-history
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [repo (index/add-repo! (tf/make-fixture! tmp :merge-history))]
        (index/index-repo! repo {:window 100})
        (is (= 2 (count (d/q '[:find [?p ...] :where [?c :commit/subject ?s]
                               [(clojure.string/starts-with? ?s "Merge")]
                               [?c :commit/parents ?p]] (db/db)))))
        (is (empty? (d/q '[:find [?c ...] :where [?c :commit/boundary? true]] (db/db))))))))

(deftest remove-repo-gc
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [a (index/add-repo! (tf/make-fixture! tmp :deps-app))]
        (index/index-repo! a {:window 100})
        (is (pos? (count (d/q '[:find [?f ...] :where [?f :form/hash]] (db/db)))))
        ((requiring-resolve 'clj-browse.gc/remove-repo!) a)
        (doseq [attr [:form/hash :blob/key :tree/sha :commit/sha :var/fqn :file/key :project/key :ref/key :repo/path]]
          (is (empty? (d/q '[:find [?e ...] :in $ ?a :where [?e ?a]] (db/db) attr)) (str attr)))))))

(deftest reanalysis-clears-old-results
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [repo (index/add-repo! (tf/make-fixture! tmp :deps-app))
            k (fn [] (d/q '[:find ?b . :where [?f :file/path "src/acme/app/db.clj"] [?f :file/blob ?b]] (db/db)))]
        (index/index-worktree! repo)
        ;; simulate an old, failed analysis
        (db/transact! [[:db/add (k) :blob/error "old failure"] [:db/add (k) :blob/extractor-version 0]])
        (index/index-worktree! repo)
        (is (nil? (:blob/error (d/entity (db/db) (k)))))
        (is (seq (:blob/occs (d/entity (db/db) (k)))))))))
