(ns clj-browse.api-edit-test
  (:require [clj-browse.api :as api]
            [clj-browse.api.all]
            [clj-browse.db :as db]
            [clj-browse.index :as index]
            [clj-browse.queries :as queries]
            [clj-browse.test-fixtures :as tf]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest paste-across-repos-adds-requires
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [app (index/add-repo! (tf/make-fixture! tmp :deps-app))
            mono (index/add-repo! (tf/make-fixture! tmp :monorepo))]
        (index/index-worktree! app)
        (index/index-worktree! mono)
        (let [src (queries/file-forms app "src/acme/app/core.clj")
              greet (first (filter #(= "greet" (:name %)) (:forms src)))
              text (queries/form-source app "src/acme/app/core.clj" greet)
              target-path "modules/api/src/mono/api.clj"
              tgt (queries/file-forms mono target-path)
              clip {:repo app :path "src/acme/app/core.clj" :ns (:ns src) :text text :hash (:hash greet)}
              plan (api/handle :paste/plan {:source clip :target {:repo mono :path target-path :after nil}} {})]
          (testing "plan"
            (is (= #{{:ns "acme.app.db" :as "db"} {:ns "clojure.string" :as "str"}} (set (:requires plan))))
            (is (seq (:warnings plan)) "cross-repo warning"))
          (testing "apply"
            (let [res (api/handle :paste/apply {:target {:repo mono :path target-path :after nil}
                                                :text text :requires (:requires plan) :sha (:sha tgt)} {})
                  out (slurp (str mono "/" target-path))]
              (is (:ok res))
              (is (str/includes? out "[acme.app.db :as db]"))
              (is (str/includes? out "[clojure.string :as str]"))
              (is (str/includes? out "(defn greet"))
              (is (some #(= "greet" (:name %)) (:forms (:file res)))))))))))

(deftest docstring-roundtrip-and-undo
  (tf/with-temp-dir [tmp]
    (db/with-temp-db
      (let [app (index/add-repo! (tf/make-fixture! tmp :deps-app))
            path "src/acme/app/db.clj"]
        (index/index-worktree! app)
        (let [ff (queries/file-forms app path)
              save (first (filter #(= "save!" (:name %)) (:forms ff)))
              before (slurp (str app "/" path))
              res (api/handle :doc/set {:repo app :path path :pos (select-keys save [:row :col])
                                        :doc "Persists `entity`." :sha (:sha ff)} {})]
          (is (:ok res))
          (is (= "Persists `entity`." (-> (filter #(= "save!" (:name %)) (:forms (:file res))) first :defs first :doc)))
          (testing "stale sha is refused"
            (is (= :conflict (:reason (api/handle :doc/set {:repo app :path path :pos (select-keys save [:row :col])
                                                             :doc "x" :sha (:sha ff)} {})))))
          (testing "undo restores the file"
            (is (:ok (api/handle :edit/undo {:repo app :path path} {})))
            (is (= before (slurp (str app "/" path))))))))))
