(ns clj-browse.projects-test
  (:require [babashka.fs :as fs]
            [clj-browse.projects :as p]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [d (fs/create-temp-dir {:prefix "projects-test"})]
      (try (binding [*root* d] (t))
           (finally (fs/delete-tree d))))))

(defn- put! [rel & [content]]
  (let [f (fs/path *root* rel)]
    (fs/create-dirs (fs/parent f))
    (spit (str f) (or content ""))
    f))

(defn- dirs! [& rels] (doseq [r rels] (fs/create-dirs (fs/path *root* r))))

(defn- by-file [projects f] (first (filter #(= f (:project/file %)) projects)))

(deftest deps-edn
  (dirs! "src" "test" "dev" "extra")
  (put! "deps.edn"
        (pr-str '{:paths ["src" "./src/" "missing"]
                  :deps {foo/bar {:local/root "../sibling"}
                         io.github.x/y {:git/sha "abc" :git/tag "v1"}
                         other/lib {:git/url "https://example.com/o/lib.git" :git/sha "1"}
                         org.clojure/clojure {:mvn/version "1"}}
                  :aliases {:test {:extra-paths ["test" "src"]
                                   :extra-deps {a/b {:local/root "extra"}}}
                            :dev/repl {:extra-paths [:shared "nothing"]}
                            :shared ["dev"]}}))
  (let [[proj :as ps] (p/discover-projects *root*)]
    (is (= 1 (count ps)))
    (is (= :deps (:project/type proj)))
    (is (= "deps.edn" (:project/file proj)))
    (is (= "" (:project/dir proj)))
    (is (= (str (fs/file-name *root*)) (:project/name proj)))
    (is (nil? (:project/error proj)))
    (is (= #{"test" "dev/repl" "shared"} (set (:project/aliases proj))))
    (is (= [{:path/dir "src" :path/tags #{"default" ":test"}}
            {:path/dir "test" :path/tags #{":test"}}
            {:path/dir "dev" :path/tags #{":dev/repl"}}]
           (:project/paths proj)))
    (is (= #{(str (fs/normalize (fs/path *root* "../sibling")))
             (str (fs/path *root* "extra"))}
           (set (:project/local-roots proj))))
    (is (= #{"https://github.com/x/y" "https://example.com/o/lib.git"}
           (set (:project/git-urls proj))))))

(deftest deps-default-paths
  (dirs! "src")
  (put! "deps.edn" "{}")
  (is (= ["src"] (map :path/dir (:project/paths (first (p/discover-projects *root*)))))))

(deftest project-clj
  (dirs! "src" "test" "dev-src" "gen")
  (put! "project.clj"
        "(defproject my/app \"1.0\"
           :source-paths [\"src\" ~(str \"gen\")]
           :dependencies [[org.clojure/clojure \"1.11.0\"]]
           :profiles {:dev {:source-paths [\"dev-src\"]}
                      :uberjar {:aot :all}})")
  (let [proj (first (p/discover-projects *root*))]
    (is (= :lein (:project/type proj)))
    (is (= "my/app" (:project/name proj)))
    (is (nil? (:project/error proj)))
    (is (= ["dev" "uberjar"] (:project/aliases proj)))
    (testing "non-literal :source-paths falls back to default"
      (is (= [{:path/dir "src" :path/tags #{"default"}}
              {:path/dir "test" :path/tags #{":test"}}
              {:path/dir "dev-src" :path/tags #{":dev"}}]
             (:project/paths proj))))))

(deftest shadow-cljs
  (dirs! "src" "cljs-src")
  (put! "shadow-cljs.edn"
        "{:source-paths [\"src\" \"cljs-src\"]
          :dev-http {8080 #shadow/env \"X\"}
          :builds {:app {:target :browser :x #env FOO}
                   :lib {:target :node-library}}}")
  (let [proj (first (p/discover-projects *root*))]
    (is (= :shadow (:project/type proj)))
    (is (nil? (:project/error proj)))
    (is (= ["app" "lib"] (:project/builds proj)))
    (is (= ["src" "cljs-src"] (map :path/dir (:project/paths proj)))))
  (testing "delegating to deps.edn does not default to src"
    (put! "shadow-cljs.edn" "{:deps true :builds {:a {}}}")
    (let [proj (first (p/discover-projects *root*))]
      (is (= [] (:project/paths proj)))
      (is (= ["a"] (:project/builds proj))))))

(deftest monorepo
  (dirs! "src" "modules/a/src" "modules/b/src" "modules/b/test")
  (put! "deps.edn" "{}")
  (put! "modules/a/deps.edn" "{}")
  (put! "modules/b/project.clj" "(defproject b \"0\")")
  (put! "node_modules/pkg/deps.edn" "{}")
  (put! "target/project.clj" "(defproject t \"0\")")
  (put! ".hidden/deps.edn" "{}")
  (let [ps (p/discover-projects *root*)]
    (is (= ["deps.edn" "modules/a/deps.edn" "modules/b/project.clj"]
           (map :project/file ps)))
    (is (= ["" "modules/a" "modules/b"] (map :project/dir ps)))
    (is (= "a" (:project/name (by-file ps "modules/a/deps.edn"))))
    (is (= "b" (:project/name (by-file ps "modules/b/project.clj"))))
    (is (= [{:path/dir "modules/a/src" :path/tags #{"default"}}]
           (:project/paths (by-file ps "modules/a/deps.edn"))))
    (is (= ["modules/b/src" "modules/b/test"]
           (map :path/dir (:project/paths (by-file ps "modules/b/project.clj")))))))

(deftest malformed
  (dirs! "src")
  (put! "deps.edn" "{:paths [\"src\"")
  (put! "sub/shadow-cljs.edn" "{:builds")
  (let [ps (p/discover-projects *root*)]
    (is (= 2 (count ps)))
    (is (every? (comp string? :project/error) ps))
    (is (= ["src"] (map :path/dir (:project/paths (by-file ps "deps.edn")))))))

(deftest find-repos
  (dirs! "a/.git" "b/c/.git" "b/c/inner/.git" "node_modules/x/.git" ".hid/y/.git"
         "plain/deep/d/.git")
  (put! "wt/.git" "gitdir: /elsewhere")
  (is (= (mapv #(str (fs/path *root* %)) ["a" "b/c" "plain/deep/d" "wt"])
         (p/find-repos *root*)))
  (testing "max-depth"
    (is (= (mapv #(str (fs/path *root* %)) ["a" "wt"]) (p/find-repos *root* {:max-depth 1}))))
  (testing "root itself is a repo"
    (dirs! ".git")
    (is (= [(str *root*)] (p/find-repos *root*)))))

(deftest list-source-files
  (put! "src/a/core.clj")
  (put! "src/a/ui.cljs")
  (put! "src/a/x.cljc")
  (put! "src/script.bb")
  (put! "src/readme.md")
  (put! "src/node_modules/z.clj")
  (put! "test/t_test.clj")
  (is (= ["src/a/core.clj" "src/a/ui.cljs" "src/a/x.cljc" "src/script.bb" "test/t_test.clj"]
         (p/list-source-files *root* ["src" "test" "nope" "src/a"]))))

(deftest predicates
  (is (p/skip-dir? "node_modules"))
  (is (not (p/skip-dir? "src")))
  (is (p/clojure-file? "a/b.cljc"))
  (is (not (p/clojure-file? "a/b.edn"))))
