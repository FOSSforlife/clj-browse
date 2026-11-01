(ns clj-browse.test-fixtures
  "Helpers that build small, deterministic git repositories for tests."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(def ^:dynamic *date*
  "When bound to a git date string, used for author and committer dates."
  nil)

(defn git!
  "Run git with `args` in `repo-dir`. Returns trimmed stdout, throws ex-info
  (with :stderr) on non-zero exit."
  [repo-dir & args]
  (let [env (cond-> {"GIT_AUTHOR_NAME" "Fixture"
                     "GIT_AUTHOR_EMAIL" "fixture@example.com"
                     "GIT_COMMITTER_NAME" "Fixture"
                     "GIT_COMMITTER_EMAIL" "fixture@example.com"
                     "GIT_CONFIG_GLOBAL" "/dev/null"
                     "GIT_CONFIG_NOSYSTEM" "1"}
              *date* (assoc "GIT_AUTHOR_DATE" *date*
                            "GIT_COMMITTER_DATE" *date*))
        {:keys [exit out err]} (-> (apply p/process
                                          {:dir (str repo-dir)
                                           :extra-env env
                                           :out :string
                                           :err :string}
                                          "git" (map str args))
                                   deref)]
    (when-not (zero? exit)
      (throw (ex-info (str "git failed: " (str/join " " args) "\n" err)
                      {:args args :exit exit :stderr err :dir (str repo-dir)})))
    (str/trim out)))

(defn- write-files!
  "Write `files` (rel-path -> content, nil deletes) under `dir`."
  [dir files]
  (doseq [[rel content] files]
    (let [f (fs/path dir rel)]
      (if (nil? content)
        (fs/delete-if-exists f)
        (do (fs/create-dirs (fs/parent f))
            (spit (fs/file f) content))))))

(def ^:private base-epoch-ms 1767225600000) ; 2026-01-01T00:00:00Z

(defn- date-for [n]
  (str (java.time.Instant/ofEpochMilli (+ base-epoch-ms (* n 3600000)))))

(defn make-repo!
  "Create a git repo at `dir` (created if missing) from `spec` and return the
  absolute dir string. spec = {:default-branch \"main\" :steps [...]}.
  Steps: {:commit msg :files {..}} {:branch b} {:checkout b}
  {:merge b :message m} {:tag t} {:write {..}}."
  [dir {:keys [default-branch steps] :or {default-branch "main"}}]
  (let [dir (str (fs/absolutize dir))
        n (volatile! 0)
        next-date! (fn [] (date-for (vswap! n inc)))]
    (fs/create-dirs dir)
    (git! dir "init" "-q" "-b" default-branch)
    (git! dir "config" "commit.gpgsign" "false")
    (doseq [step steps]
      (cond
        (:commit step)
        (do (write-files! dir (:files step))
            (git! dir "add" "-A")
            (binding [*date* (next-date!)]
              (git! dir "commit" "-q" "--allow-empty" "-m" (:commit step))))

        (:branch step) (git! dir "checkout" "-q" "-b" (:branch step))
        (:checkout step) (git! dir "checkout" "-q" (:checkout step))

        (:merge step)
        (binding [*date* (next-date!)]
          (git! dir "merge" "-q" "--no-ff" "-m" (or (:message step) (str "Merge " (:merge step)))
                (:merge step)))

        (:tag step)
        (binding [*date* (next-date!)]
          (git! dir "tag" (:tag step)))

        (:write step) (write-files! dir (:write step))

        :else (throw (ex-info "Unknown fixture step" {:step step}))))
    dir))

(defn with-temp-dir*
  "Call (f dir-string) with a fresh temp dir; delete it afterwards."
  [f]
  (let [dir (fs/create-temp-dir {:prefix "clj-browse-test-"})]
    (try
      (f (str dir))
      (finally
        (fs/delete-tree dir)))))

(defmacro with-temp-dir
  "(with-temp-dir [d] ...) binds d to a temp dir path string, cleaned up after."
  [[sym] & body]
  `(with-temp-dir* (fn [~sym] ~@body)))

;; ---------------------------------------------------------------------------
;; Fixture contents

(def ^:private deps-app-core-v1
  "(ns acme.app.core
  \"Core functions of the acme app.\"
  (:require [acme.app.db :as db]
            [clojure.string :as str]))

(defn greet
  \"Greets a person by name.\"
  ([] (greet \"world\"))
  ([name]
   (db/save! {:greeted name})
   (str \"Hello, \" (str/upper-case name))))

(defn- helper
  \"Internal helper.\"
  [x]
  (inc x))

(def config
  \"Default configuration.\"
  {:env :dev :retries 3})

(defmacro with-thing
  \"Evaluates body with a thing bound to sym.\"
  [[sym thing] & body]
  `(let [~sym ~thing]
     ~@body))

(defmulti area :shape)

(defmethod area :square [{:keys [side]}]
  (* side side))

(comment
  (greet \"x\")
  (helper 1))

(println \"loaded\")
")

(def ^:private deps-app-db-v1
  "(ns acme.app.db
  \"A trivial in-memory store.\")

(defonce store (atom []))

(defn save!
  \"Saves an entity to the store.\"
  [entity]
  (swap! store conj entity)
  entity)
")

(def ^:private deps-app-db-v3
  (str deps-app-db-v1 "
(defn load-all
  \"Loads all entities, saving a marker first.\"
  []
  (save! {:loaded true})
  @store)
"))

(def ^:private deps-app-test
  "(ns acme.app.core-test
  (:require [acme.app.core :as core]
            [clojure.test :refer [deftest is]]))

(deftest greet-test
  (is (= \"Hello, BOB\" (core/greet \"bob\"))))
")

(defn- small-deps-app [ns-sym]
  {:default-branch "main"
   :steps [{:commit "Initial commit"
            :files {"deps.edn" "{:paths [\"src\"]}\n"
                    (str "src/" ns-sym "/core.clj")
                    (str "(ns " ns-sym ".core\n  \"Small app.\")\n\n(defn hello\n  \"Says hello.\"\n  []\n  :hello)\n")}}]})

(defn- many-commits-spec []
  (let [src (fn [i]
              (str "(ns mc.core\n  \"Many commits.\")\n\n(defn counter\n  \"Revision " i ".\"\n  []\n  " i ")\n"))]
    {:default-branch "main"
     :steps (into [{:commit "Initial commit"
                    :files {"deps.edn" "{:paths [\"src\"]}\n"
                            "src/mc/core.clj" (src 0)}}]
                  (map (fn [i] {:commit (str "Update counter docstring " i)
                                :files {"src/mc/core.clj" (src i)}}))
                  (range 1 130))}))

(def fixtures
  "Map of fixture keyword -> repo spec."
  {:deps-app
   {:default-branch "main"
    :steps
    [{:commit "Initial app"
      :files {"deps.edn" "{:paths [\"src\"]\n :aliases {:test {:extra-paths [\"test\"]}}}\n"
              "src/acme/app/core.clj" deps-app-core-v1
              "src/acme/app/db.clj" deps-app-db-v1
              "test/acme/app/core_test.clj" deps-app-test}}
     {:commit "Improve greet docstring"
      :files {"src/acme/app/core.clj"
              (str/replace deps-app-core-v1 "Greets a person by name."
                           "Greets a person by name and records the greeting.")}}
     {:commit "Add db/load-all"
      :files {"src/acme/app/db.clj" deps-app-db-v3}}
     {:tag "v1"}]}

   :lein-lib
   {:default-branch "main"
    :steps
    [{:commit "Initial lib"
      :files {"project.clj" "(defproject acme/lib \"0.1.0\"\n  :description \"A library\"\n  :source-paths [\"src\"]\n  :profiles {:dev {:source-paths [\"dev\"]}})\n"
              "src/acme/lib/util.clj"
              "(ns acme.lib.util
  \"Utility functions.\"
  (:require [clojure.string :as str]))

(defn slugify
  \"Converts s to a url slug.\"
  [s]
  (-> s str/lower-case (str/replace #\"\\s+\" \"-\")))

(defn clamp
  \"Clamps n between lo and hi.\"
  [n lo hi]
  (max lo (min hi n)))

(defn third
  \"Returns the third item of coll.\"
  [coll]
  (nth coll 2 nil))
"
              "dev/user.clj" "(ns user\n  (:require [acme.lib.util :as util]))\n\n(defn reload\n  \"Dev helper.\"\n  []\n  (util/slugify \"Reload Me\"))\n"}}]}

   :shadow-ui
   {:default-branch "main"
    :steps
    [{:commit "Initial ui"
      :files {"shadow-cljs.edn" "{:source-paths [\"src\"]\n :builds {:app {:target :browser}}}\n"
              "src/acme/ui/events.cljs"
              "(ns acme.ui.events
  (:require [re-frame.core :as rf]))

(rf/reg-event-db
 :user/login
 (fn [db [_ u]]
   (assoc db :user u)))

(rf/reg-sub
 :user/current
 (fn [db _]
   (:user db)))

(rf/reg-event-fx
 ::logout
 (fn [{:keys [db]} _]
   {:db (dissoc db :user)}))
"
              "src/acme/ui/views.cljs"
              "(ns acme.ui.views
  (:require [re-frame.core :as rf]))

(defn user-badge
  \"Shows the current user.\"
  []
  (let [u @(rf/subscribe [:user/current])]
    [:div.badge (or (:name u) \"anonymous\")]))

(defn login-button []
  [:button {:on-click #(rf/dispatch [:user/login {:name \"ann\"}])} \"Log in\"])
"
              "src/acme/ui/shared.cljc"
              "(ns acme.ui.shared)

#?(:clj (defn now [] (System/currentTimeMillis))
   :cljs (defn now [] (js/Date.now)))

(defn fmt
  \"Formats a value for display.\"
  [x]
  (str \"<\" x \">\"))

#?(:cljs (defn only-cljs [] 1))
"}}]}

   :monorepo
   {:default-branch "main"
    :steps
    [{:commit "Initial monorepo"
      :files {"deps.edn" "{:paths []\n :aliases {:dev {:extra-paths [\"dev\"]}}}\n"
              "modules/core/deps.edn" "{:paths [\"src\"]}\n"
              "modules/core/src/mono/core.clj"
              "(ns mono.core\n  \"Core module.\")\n\n(defn compute\n  \"Computes something.\"\n  [x]\n  (* 2 x))\n"
              "modules/api/deps.edn" "{:paths [\"src\"]\n :deps {mono/core {:local/root \"../core\"}}}\n"
              "modules/api/src/mono/api.clj"
              "(ns mono.api\n  \"API module.\"\n  (:require [mono.core :as core]))\n\n(defn handler\n  \"Handles a request.\"\n  [req]\n  {:status 200 :body (core/compute (:n req))})\n"
              "modules/api/node_modules/junk/deps.edn" "{:paths [\"src\"]}\n"
              "dev/user.clj" "(ns user\n  (:require [mono.api :as api]))\n\n(defn try-it [] (api/handler {:n 1}))\n"}}]}

   :merge-history
   {:default-branch "main"
    :steps
    [{:commit "Initial"
      :files {"deps.edn" "{:paths [\"src\"]}\n"
              "src/m/a.clj" "(ns m.a)\n\n(defn f\n  \"f v0\"\n  [] 0)\n"}}
     {:commit "Main work"
      :files {"README.md" "# m\n"}}
     {:branch "feature"}
     {:commit "Feature change 1"
      :files {"src/m/a.clj" "(ns m.a)\n\n(defn f\n  \"f v1\"\n  [] 1)\n"}}
     {:commit "Feature change 2"
      :files {"src/m/a.clj" "(ns m.a)\n\n(defn f\n  \"f v2\"\n  [] 2)\n"}}
     {:checkout "main"}
     {:commit "Add g on main"
      :files {"src/m/b.clj" "(ns m.b)\n\n(defn g\n  \"g\"\n  [] :g)\n"}}
     {:merge "feature" :message "Merge feature"}]}

   :broken
   {:default-branch "main"
    :steps
    [{:commit "Initial with broken files"
      :files {"deps.edn" "{:paths [\"src\"]}\n"
              "src/b/ok.clj" "(ns b.ok)\n\n(defn fine\n  \"Fine.\"\n  [x]\n  (inc x))\n"
              "src/b/bad.clj" "(ns b.bad)\n\n(defn broken [x] (inc x)\n"
              "src/b/conflict.clj"
              "(ns b.conflict)\n\n<<<<<<< HEAD\n(defn c [] 1)\n=======\n(defn c [] 2)\n>>>>>>> feature\n"}}]}

   :dirty
   (update (small-deps-app "d") :steps conj
           {:write {"src/d/core.clj"
                    "(ns d.core\n  \"Small app.\")\n\n(defn hello\n  \"Says hello.\"\n  []\n  :hello)\n\n(defn uncommitted\n  \"Not yet committed.\"\n  []\n  :wip)\n"}})

   :many-commits (many-commits-spec)})

(defn make-fixture!
  "Create fixture `fixture-kw` under `parent-dir`; returns its absolute path."
  [parent-dir fixture-kw]
  (make-repo! (fs/path parent-dir (name fixture-kw)) (fixtures fixture-kw)))
