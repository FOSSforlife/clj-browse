(ns clj-browse.config
  "User preferences in config.edn. The file is the source of truth; the settings
  UI writes it and a watcher reloads it."
  (:require [babashka.fs :as fs]
            [clj-browse.paths :as paths]
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.tools.logging :as log]))

(def defaults
  {:theme :system                 ; :system :light :dark
   :repo-nav :tabs                ; :tabs :sidebar
   :compact-view? false
   :outline-view :directories     ; :directories :namespaces
   :compact-folders? true
   :docstring-lines 3
   :editor-command "code -g {file}:{line}"  ; {file} {line} placeholders; also "$EDITOR"
   :commit-window 100
   :nrepl {:nrepl-version "1.8.0"
           :cider-nrepl-version "0.64.0"
           :middleware ["cider.nrepl/cider-middleware"]}
   :keybindings {:outline/up "ArrowUp"
                 :outline/down "ArrowDown"
                 :outline/expand "ArrowRight"
                 :outline/collapse "ArrowLeft"
                 :form/edit-doc "Enter"
                 :form/run "r"
                 :form/copy "c"
                 :form/paste "v"
                 :form/usages "u"
                 :file/open-external "e"
                 :form/send-repl "Ctrl+Enter"
                 :palette/open "Ctrl+k"}
   :server {:host "127.0.0.1" :port 7878 :token nil}})

(defonce state (atom defaults))
(defonce listeners (atom {}))

(defn deep-merge [& ms]
  (apply merge-with (fn [a b] (if (and (map? a) (map? b)) (deep-merge a b) b)) ms))

(defn read-config []
  (let [f (paths/config-file)]
    (if (fs/exists? f)
      (try (deep-merge defaults (edn/read-string (slurp (str f))))
           (catch Exception e
             (log/warn e "Invalid config.edn, using defaults")
             defaults))
      defaults)))

(defn reload! []
  (let [old @state new (reset! state (read-config))]
    (when (not= old new)
      (doseq [[_ f] @listeners] (f new)))
    new))

(defn get-config [] @state)

(defn write! [cfg]
  (paths/ensure-dirs!)
  (spit (str (paths/config-file)) (with-out-str (pp/pprint cfg)))
  (reload!))

(defn update! [f & args]
  (write! (apply f (read-config) args)))

(defn on-change! [k f] (swap! listeners assoc k f))
