(ns clj-browse.paths
  "Per-OS config and data directories.
  Linux: $XDG_CONFIG_HOME/clj-browse (~/.config) and $XDG_DATA_HOME/clj-browse (~/.local/share).
  macOS: ~/Library/Application Support/clj-browse for both, unless XDG vars are set.
  `CLJ_BROWSE_HOME` overrides both (used by tests)."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(defn- mac? [] (str/includes? (str/lower-case (System/getProperty "os.name")) "mac"))

(defn- env [k] (not-empty (System/getenv k)))

(def ^:dynamic *home-override* nil)

(defn- base [xdg-var linux-default]
  (cond
    *home-override* (fs/path *home-override*)
    (env "CLJ_BROWSE_HOME") (fs/path (env "CLJ_BROWSE_HOME"))
    (env xdg-var) (fs/path (env xdg-var) "clj-browse")
    (mac?) (fs/path (fs/home) "Library" "Application Support" "clj-browse")
    :else (fs/path (fs/home) linux-default "clj-browse")))

(defn config-dir [] (base "XDG_CONFIG_HOME" ".config"))
(defn data-dir [] (base "XDG_DATA_HOME" ".local/share"))
(defn config-file [] (fs/path (config-dir) "config.edn"))
(defn db-dir [] (fs/path (data-dir) "db"))

(defn ensure-dirs! []
  (fs/create-dirs (config-dir))
  (fs/create-dirs (data-dir)))
