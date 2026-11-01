(ns clj-browse.db
  "The single Datalevin database."
  (:require [babashka.fs :as fs]
            [clj-browse.paths :as paths]
            [clj-browse.schema :as schema]
            [datalevin.core :as d]))

(defonce ^:private conn* (atom nil))

(defn open!
  ([] (open! (str (paths/db-dir))))
  ([dir]
   (fs/create-dirs dir)
   (reset! conn* (d/get-conn dir schema/schema))))

(defn close! []
  (when-let [c @conn*] (d/close c) (reset! conn* nil)))

(defn conn [] (or @conn* (throw (ex-info "DB not open" {}))))
(defn db [] (d/db (conn)))

(defn transact! [tx] (d/transact! (conn) tx))

(defn q [query & inputs] (apply d/q query (db) inputs))

(defn pull [pattern eid] (d/pull (db) pattern eid))

(defn entid [lookup] (:db/id (d/entity (db) lookup)))

(defmacro with-temp-db
  "Open a fresh DB in a temp dir for the duration of body."
  [& body]
  `(let [dir# (str (fs/create-temp-dir {:prefix "clj-browse-db"}))]
     (open! dir#)
     (try ~@body
          (finally (close!) (fs/delete-tree dir#)))))
