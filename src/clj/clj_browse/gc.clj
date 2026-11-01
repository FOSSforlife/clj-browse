(ns clj-browse.gc
  "Repo removal and mark-and-sweep of shared content (trees, blobs, forms, vars)."
  (:require [clj-browse.db :as db]
            [clj-browse.index :as index]
            [datalevin.core :as d]))

(defn- tx-chunks! [tx] (doseq [part (partition-all 2000 tx)] (db/transact! (vec part))))

(defn- reachable-trees [db roots]
  (loop [todo (vec roots) seen (transient #{})]
    (if-let [t (peek todo)]
      (if (contains? seen t)
        (recur (pop todo) seen)
        (let [subs (d/q '[:find [?s ...] :in $ ?t :where [?t :tree/entries ?e] [?e :entry/tree ?s]] db t)]
          (recur (into (pop todo) subs) (conj! seen t))))
      (persistent! seen))))

(defn sweep!
  "Delete trees, blobs, forms and vars nothing points at any more."
  []
  (let [db (db/db)
        roots (d/q '[:find [?t ...] :where [?c :commit/tree ?t]] db)
        live-trees (reachable-trees db roots)
        all-trees (d/q '[:find [?t ...] :where [?t :tree/sha]] db)
        dead-trees (remove live-trees all-trees)]
    (tx-chunks! (map (fn [t] [:db/retractEntity t]) dead-trees))
    (let [db (db/db)
          live-blobs (into (set (d/q '[:find [?b ...] :where [_ :file/blob ?b]] db))
                           (d/q '[:find [?b ...] :where [_ :entry/blob ?b]] db))
          dead-blobs (remove live-blobs (d/q '[:find [?b ...] :where [?b :blob/key]] db))]
      (tx-chunks! (map (fn [b] [:db/retractEntity b]) dead-blobs)))
    (let [db (db/db)
          live-forms (set (d/q '[:find [?f ...] :where [_ :occ/form ?f]] db))
          dead-forms (remove live-forms (d/q '[:find [?f ...] :where [?f :form/hash]] db))]
      (tx-chunks! (map (fn [f] [:db/retractEntity f]) dead-forms)))
    (let [db (db/db)
          live-vars (into (set (d/q '[:find [?v ...] :where [_ :def/var ?v]] db))
                          (d/q '[:find [?v ...] :where [_ :usage/var ?v]] db))
          dead-vars (remove live-vars (d/q '[:find [?v ...] :where [?v :var/fqn]] db))]
      (tx-chunks! (map (fn [v] [:db/retractEntity v]) dead-vars)))))

(defn remove-repo!
  [repo-path]
  (when-let [rid (index/repo-eid repo-path)]
    (let [db (db/db)
          owned (concat (d/q '[:find [?e ...] :in $ ?r :where [?e :file/repo ?r]] db rid)
                        (d/q '[:find [?e ...] :in $ ?r :where [?e :project/repo ?r]] db rid)
                        (d/q '[:find [?e ...] :in $ ?r :where [?e :ref/repo ?r]] db rid)
                        (d/q '[:find [?e ...] :in $ ?r :where [?e :run/repo ?r]] db rid))
          commits (d/q '[:find ?c (count ?r2) :in $ ?r :where [?c :commit/repos ?r] [?c :commit/repos ?r2]] db rid)]
      (tx-chunks! (map (fn [e] [:db/retractEntity e]) owned))
      (tx-chunks! (for [[c n] commits]
                    (if (= 1 n) [:db/retractEntity c] [:db/retract c :commit/repos rid])))
      (db/transact! [[:db/retractEntity rid]])
      (sweep!)
      true)))
