(ns clj-browse.events
  "Server → client push events. Subscribers are WebSocket channels (or test fns).")

(defonce subscribers (atom {}))

(defn subscribe! [k f] (swap! subscribers assoc k f))
(defn unsubscribe! [k] (swap! subscribers dissoc k))

(defn broadcast!
  "event: [event-type data]"
  [event]
  (doseq [[_ f] @subscribers]
    (try (f event) (catch Exception _))))
