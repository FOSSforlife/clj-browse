(ns clj-browse.api
  "Request dispatch. Handlers live in clj-browse.api.* namespaces and are
  loaded by clj-browse.api.all.")

(defmulti handle (fn [op _params _ctx] op))

(defmethod handle :default [op _ _]
  (throw (ex-info (str "Unknown op " op) {:op op})))

(defmethod handle :ping [_ _ _] :pong)
