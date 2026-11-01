(ns clj-browse.api.bb
  (:require [clj-browse.api :refer [handle]]
            [clj-browse.bb-tasks :as bb]
            [clj-browse.events :as events]))

(defmethod handle :bb/list [_ {:keys [repo]} _]
  (let [res (bb/list-tasks repo)]
    (when res
      (assoc res :tasks (mapv (fn [t] (assoc t :last (some-> (bb/last-output repo (:name t))
                                                             (dissoc :text))))
                              (:tasks res))
             :running (filterv #(= repo (:repo %)) (bb/running))))))

(defmethod handle :bb/run [_ {:keys [repo task args]} _]
  (let [id (promise)]
    (deliver id
             (bb/run-task! repo task (or args "")
                           {:on-output (fn [{:keys [stream text]}]
                                         (events/broadcast! [:bb/output {:repo repo :task task :id @id
                                                                         :stream stream :text text}]))
                            :on-exit (fn [code]
                                       (events/broadcast! [:bb/exit {:repo repo :task task :id @id :exit code}]))}))
    @id))

(defmethod handle :bb/stop [_ {:keys [id]} _] (bb/stop! id))

(defmethod handle :bb/output [_ {:keys [repo task]} _] (bb/last-output repo task))
