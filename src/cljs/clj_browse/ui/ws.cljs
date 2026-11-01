(ns clj-browse.ui.ws
  "Single transit WebSocket to the backend.
    → [:req id op params]
    ← [:res id {:ok v}|{:error msg}]   [:evt type data]"
  (:require [cognitect.transit :as t]
            [re-frame.core :as rf]))

(defonce ^:private state (atom {:ws nil :open? false :queue [] :pending {} :next-id 0 :retry 0}))

(def ^:private writer (t/writer :json))
(def ^:private reader (t/reader :json))

(defn- url []
  (let [loc js/window.location
        proto (if (= "https:" (.-protocol loc)) "wss:" "ws:")
        token (.get (js/URLSearchParams. (.-search loc)) "token")]
    (str proto "//" (.-host loc) "/ws" (when token (str "?token=" token)))))

(defn- send-raw! [msg]
  (let [{:keys [ws open?]} @state]
    (if open?
      (.send ws (t/write writer msg))
      (swap! state update :queue conj msg))))

(defn request
  "Send a request; returns a js/Promise of the :ok value (rejects with error msg)."
  [op params]
  (js/Promise.
   (fn [resolve reject]
     (let [id (:next-id (swap! state update :next-id inc))]
       (swap! state assoc-in [:pending id] {:resolve resolve :reject reject})
       (send-raw! [:req id op params])))))

(defn- on-message [e]
  (let [[kind a b c] (t/read reader (.-data e))]
    (case kind
      :res (let [{:keys [resolve reject]} (get-in @state [:pending a])]
             (swap! state update :pending dissoc a)
             (if (contains? b :error)
               (when reject (reject (:error b)))
               (when resolve (resolve (:ok b)))))
      :evt (rf/dispatch [:server/event a b])
      nil)
    c))

(defn connect! []
  (let [ws (js/WebSocket. (url))]
    (swap! state assoc :ws ws)
    (set! (.-onopen ws)
          (fn []
            (let [{:keys [queue retry]} @state]
              (swap! state assoc :open? true :queue [] :retry 0)
              (doseq [m queue] (.send ws (t/write writer m)))
              (rf/dispatch [:ws/status :open])
              (when (pos? retry) (rf/dispatch [:app/reconnected])))))
    (set! (.-onmessage ws) on-message)
    (set! (.-onclose ws)
          (fn []
            (swap! state assoc :open? false)
            (rf/dispatch [:ws/status :closed])
            (let [retry (:retry (swap! state update :retry inc))]
              (js/setTimeout connect! (min 5000 (* 300 retry))))))))

;; re-frame effect: {:ws {:op :params :on-ok [ev ...] :on-err [ev ...]}} (or a vector of such maps)
(rf/reg-fx
 :ws
 (fn [reqs]
   (doseq [{:keys [op params on-ok on-err]} (if (map? reqs) [reqs] reqs)
           :when op]
     (-> (request op params)
         (.then (fn [v] (when on-ok (rf/dispatch (conj on-ok v)))))
         (.catch (fn [err]
                   (if on-err
                     (rf/dispatch (conj on-err err))
                     (rf/dispatch [:toast/show {:kind :error :text (str (name op) ": " err)}]))))))))
