(ns clj-browse.server
  "HTTP + WebSocket server. The UI talks to the backend over a single
  transit-JSON WebSocket:
    client → server  [:req id op params]
    server → client  [:res id {:ok v}|{:error msg}]   and   [:evt type data]"
  (:require [clj-browse.api :as api]
            [clj-browse.config :as config]
            [clj-browse.events :as events]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [cognitect.transit :as transit]
            [org.httpkit.server :as http]
            [reitit.ring :as ring])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)))

(defn write-transit ^String [v]
  (let [out (ByteArrayOutputStream. 4096)]
    (transit/write (transit/writer out :json {:handlers {}}) v)
    (.toString out "UTF-8")))

(defn read-transit [^String s]
  (transit/read (transit/reader (ByteArrayInputStream. (.getBytes s "UTF-8")) :json)))

(defn- token-ok? [req]
  (let [token (get-in (config/get-config) [:server :token])]
    (or (str/blank? token)
        (= token (some->> (:query-string req) (re-find #"(?:^|&)token=([^&]+)") second)))))

(defn- handle-msg [ch raw]
  (let [[kind id op params] (try (read-transit raw) (catch Exception _ nil))]
    (when (= kind :req)
      (Thread/startVirtualThread
       (fn []
         (let [res (try {:ok (api/handle op (or params {}) {:channel ch})}
                        (catch Throwable e
                          (log/warn e "API error" op)
                          {:error (or (ex-message e) (str (class e))) :data (ex-data e)}))]
           (try (http/send! ch (write-transit [:res id (update res :data #(when % (pr-str %)))]))
                (catch Throwable e (log/warn e "send failed")))))))))

(defn ws-handler [req]
  (if-not (token-ok? req)
    {:status 401 :body "bad token"}
    (http/as-channel req
                     {:on-open (fn [ch]
                                 (events/subscribe! ch (fn [[t d]] (http/send! ch (write-transit [:evt t d])))))
                      :on-receive (fn [ch msg] (handle-msg ch msg))
                      :on-close (fn [ch _] (events/unsubscribe! ch))})))

(defn- index-html [req]
  (if-not (token-ok? req)
    {:status 401 :body "Missing or wrong ?token="}
    {:status 200 :headers {"content-type" "text/html; charset=utf-8"}
     :body (slurp (io/resource "public/index.html"))}))

(def app
  (ring/ring-handler
   (ring/router [["/" {:get index-html}]
                 ["/ws" {:get ws-handler}]
                 ["/health" {:get (fn [_] {:status 200 :body "ok"})}]])
   (ring/routes
    (ring/create-resource-handler {:path "/" :root "public"})
    (ring/create-default-handler))))

(defonce ^:private server (atom nil))

(defn start! [{:keys [host port]}]
  (reset! server (http/run-server #'app {:ip host :port port :legacy-return-value? false
                                          :max-ws (* 64 1024 1024)}))
  (log/info "clj-browse listening on" (str "http://" host ":" (http/server-port @server)))
  (http/server-port @server))

(defn stop! []
  (when-let [s @server] (http/server-stop! s) (reset! server nil)))
