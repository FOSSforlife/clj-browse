(ns clj-browse.electron.main
  "Thin Electron shell: spawns the clj-browse JVM server on a random port,
  waits for its 'running at' line, then shows a BrowserWindow on that URL.

  Quits on all-windows-closed on every platform (no tray, no macOS activate
  handling) and stops the JVM child on quit (SIGTERM, SIGKILL after 5s)."
  (:require [clojure.string :as str]))

(set! *warn-on-infer* false)

(def ^:private electron (js/require "electron"))
(def ^:private child-process (js/require "child_process"))
(def ^:private path (js/require "path"))
(def ^:private fs (js/require "fs"))

(def app (.-app electron))
(def BrowserWindow (.-BrowserWindow electron))
(def Menu (.-Menu electron))
(def dialog (.-dialog electron))
(def shell (.-shell electron))

(def jvm-flags
  ["--add-opens=java.base/java.nio=ALL-UNNAMED"
   "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
   "--enable-native-access=ALL-UNNAMED"
   "--sun-misc-unsafe-memory-access=allow"])

(def startup-timeout-ms 120000)
(def bg-color "#0f1115")

(defonce state (atom {:server nil :window nil :splash nil :url nil :stderr [] :quitting? false}))

(defn- packaged? [] (.-isPackaged app))

(defn- find-java []
  (let [exe (if (= (.-platform js/process) "win32") "java.exe" "java")
        bundled (.join path (.-resourcesPath js/process) "jre" "bin" exe)
        java-home (.. js/process -env -JAVA_HOME)
        from-home (when (seq java-home) (.join path java-home "bin" exe))]
    (cond
      (and (packaged?) (.existsSync fs bundled)) bundled
      (and from-home (.existsSync fs from-home)) from-home
      :else "java")))

(defn- find-jar []
  (if (packaged?)
    (.join path (.-resourcesPath js/process) "clj-browse.jar")
    ;; dev: electron/app/main.js -> electron/ -> repo root
    (.resolve path js/__dirname ".." ".." "target" "clj-browse.jar")))

(defn- html-url [title body]
  (str "data:text/html;charset=utf-8,"
       (js/encodeURIComponent
        (str "<body style='margin:0;height:100vh;display:flex;flex-direction:column;align-items:center;"
             "justify-content:center;background:" bg-color ";color:#c9d1d9;font-family:system-ui,sans-serif'>"
             "<h2 style='font-weight:500'>" title "</h2><p style='opacity:.6'>" body "</p></body>"))))

(defn- show-splash! []
  (let [w (BrowserWindow. #js {:width 420 :height 240 :frame false :resizable false
                               :backgroundColor bg-color :show true})]
    (.loadURL w (html-url "clj-browse" "Starting server…"))
    (swap! state assoc :splash w)))

(defn- close-splash! []
  (when-let [s (:splash @state)]
    (when-not (.isDestroyed s) (.close s))
    (swap! state assoc :splash nil)))

(defn- run-in-page! [fn-name dir]
  (when-let [w (:window @state)]
    (.executeJavaScript (.-webContents w)
                        (str "window.cljBrowse && window.cljBrowse." fn-name "(" (js/JSON.stringify dir) ")"))))

(defn- pick-directory! [fn-name]
  (-> (.showOpenDialog dialog #js {:properties #js ["openDirectory"]})
      (.then (fn [^js r]
               (when-not (or (.-canceled r) (zero? (.. r -filePaths -length)))
                 (run-in-page! fn-name (aget (.-filePaths r) 0)))))))

(defn- build-menu []
  (let [mac? (= (.-platform js/process) "darwin")
        file-items [#js {:label "Add Repo…" :accelerator "CmdOrCtrl+O" :click #(pick-directory! "addRepo")}
                    #js {:label "Scan Folder…" :accelerator "CmdOrCtrl+Shift+O" :click #(pick-directory! "scanFolder")}
                    #js {:type "separator"}
                    #js {:role (if mac? "close" "quit")}]
        tmpl (concat
              (when mac? [#js {:role "appMenu"}])
              [#js {:label "File" :submenu (clj->js file-items)}
               #js {:label "Edit" :submenu #js [#js {:role "undo"} #js {:role "redo"} #js {:type "separator"}
                                                #js {:role "cut"} #js {:role "copy"} #js {:role "paste"}
                                                #js {:role "selectAll"}]}
               #js {:label "View" :submenu #js [#js {:role "reload"} #js {:role "forceReload"}
                                                #js {:role "toggleDevTools"} #js {:type "separator"}
                                                #js {:role "resetZoom"} #js {:role "zoomIn"} #js {:role "zoomOut"}
                                                #js {:type "separator"} #js {:role "togglefullscreen"}]}
               #js {:role "windowMenu"}])]
    (.buildFromTemplate Menu (clj->js tmpl))))

(defn- open-main-window! [url]
  (let [w (BrowserWindow. #js {:width 1400 :height 900 :title "clj-browse"
                               :backgroundColor bg-color :show false})
        wc (.-webContents w)]
    (.setApplicationMenu Menu (build-menu))
    (.setWindowOpenHandler wc (fn [^js details]
                                (.openExternal shell (.-url details))
                                #js {:action "deny"}))
    (.on wc "will-navigate" (fn [^js e target]
                              (when-not (str/starts-with? target url)
                                (.preventDefault e)
                                (.openExternal shell target))))
    (.once w "ready-to-show" (fn [] (close-splash!) (.show w)))
    (.on w "closed" #(swap! state assoc :window nil))
    (.loadURL w url)
    (swap! state assoc :window w)))

(defn- kill-server! []
  (when-let [^js srv (:server @state)]
    (swap! state assoc :server nil)
    (when (nil? (.-exitCode srv))
      (.kill srv "SIGTERM")
      (let [t (js/setTimeout #(try (.kill srv "SIGKILL") (catch :default _ nil)) 5000)]
        (.unref t)
        (.on srv "exit" #(js/clearTimeout t))))))

(defn- fail! [msg]
  (when-not (:quitting? @state)
    (swap! state assoc :quitting? true)
    (close-splash!)
    (let [tail (str/join "\n" (take-last 15 (:stderr @state)))]
      (.showErrorBox dialog "clj-browse failed to start" (str msg (when (seq tail) (str "\n\n" tail))))
      (kill-server!)
      (.quit app))))

(defn- start-server! []
  (let [java (find-java)
        jar (find-jar)
        args (clj->js (concat jvm-flags ["-jar" jar "serve" "--port" "0"]))
        srv (.spawn child-process java args #js {:stdio #js ["ignore" "pipe" "pipe"]})
        buf (atom "")
        ready? (atom false)
        timer (js/setTimeout #(when-not @ready? (fail! "Timed out waiting for the server to start.")) startup-timeout-ms)]
    (swap! state assoc :server srv)
    (.on (.-stdout srv) "data"
         (fn [chunk]
           (when-not @ready?
             (swap! buf str (.toString chunk))
             (when-let [[_ url] (re-find #"clj-browse running at (http://\S+)" @buf)]
               (reset! ready? true)
               (js/clearTimeout timer)
               (swap! state assoc :url url)
               (open-main-window! url)))))
    (.on (.-stderr srv) "data"
         (fn [chunk]
           (swap! state update :stderr #(vec (take-last 50 (concat % (str/split-lines (.toString chunk))))))))
    (.on srv "error" (fn [e] (fail! (str "Could not launch " java ": " (.-message e)))))
    (.on srv "exit" (fn [code sig]
                      (js/clearTimeout timer)
                      (when-not (:quitting? @state)
                        (fail! (str "Server exited (code " code ", signal " sig ")")))))))

(defn main []
  (.on app "window-all-closed" (fn [] (.quit app)))
  (.on app "before-quit" (fn [] (swap! state assoc :quitting? true) (kill-server!)))
  (.on app "will-quit" (fn [] (kill-server!)))
  (-> (.whenReady app)
      (.then (fn [] (show-splash!) (start-server!)))))
