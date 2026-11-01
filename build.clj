(ns build
  "tools.build script.  clojure -T:build uber

  The jar's manifest carries Add-Opens / Enable-Native-Access so `java -jar` works.
  Flags still needed on the command line (no manifest equivalent):
    --sun-misc-unsafe-memory-access=allow
  Full set used for Datalevin:
    --add-opens=java.base/java.nio=ALL-UNNAMED --add-opens=java.base/sun.nio.ch=ALL-UNNAMED
    --enable-native-access=ALL-UNNAMED --sun-misc-unsafe-memory-access=allow"
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]))

(def main-ns 'clj-browse.main)
(def class-dir "target/classes")
(def web-dir "target/web")
(def jar-file "target/clj-browse.jar")

(defn clean [_] (b/delete {:path "target"}))

(defn- sh! [& cmd]
  (println "$" (apply str (interpose " " cmd)))
  (let [{:keys [exit out err]} (apply sh/sh cmd)]
    (print out) (print err) (flush)
    (when-not (zero? exit) (throw (ex-info (str "command failed: " (first cmd)) {:cmd cmd :exit exit})))))

(defn web
  "Release build of the frontend into target/web (leaves resources/public untouched)."
  [_]
  (b/delete {:path web-dir})
  (sh! "npx" "shadow-cljs" "release" "app" "--config-merge" (str "{:output-dir \"" web-dir "/js\"}"))
  (sh! "npx" "tailwindcss" "-i" "src/css/app.css" "-o" (str web-dir "/css/app.css") "--minify"))

(defn uber [_]
  (clean nil)
  (web nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "resources"] :target-dir class-dir})
    ;; drop dev artifacts, overlay release build
    (b/delete {:path (str class-dir "/public/js")})
    (b/delete {:path (str class-dir "/public/css/app.css")})
    (b/copy-dir {:src-dirs [(str web-dir "/js")] :target-dir (str class-dir "/public/js")})
    (b/copy-dir {:src-dirs [(str web-dir "/css")] :target-dir (str class-dir "/public/css")})
    (b/compile-clj {:basis basis :ns-compile [main-ns] :class-dir class-dir
                    :bindings {#'*warn-on-reflection* false}})
    (b/uber {:basis basis :class-dir class-dir :uber-file jar-file :main main-ns
             :manifest {"Add-Opens" "java.base/java.nio java.base/sun.nio.ch"
                        "Enable-Native-Access" "ALL-UNNAMED"}})
    (println "Built" jar-file)))
