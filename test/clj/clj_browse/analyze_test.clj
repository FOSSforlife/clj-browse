(ns clj-browse.analyze-test
  "Dogfooding: every source file of clj-browse itself must analyze cleanly."
  (:require [babashka.fs :as fs]
            [clj-browse.analyze :as analyze]
            [clojure.test :refer [deftest is]]))

(deftest analyzes-own-sources
  (let [files (->> (concat (fs/glob "src" "**.{clj,cljs,cljc}") (fs/glob "test" "**.clj"))
                   (map (fn [p] {:path (str p) :text (slurp (str p))})))
        results (analyze/analyze-batch (vec files) {})]
    (is (> (count files) 30))
    (doseq [[f r] (map vector files results)]
      (is (nil? (:error r)) (str (:path f) ": " (:error r)))
      (is (seq (:forms r)) (:path f)))))
