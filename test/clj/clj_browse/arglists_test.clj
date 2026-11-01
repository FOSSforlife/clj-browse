(ns clj-browse.arglists-test
  (:require [clj-browse.arglists :as a]
            [clojure.test :refer [deftest is]]))

(deftest parse-arglists
  (is (= ["a" "b"] (map :label (:params (a/parse "[a b]")))))
  (is (= "more" (:label (:variadic (a/parse "[a & more]")))))
  (is (= "{:x nil :y nil}" (:template (first (:params (a/parse "[{:keys [x y]}]"))))))
  (is (= "opts" (:label (first (:params (a/parse "[{:keys [x] :as opts}]"))))))
  (is (= "{:a/x nil}" (:template (first (:params (a/parse "[{:a/keys [x]}]"))))))
  (is (= "[nil nil]" (:template (first (:params (a/parse "[[x y]]"))))))
  (is (= ["s"] (map :label (:params (a/parse "[^String s]")))))
  (is (= [] (:params (a/parse "[]")))))
