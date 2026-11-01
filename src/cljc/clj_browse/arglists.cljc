(ns clj-browse.arglists
  "Turn arglist strings (from clj-kondo, e.g. \"[a {:keys [b c]} & more]\") into
  input models for the Run modal."
  (:require [clojure.string :as str]
            [edamame.core :as e]))

(defn- read-arglist [s]
  (try (e/parse-string s {:all true :auto-resolve name :readers (fn [_] identity)})
       (catch #?(:clj Exception :cljs :default) _ nil)))

(defn- keys-template [m]
  (let [ks (concat
            (for [[k v] m
                  :when (and (keyword? k) (= "keys" (name k)) (vector? v))
                  sym v]
              (if-let [ns (namespace k)]
                (keyword ns (name sym))
                (if (keyword? sym) sym (keyword (namespace sym) (name sym)))))
            (for [sym (:strs m)] (str sym))
            (for [sym (:syms m)] (list 'quote sym))
            (for [[k v] m :when (or (symbol? k) (vector? k) (map? k))] v))]
    (if (seq ks)
      (str "{" (str/join " " (map #(str (pr-str %) " nil") ks)) "}")
      "{}")))

(defn- param [x]
  (cond
    (symbol? x) {:label (name x) :template ""}
    (map? x) {:label (or (some-> (:as x) name) "map") :template (keys-template x) :destructured :map}
    (vector? x) (let [[before after] (split-with #(not= :as %) x)
                      parts (remove #{'&} before)]
                  {:label (or (some-> (second after) name) (str "[" (str/join " " (map pr-str before)) "]"))
                   :template (str "[" (str/join " " (repeat (count parts) "nil")) "]")
                   :destructured :vector})
    :else {:label (pr-str x) :template ""}))

(defn parse
  "\"[a b & more]\" → {:params [{:label :template}...] :variadic {:label \"more\" ...} | nil}"
  [arglist-str]
  (let [v (read-arglist arglist-str)]
    (if-not (vector? v)
      {:params [] :variadic nil :raw arglist-str}
      (let [[fixed [_ rest-arg]] (split-with #(not= '& %) v)]
        {:params (mapv param fixed)
         :variadic (when rest-arg (param rest-arg))
         :raw arglist-str}))))

(defn arities [arglist-strs]
  (mapv parse arglist-strs))
