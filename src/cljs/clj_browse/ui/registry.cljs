(ns clj-browse.ui.registry
  "Extension points. Built-in features register here exactly like future
  third-party (SCI-loaded) extensions will.

  Points:
    :form-action         {:id :label :icon :order :applies? (fn [ctx]) :run (fn [ctx]) :action kw}
    :result-viewer       {:id :label :applies? (fn [value-str]) :render (fn [value-str]) :order}
    :form-renderer       {:id :applies? (fn [ctx]) :render (fn [ctx])}  ; replaces docstring area
    :outline-decoration  {:id :applies? (fn [ctx]) :render (fn [ctx])}  ; extra badges on rows
    :panel               {:id :label :icon :render (fn [repo]) :order}

  ctx for forms: {:repo :path :form :def :file-data :row}")

(defonce registry (atom {}))

(defn register! [point {:keys [id] :as ext}]
  (swap! registry update point
         (fn [xs] (->> (conj (vec (remove #(= id (:id %)) xs)) ext)
                       (sort-by #(or (:order %) 100))
                       vec)))
  id)

(defn unregister! [point id]
  (swap! registry update point (fn [xs] (vec (remove #(= id (:id %)) xs)))))

(defn extensions [point] (get @registry point []))

(defn applicable [point ctx]
  (filter #(or (nil? (:applies? %)) ((:applies? %) ctx)) (extensions point)))
