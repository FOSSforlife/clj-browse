(ns clj-browse.ui.tree
  "Pure functions: outline data + expansion state → flat list of visible rows.
  A flat list makes keyboard navigation and guide lines simple.

  Row ids:
    [:project pkey] [:path pkey dir] [:dir pkey dir] [:file pkey path]
    [:form pkey path index] [:child pkey path index ci] [:split pkey path index di]"
  (:require [clojure.string :as str]))

(defn- rel-dir [path] (let [i (str/last-index-of path "/")] (if i (subs path 0 i) "")))
(defn file-name [path] (let [i (str/last-index-of path "/")] (if i (subs path (inc i)) path)))

(defn dir-tree
  "Files under a root dir → nested {:dirs {name subtree} :files [file...]}"
  [root files]
  (reduce (fn [t f]
            (let [rel (subs (:path f) (inc (count root)))
                  parts (str/split rel #"/")
                  dirs (butlast parts)]
              (update-in t (vec (mapcat (fn [d] [:dirs d]) dirs))
                         (fn [node] (update (or node {}) :files (fnil conj []) f)))))
          {} files))

(defn- compact
  "Merge chains of directories that contain exactly one subdirectory and no files."
  [name node]
  (let [{:keys [dirs files]} node]
    (if (and (empty? files) (= 1 (count dirs)))
      (let [[n sub] (first dirs)] (compact (str name "/" n) sub))
      [name node])))

(defn- form-row-children [pkey path form]
  (cond
    (= :comment (:kind form))
    (for [c (:children form)]
      {:id [:child pkey path (:index form) (:index c)] :type :child :child c :form form :path path})
    (and (= :reader-conditional (:kind form)) (> (count (:defs form)) 1))
    (for [[i d] (map-indexed vector (:defs form))]
      {:id [:split pkey path (:index form) i] :type :split :def d :form form :path path})))

(defn expandable-form? [form]
  (or (and (= :comment (:kind form)) (seq (:children form)))
      (and (= :reader-conditional (:kind form)) (> (count (:defs form)) 1))))

(defn visible-rows
  [{:keys [outline files expanded repo namespace-view? compact-folders?]}]
  (let [out (transient [])
        emit! (fn [row depth] (conj! out (assoc row :depth depth)))
        open? (fn [id] (contains? expanded id))]
    (letfn [(file-rows [pkey f depth label]
              (let [id [:file pkey (:path f)]
                    data (get files [repo (:path f)])]
                (emit! {:id id :type :file :file f :label label :expandable? (pos? (:form-count f 0))
                        :loaded? (some? data)} depth)
                (when (and (open? id) data)
                  (doseq [form (:forms data)]
                    (let [fid [:form pkey (:path f) (:index form)]]
                      (emit! {:id fid :type :form :form form :path (:path f) :file-data data
                              :expandable? (expandable-form? form)} (inc depth))
                      (when (open? fid)
                        (doseq [c (form-row-children pkey (:path f) form)]
                          (emit! (assoc c :file-data data) (+ 2 depth)))))))))
            (dir-rows [pkey prefix [name node] depth]
              (let [[name node] (if compact-folders? (compact name node) [name node])
                    full (str prefix "/" name)
                    id [:dir pkey full]]
                (emit! {:id id :type :dir :label name :dir full :expandable? true} depth)
                (when (open? id)
                  (tree-rows pkey full node (inc depth)))))
            (tree-rows [pkey prefix node depth]
              (doseq [d (sort-by key (:dirs node))]
                (dir-rows pkey prefix d depth))
              (doseq [f (sort-by :path (:files node))]
                (file-rows pkey f depth (file-name (:path f)))))]
      (doseq [p (:projects outline)]
        (let [pid [:project (:key p)]]
          (emit! {:id pid :type :project :project p :expandable? true} 0)
          (when (open? pid)
            (doseq [{:keys [dir files] :as path} (:paths p)]
              (let [id [:path (:key p) dir]]
                (emit! {:id id :type :path :path path :project p :expandable? (seq files)} 1)
                (when (open? id)
                  (if namespace-view?
                    (doseq [f (sort-by #(or (:ns %) (:path %)) files)]
                      (file-rows (:key p) f 2 (or (:ns f) (file-name (:path f)))))
                    (tree-rows (:key p) dir (dir-tree dir files) 2)))))))))
    (persistent! out)))

(defn ancestors-of
  "Ids to expand so that a file (and optionally a form) becomes visible, using the
  first project/path that contains the file."
  [outline path compact-folders?]
  (first
   (for [p (:projects outline)
         {:keys [dir files]} (:paths p)
         :when (some #(= path (:path %)) files)]
     (let [pkey (:key p)
           tree (dir-tree dir files)
           rel-parts (str/split (subs (rel-dir path) (min (count (rel-dir path)) (inc (count dir)))) #"/")
           rel-parts (remove str/blank? rel-parts)
           ;; walk the (possibly compacted) dir rows down to the file
           dir-ids (loop [node tree prefix dir parts rel-parts acc []]
                     (if (empty? parts)
                       acc
                       (let [n (first parts)
                             sub (get-in node [:dirs n])
                             [cname cnode] (if compact-folders? (compact n sub) [n sub])
                             consumed (count (str/split cname #"/"))
                             full (str prefix "/" cname)]
                         (recur cnode full (drop consumed parts) (conj acc [:dir pkey full])))))]
       {:pkey pkey
        :ids (into [[:project pkey] [:path pkey dir]] dir-ids)
        :file-id [:file pkey path]}))))
