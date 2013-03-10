(ns algorithms.core
  (:import (java.io BufferedReader FileReader))
  (:use clojure.java.io))

(defn dirname [path]
  (.getParent (java.io.File. path)))

(defn expand-path [path]
  (.getCanonicalPath (java.io.File. path)))

(defn data-path [algo-dir file-name]
  (clojure.string/join (System/getProperty "file.separator")
                       (vector (System/getProperty "user.dir") "data" algo-dir file-name)))

(defn load-graph [algo-dir file-name]
  (with-open [rdr (BufferedReader. (FileReader. (data-path algo-dir file-name)))]
    (reduce
     (fn [graph line]
       (let [[vertex node] (clojure.string/split line #"\s+")]
         (assoc graph vertex (-> (get graph vertex []) (conj node)))))
     {}
     (line-seq rdr))))

(defn load-weighted-graph [algo-dir file-name]
  (with-open [rdr (BufferedReader. (FileReader. (data-path algo-dir file-name)))]
    (reduce
     (fn [graph line]
       (let [[vertex & nodes-and-weights] (clojure.string/split line #"\s+")]
         (assoc graph vertex
           (merge (get graph vertex {})
                  (reduce (fn [h weighted-edge]
                            (let [[v w] (clojure.string/split weighted-edge #",")]
                              (assoc h v w)))
                          {}
                          nodes-and-weights)))
         ))
     {}
     (line-seq rdr))))

(defn load-graph-and-stats [algo-dir file-name]
  (map persistent! (with-open [rdr (BufferedReader. (FileReader. (data-path algo-dir file-name)))]
     (reduce
      (fn [[graph transposed vertices] line]
        (let [[u v] (clojure.string/split line #"\s+")]
          (vector
           (assoc! graph u (-> (get graph u []) (conj v)))
           (assoc! transposed v (-> (get transposed v []) (conj u)))
           (-> (conj! vertices u) (conj! v)))))
      [(transient {}) (transient {}) (transient #{})]
      (line-seq rdr)))))

(defn int-graph [G]
  (reduce
   (fn [G' [v edges]]
     (assoc G' (Integer. v) (vec (map #(Integer. %) edges))))
   {}
   G))

(defn transpose [G]
  (reduce
   (fn [G' [u vs]]
     (let [transposed-vertex
             (reduce
              (fn [transposed-u v]
                (assoc transposed-u v [u]))
              {}
              vs)]
       (merge-with into G' transposed-vertex)))
   {}
   G))
