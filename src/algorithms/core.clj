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

(defn load-from-file [algo-dir file-name reduce-fn init-val]
  (with-open [rdr (BufferedReader. (FileReader. (data-path algo-dir file-name)))]
    (reduce reduce-fn init-val (line-seq rdr))))

(defn load-graph [algo-dir file-name]
  (load-from-file
    algo-dir
    file-name
    (fn [graph line]
       (let [[vertex node] (clojure.string/split line #"\s+")]
         (assoc graph vertex (-> (get graph vertex []) (conj node)))))
    {}))

(defn load-weighted-graph [algo-dir file-name]
  (load-from-file
    algo-dir
    file-name
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
    {}))

;;(load-weighted-graph "dijkstra" "dijkstra_test_case_1.txt")

;;TODO: Should be called load-huge-graph
;;TODO: Don't use transients, use java arrays
(defn load-graph-and-stats [algo-dir file-name]
  (map persistent!
    (load-from-file
     algo-dir
     file-name
     (fn [[graph transposed vertices] line]
       (let [[u v] (map #(Integer. ^String %) (clojure.string/split line #"\s+"))]
         (vector
          (assoc! graph u (-> (get graph u []) (conj v)))
          (assoc! transposed v (-> (get transposed v []) (conj u)))
          (-> (conj! vertices u) (conj! v)))))
     [(transient {}) (transient {}) (transient #{})])))

(defn int-graph [G]
  (reduce
   (fn [G' [v edges]]
     (assoc G' (Integer. ^String v) (vec (map #(Integer. ^String %) edges))))
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

(defn sort-decreasing [coll]
  (sort (fn [n1 n2] (> (Integer. n1) (Integer. n2))) coll))

(defn load-int-set [algo-dir file-name]
  (load-from-file
   algo-dir
   file-name
   (fn [hash line]
     (conj hash (Integer. ^String line)))
   #{}))

(defn load-ints-from-file [algo-dir file-name]
  (load-from-file
   algo-dir
   file-name
   (fn [numbers line]
     (conj numbers (Integer. ^String line)))
   []))
