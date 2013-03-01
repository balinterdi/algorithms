(ns algorithms.strongly-connected-components
  (:use clojure.test)
  (:use clojure.java.io)
  (:import (java.io BufferedReader FileReader)))

(defn dirname [path]
  (.getParent (java.io.File. path)))

(defn expand-path [path]
  (.getCanonicalPath (java.io.File. path)))

(defn data-path [file-name]
  (clojure.string/join (System/getProperty "file.separator")
                       (vector (System/getProperty "user.dir") "data" file-name)))
(defn load-graph [file-name]
  (with-open [rdr (BufferedReader. (FileReader. (data-path file-name)))]
    (reduce
     (fn [graph line]
       (let [[vertex node] (clojure.string/split line #"\s+")]
         (assoc graph vertex (-> (get graph vertex []) (conj node)))))
     {}
     (line-seq rdr))))

(defn int-graph [G]
  (reduce
   (fn [G' [v edges]]
     (println v edges)
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
       (merge-with concat G' transposed-vertex)))
   {}
   G))

(def simple-graph (int-graph (load-graph "scc_test_case_1.txt")))
;(dfs (transpose simple-graph) 9)
(def transposed (transpose simple-graph))

(defn dfs
  ([G u]
     (dfs G [u] #{u}))
  ([G [v & vs] explored]
     ;(println "G: " G "v: " (class v) " neighbors: " (get G v) " explored: " explored)
     (if (nil? v)
       []
       (cons v
             (dfs G
                  (remove explored (concat (get G v) vs))
                  (conj explored v))))
     ))

(defn dfs-by-finishing-times
  ([G u]
     (dfs-by-finishing-times G [u] #{u}))
  ([G [v & vs] explored]
     ;(println "G: " G "v: " (class v) " neighbors: " (get G v) " explored: " explored)
     (if (nil? v)
       []
       (let [neighbors (remove explored (get G v))]
         (if (empty? neighbors)
           (cons v (dfs-by-finishing-times G vs explored))
           (conj
             (vec
               (dfs-by-finishing-times G (concat neighbors vs) (conj explored v)))
             v))))
     ))

(dfs-by-finishing-times transposed 9)

(defn finishing-times [G]
  "The first pass of Kosaraju's algorithm.
   Scan the transpose graph of G, and mark the finishing time for each"
  (let [G' (transpose G)
        vertices (sort #(< %2 %1) (keys G))]
    (loop [[u & vs] vertices, explored #{}, finished []]
      (cond
       (nil? u) finished
       :else (let [path (dfs-by-finishing-times G' [u] explored)
                   new-explored (into explored (set path))]
          (recur (remove new-explored vs)
                 new-explored
                 (concat finished path)))))
    ))

(def simple-graph (int-graph (load-graph "scc_test_case_1.txt")))
;(dfs (transpose simple-graph) 9)
(def transposed (transpose simple-graph))
;(finishing-times simple-graph)

(deftest test-finishing-times
  (let [ft (finishing-times simple-graph)]
    (is (or (= ft [3 5 2 8 6 9 1 4 7])
            (= ft [5 2 8 3 6 9 1 4 7])))))

(run-all-tests #"algorithms.strongly-connected-components")
