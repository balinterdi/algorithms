; see https://class.coursera.org/algo-003/forum/thread?thread_id=490 for test cases
(ns algorithms.strongly-connected-components
  (:use clojure.test)
  (:use clojure.java.io)
  (:import (java.io BufferedReader FileReader)))

;; StackOverflow on tail-recursive function?
;; http://stackoverflow.com/questions/4249926/stackoverflowerror-on-tail-recursive-function
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

(defn load-graph-and-stats [file-name]
  (map persistent! (with-open [rdr (BufferedReader. (FileReader. (data-path file-name)))]
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

(defn dfs
  ([G u]
     (dfs G [u] #{u} []))
  ([G u explored]
     (dfs G [u] explored []))
  ([G [v & vs] explored path]
     (if (nil? v)
       (reverse path)
       (recur G
              (remove explored (into (get G v) vs))
              (conj explored v)
              (cons v path)))
     ))

;; Running (dfs-by-finishing-times tposed "51914" #{})) on the huge graph
;; explores ~10.000 nodes in 57 seconds. Using transients where possible
(defn dfs-by-finishing-times
  ([G u]
     ;;(dfs-by-finishing-times G [u] (transient #{u}) (transient []))
     (dfs-by-finishing-times G [u] (transient #{u}) [] 0)
     )
  ([G u explored]
     ;;     (dfs-by-finishing-times G [u] (transient explored) (transient []))
     (dfs-by-finishing-times G [u] (transient explored) [] 0)
     )
  ([G [v & vs :as stack] explored path iter-cnt]
     ;;(println "stack: " (count stack) " explored: " (count explored) "path: " (count path))
     (do
       (when (zero? (rem iter-cnt 1000))
         (println "Iter: " iter-cnt "Explored count: " (count explored)))
       (if (> (count explored) 10000)
         (and (println "Exiting after " (count explored) " explored nodes") path)
         (if (seq stack)
          (let [neighbors (persistent!
                           (reduce
                            (fn [c u] (if (explored u) c (conj! c u)))
                            (transient [])
                            (G v)))]
            (if (empty? neighbors)
              (recur G vs (conj! explored v) (conj path v) (inc iter-cnt))
              (recur G (into neighbors (conj vs v)) (conj! explored v) path (inc iter-cnt))))
          path)))
     ))

;;(finishing-times (transpose test-case-5) (vertices test-case-5))
;;(finishing-times (transpose simple-graph) (vertices simple-graph))

(defn vertices [G]
  (into
    (set (keys G))
    (reduce into (vals G))))

(defn finishing-times [G vertices]
  "The first pass of Kosaraju's algorithm.
   Scan the transpose graph of G, and mark the finishing time for each.
   G should already be the transposed graph"
  (loop [[u & vs] (seq vertices), explored #{}, finished []]
      (if (nil? u)
       (distinct finished)
       (let [;_ (println u)
             path (dfs-by-finishing-times G u explored)
             new-explored (into explored (set path))]
          (recur (remove new-explored vs)
                 new-explored
                 (into finished path))))))

;;(finishing-times simple-graph)

(defn leaders [G vertices-by-finishing-time]
  "Second pass of Kosaraju's algorithm.
   Init depth first searches in order of the vertices obtained in the 1st pass"
  (loop [[v & vs] (reverse vertices-by-finishing-time), explored #{}, leaders {}]
    (if (nil? v)
      leaders
      (let [path (dfs G v explored)
            new-explored (into explored (set path))]
        (recur (remove new-explored vs)
               new-explored
               (merge leaders {v (distinct path)}))))))

(defn scc [G transposed vertices]
  (leaders G (finishing-times transposed vertices)))

(defn scc-sizes [G transposed vertices]
  (take 5 (sort #(< %2 %1) (map count (vals (scc G transposed vertices))))))

(def simple-graph (load-graph "scc_simple_graph_1.txt"))

(def simple-graph-2 (int-graph (load-graph "scc_simple_graph_2.txt")))
(def simple-graph-3 (int-graph (load-graph "scc_simple_graph_3.txt")))
(def simple-graph-4 (int-graph (load-graph "scc_simple_graph_4.txt")))

;; See https://class.coursera.org/algo-003/forum/thread?thread_id=490 for the test-case graphs
(def test-case-5 (int-graph (load-graph "scc_test_case_5.txt")))
(def test-case-7 (int-graph (load-graph "scc_test_case_7.txt")))

;;(def huge-graph (int-graph (load-graph "SCC.txt")))

;;(def transposed (transpose simple-graph))

;;(scc simple-graph)
;;(scc simple-graph-4)
;;(scc-sizes simple-graph-4)
;;(scc test-case-5)
;;(scc-sizes test-case-7)
;;(scc-sizes simple-graph (transpose simple-graph) (vertices simple-graph))

;;(scc test-case-5)

(deftest test-finishing-times
  (let [ft (finishing-times simple-graph)]
    (is (or (= ft [3 5 2 8 6 9 1 4 7])
            (= ft [5 2 8 3 6 9 1 4 7])))))

(comment (deftest test-dfs-by-finishing-times
   (is (= (dfs-by-finishing-times transposed 9) [3 5 2 8 6 9]))))

(deftest test-dfs
  (is (= (dfs simple-graph 9) [9 3 6 7 1 4])))

(deftest test-scc
  (is (= (scc simple-graph) {8 [8 5 2], 9 [9 3 6], 7 [7 1 4]})))

(deftest test-scc-sizes
  (is (= (scc-sizes {1 [3], 2 [1 3]}) [1 1 1]))
  (is (= (scc-sizes {3 [1], 2 [1 3]}) [1 1 1])))

;(run-all-tests #"algorithms.strongly-connected-components")
