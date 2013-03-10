; see https://class.coursera.org/algo-003/forum/thread?thread_id=490 for test cases
(ns algorithms.strongly-connected-components
  (:require  [algorithms.core :as core :only [load-graph load-graph-and-stats transpose]])
  (:use clojure.test))

;; StackOverflow on tail-recursive function?
;; http://stackoverflow.com/questions/4249926/stackoverflowerror-on-tail-recursive-function

(defn load-graph [file-name]
  (core/load-graph "scc" file-name))

(defn load-graph-and-stats [file-name]
  (core/load-graph-and-stats "scc" file-name))

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
;; where the stack was represented by a vector, explores ~10.000 nodes in 60 seconds.
;; When the stack became a list, running time for exploring the same number of nodes
;; came down to ~250 msecs, a 240x speedup!
;; However, there is still FIXME:1 to be sorted out
;;TODO: finish using a list for stack. I'm not sure if this can be achieved by using recur
(defn dfs-by-finishing-times
  ([G u]
     ;;(dfs-by-finishing-times G [u] (transient #{u}) (transient []))
     (dfs-by-finishing-times G u #{u}))
  ([G u explored]
     ;;(println "stack: " (count stack) " explored: " (count explored) "path: " (count path))
     (loop [[v & vs :as stack] (list u), explored (transient explored), path (transient []), iter-cnt 0]
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
              (recur vs (conj! explored v) (conj! path v) (inc iter-cnt))
              ;;FIXME:1 v should be added back to the end of the stack. What's more, in an efficient manner.
              (recur (reduce (fn [stack e] (cons e stack)) vs neighbors) (conj! explored v) path (inc iter-cnt))))
          (persistent! path)))))
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

(defn scc
  ([G]
     (scc G (core/transpose G) (vertices G)))
  ([G transposed vertices]
     (leaders G (finishing-times transposed vertices)))
  )

(defn scc-sizes
  ([G]
     (scc-sizes G (core/transpose G) (vertices G)))
  ([G transposed vertices]
     (take 5 (sort #(< %2 %1) (map count (vals (scc G transposed vertices))))))
  )



;(run-all-tests #"algorithms.strongly-connected-components")
