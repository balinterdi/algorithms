; see https://class.coursera.org/algo-003/forum/thread?thread_id=490 for test cases
(ns algorithms.strongly-connected-components
  (:require  [algorithms.core :as core])
  (:use clojure.test))

;; StackOverflow on tail-recursive function?
;; http://stackoverflow.com/questions/4249926/stackoverflowerror-on-tail-recursive-function

(defn load-graph [file-name]
  (core/load-graph "scc" file-name))

(defn load-graph-and-stats [file-name]
  (core/load-graph-and-stats "scc" file-name))

(defn vertices [G]
  (into
    (set (keys G))
    (reduce into (vals G))))

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

;; The algorithm (when it prints every 100.000 iteration) runs the following
;; form in 22.933 secs:
;; (def ftimes (dfs-by-finishing-times T "51914" #{}))
(defn dfs-by-finishing-times
  ([G u]
     ;;(dfs-by-finishing-times G [u] (transient #{u}) (transient []))
     (dfs-by-finishing-times G u #{}))
  ([G u explored]
     ;;(println "stack: " (count stack) " explored: " (count explored) "path: " (count path))
     (loop [[v & vs :as stack] (list u), explored (transient explored), lhalf [], rhalf [],  iter-cnt 0]
       (do
       (if (> (count explored) (Integer/MAX_VALUE))
         (and (println "Exiting after " (count explored) " explored nodes") (concat lhalf rhalf))
         (if (seq stack)
          (let [neighbors (persistent!
                           (reduce
                            (fn [c u] (if (explored u) c (conj! c u)))
                            (transient [])
                            (G v)))]
            (cond
             (explored v) (recur vs explored lhalf rhalf (inc iter-cnt))
             (empty? neighbors) (recur vs (conj! explored v) (conj lhalf v) rhalf (inc iter-cnt))
             :else (recur (reduce (fn [stack e] (cons e stack)) vs neighbors)
                     (conj! explored v)
                     lhalf
                     (cons v rhalf)
                     (inc iter-cnt))))
          (concat lhalf rhalf)))))
     ))

(defn finishing-times [G vertices]
  "The first pass of Kosaraju's algorithm.
   Scan the transpose graph of G, and mark the finishing time for each.
   G should already be the transposed graph"
  ;;FIXME: Maybe passing several tens or hundreds of MBs of data to dfs is not such a good idea?
  (loop [[u & vs :as stack] (seq vertices)
          explored #{},
          finished []]
     (if (nil? u)
       finished
       (let [path (dfs-by-finishing-times G u explored)
             new-explored (into explored path)]
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
