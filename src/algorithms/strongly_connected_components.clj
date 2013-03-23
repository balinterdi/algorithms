; see https://class.coursera.org/algo-003/forum/thread?thread_id=490 for test cases
(ns algorithms.strongly-connected-components
  (:require  [algorithms.core :as core])
  (:use clojure.test))

;; StackOverflow on tail-recursive function?
;; http://stackoverflow.com/questions/4249926/stackoverflowerror-on-tail-recursive-function

(defn load-graph [file-name]
  (core/load-graph "scc" file-name))

(defn load-huge-graph [file-name]
  (core/load-huge-graph "scc" file-name))

(defn vertices [G]
  (into
    (set (keys G))
    (reduce into (vals G))))

(defn finishing-times [G vertices]
  "The first pass of Kosaraju's algorithm.
   Scan the transpose graph of G, and mark the finishing time for each.
   G should already be the transposed graph"
  (let [V (count vertices)
        finished (long-array V)
        explored (boolean-array V)
        finishing-time (atom 0)
        set-explored (fn [i] (aset explored (dec i) true))
        explored? (fn [i] (let [e (aget ^booleans explored (dec i))] e))
        dfs-by-finishing-times
        (fn dfs [^long u]
          (set-explored u)
          (doseq [v (G u)]
            (when-not (explored? v)
              (dfs v)))
          (aset finished @finishing-time u)
          (swap! finishing-time inc))]
    (loop [[v & vs] (seq vertices)]
      (if v
        (do
          (when-not (explored? v)
            (dfs-by-finishing-times v))
          (recur vs))
        (vec finished)))
    ))

(defn leaders [G vertices-by-finishing-time]
  "Second pass of Kosaraju's algorithm.
   Init depth first searches in order of the vertices obtained in the 1st pass"
  (let [V (count vertices-by-finishing-time)
        explored (boolean-array V)
        set-explored (fn [i]
                       (aset explored (dec i) true))
        explored? (fn [i] (aget ^booleans explored (dec i)))
        dfs
        (fn [[v & vs] path]
          (cond
           (nil? v) (reverse path)
           (explored? v) (recur vs path)
           :else
           (do
             (set-explored v)
             (recur
                (reduce (fn [stack e] (cons e stack)) vs (G v))
                (cons v path)))))
        ]
    (loop [[v & vs] (reverse vertices-by-finishing-time), scc-sizes [], finished-count 0]
      (if (nil? v)
        scc-sizes
        (if (explored? v)
          (recur vs scc-sizes finished-count)
          (let [path (dfs [v] [])]
            (recur vs
                   (conj scc-sizes (count path))
                   (+ finished-count (count path)))))))
      ))

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
     (take 5 (sort #(< %2 %1) (scc G transposed vertices)))))



;(run-all-tests #"algorithms.strongly-connected-components")
