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

(comment (defn dfs
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
      )))


(defn finishing-times [G vertices]
  "The first pass of Kosaraju's algorithm.
   Scan the transpose graph of G, and mark the finishing time for each.
   G should already be the transposed graph"
  (let [V (count vertices)
        explored (boolean-array V)
        finished (long-array V)
        ;;amalloy: or use java.util.Arrays/copyOfRange if you don't need to allocate the object yourself
        set-finished (fn [offset values]
                       (System/arraycopy (into-array Long/TYPE values) 0
                                         finished offset
                                         (count values)))
        set-explored (fn [i]
                       (aset explored (dec i) true))
        explored? (fn [i] (aget ^booleans explored (dec i)))
        dfs-by-finishing-times
        (fn [u]
          (loop [[v & vs :as stack] (list u), lhalf [], rhalf [],  iter-cnt 0]
            (if v
              (let [neighbors (remove explored? (G v))]
                (cond
                 (explored? v) (recur vs lhalf rhalf (inc iter-cnt))
                 (empty? neighbors)
                   (do
                     (set-explored v)
                     (recur vs (conj lhalf v) rhalf (inc iter-cnt)))
                 :else
                   (do
                     (set-explored v)
                     (recur (reduce (fn [stack e] (cons e stack)) vs neighbors)
                          lhalf
                          (cons v rhalf)
                          (inc iter-cnt)))))
              (into lhalf rhalf))))]
    (loop [[u & vs :as stack] (seq vertices)
           finished-count 0]
     (if (== V finished-count)
       (into [] finished)
       (if (explored? u)
          (recur vs finished-count)
          (let [path (dfs-by-finishing-times u)]
            (do
              (set-finished finished-count path)
              (recur vs (+ finished-count (count path))))))
       ))))

;;(finishing-times simple-graph)

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
      (do
        (when (zero? (rem finished-count 1e4))
          (println "Finished count: " finished-count))
        (if (nil? v)
        scc-sizes
        (if (explored? v)
          (recur vs scc-sizes finished-count)
          (let [path (dfs [v] [])]
            (recur vs
                   (conj scc-sizes (count path))
                   (+ finished-count (count path)))))
        )))))

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
