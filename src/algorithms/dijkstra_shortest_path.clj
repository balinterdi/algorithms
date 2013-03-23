(ns algorithms.dijkstra-shortest-path
  (:require  [algorithms.core :as core :only [load-weighted-graph]] ))

(def infinity 1000000) ; could be Integer/MAX_VALUE

(defn load-weighted-graph [file-name]
  (core/load-weighted-graph "dijkstra" file-name))

(defn vertices [G]
  (reduce (fn [vs [v edges]]
            (conj (into vs (keys edges)) v))
    #{}
    G))

(defn edges [G]
  (reduce (fn [M [u edges-u]]
            (concat M (reduce (fn [es [v w]]
                                (conj es [[u v] (Integer. w)]))
                       []
                       edges-u)))
    []
    G))

;;(def dijkstra-simple (load-weighted-graph "dijkstra_simple_test_case.txt"))
;;TODO: use a heap to store unexplored vertices to speed things up
(defn shortest-paths
  ([G s] (shortest-paths G s {s 0}
                        (reduce (fn [h v]
                                  (assoc h v infinity))
                                {}
                                (disj (vertices G) s))
                        (set (edges G))
    ))
  ([G s found searching edges]
     (loop [found found, searching searching, edges edges]
       (if (empty? searching)
       found
       (let [V (set (keys found))
             X (set (keys searching))
             possible-edges
             (filter
              (fn [[[u v] w]]
                (and (V u) (X v)))
              edges)
             next-edge
             (reduce (fn [[_ min-score :as min-edge] [[u v :as e] w]]
                       (let [greedy-score (+ (found u) w)]
                         (if (< greedy-score min-score)
                         [e greedy-score]
                         min-edge)))
                     [[nil nil] infinity]
                     possible-edges)
             [[u v] greedy-score] next-edge]
         (recur (assoc found v greedy-score)
                (dissoc searching v)
                (disj edges next-edge))
         ))))
  )

(defn distances-to [G s nodes]
  (map (shortest-paths G s) nodes))

(comment (defn homework-assignment []
   (clojure.string/join "," (distances-to bigger-graph "1" (map str [7,37,59,82,99,115,133,165,188,197])))))
