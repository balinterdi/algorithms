(ns algorithms.strongly-connected-components-test
  (:require [algorithms.core :as core])
  (:use clojure.test
        algorithms.strongly-connected-components))

(def simple-graph (load-graph "scc_simple_graph_1.txt"))

;; See https://class.coursera.org/algo-003/forum/thread?thread_id=490 for the test-case graphs

;;(def simple-graph-2 (load-graph "scc_simple_graph_2.txt"))
;;(def simple-graph-3 (load-graph "scc_simple_graph_3.txt"))
;;(def simple-graph-4 (load-graph "scc_simple_graph_4.txt"))

;;(def test-case-5 (load-graph "scc_test_case_5.txt"))
;;(def test-case-7 (load-graph "scc_test_case_7.txt"))

(deftest test-finishing-times
  (let [ft (finishing-times (core/transpose simple-graph) (vertices simple-graph))]
    (is (or (= ft [3 5 2 8 6 9 1 4 7])
            (= ft [5 2 8 3 6 9 1 4 7])))))

(comment (deftest test-dfs-by-finishing-times
   (is (= (dfs-by-finishing-times (core/transpose simple-graph) "9") (map str [3 5 2 8 6 9])))))

(deftest test-dfs
  (is (= (dfs simple-graph 9) (map str [9 3 6 7 1 4]))))

(deftest test-scc
  (is (= (core/int-graph (scc simple-graph)) {8 [8 5 2], 9 [9 3 6], 7 [7 1 4]})))

(deftest test-scc-sizes
  (is (= (scc-sizes {1 [3], 2 [1 3]}) [1 1 1]))
  (is (= (scc-sizes {3 [1], 2 [1 3]}) [1 1 1])))