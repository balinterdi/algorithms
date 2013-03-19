(ns algorithms.strongly-connected-components-test
  (:require [algorithms.core :as core])
  (:use clojure.test
        algorithms.strongly-connected-components))

;;(def simple-graph (load-graph "scc_simple_graph_1.txt"))

;; See https://class.coursera.org/algo-003/forum/thread?thread_id=490 for the test-case graphs

;;(def simple-graph-2 (load-graph "scc_simple_graph_2.txt"))
;;(def simple-graph-3 (load-graph "scc_simple_graph_3.txt"))
;;(def simple-graph-4 (load-graph "scc_simple_graph_4.txt"))

;;(def test-case-5 (load-graph "scc_test_case_5.txt"))
;;(def test-case-7 (load-graph "scc_test_case_7.txt"))

(deftest test-finishing-times
  (testing "Finishing times are rightly calculated for a simple example"
    (let [simple-graph (core/int-graph (load-graph "scc_simple_graph_1.txt"))
          ft (finishing-times (core/transpose simple-graph) (core/sort-decreasing (vertices simple-graph)))]
       (is (or (= ft [3 5 2 8 6 9 1 4 7])
               (= ft [5 2 8 3 6 9 1 4 7])
               (= ft [5 3 2 8 6 9 1 4 7]))))))

(deftest test-scc-sizes-smallest
  (is (= (scc-sizes
          (core/int-graph (load-graph "scc_simple_graph_1.txt")))
       [3 3 3])))

(deftest test-scc-sizes-small
   (is (= (scc-sizes {1 [3], 2 [1 3]}) [1 1 1]))
   (is (= (scc-sizes {3 [1], 2 [1 3]}) [1 1 1])))

(deftest test-scc-sizes-bigger
  (testing "SCC sizes are rightly calculated for a graph with a loop"
    (let [g (core/int-graph (load-graph "scc_test_case_5.txt"))]
      (is (= (scc-sizes g) [6 3 2 1])))))

(deftest test-scc-sizes-complex-big-with-loop
  (testing "SCC sizes are rightly calculated for a rather big and complex graph with a loop"
    (let [g (core/int-graph (load-graph "test_case_6.txt"))]
      (is (= (scc-sizes g) [35 7 1 1 1])))))

(deftest test-scc-sizes-complex-big
  (testing "SCC sizes are rightly calculated for a rather big and complex graph"
    (let [g (core/int-graph (load-graph "scc_test_case_7.txt"))]
      (is (= (scc-sizes g) [36 7 1 1 1])))))

(deftest test-scc-sizes-big
  (is (= (scc-sizes
          (core/int-graph (load-graph "test_case_8.txt")))
       [8 5 2 1])))
