(ns algorithms.dijkstra-shortest-path-test
  (:use clojure.test
        algorithms.dijkstra-shortest-path))


(deftest test-shortest-path-in-simple-graph
  (testing "Find all shortest path from source in very simple graph"
    (let [G (load-weighted-graph "dijkstra_simple_test_case.txt")]
      (is (= (shortest-paths G "1")
             {"1" 0, "2" 1, "3" 3, "4" 6})))))

(deftest test-shortest-path-in-simple-graph
  (testing "Find all shortest path from source in very simple graph"
    (let [G (load-weighted-graph "dijkstra_test_case_4.txt")]
      (is (= (shortest-paths G "1")
            {"1" 0, "2" 7, "3" 9, "4" 20, "5" 20, "6" 11})))))
