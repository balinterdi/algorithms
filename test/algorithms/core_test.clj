(ns algorithms.core-test
  (:use clojure.test
        algorithms.core))

(deftest test-loading-weighted-graph
  (testing "Load weighted graph from file"
    (is (load-weighted-graph "dijkstra_simple_test_case.txt")
        {"1" {"2" "1", "3" "4"}, "2" {"3" "2", "4" "6"}, "3" {"4" "3"}})))
