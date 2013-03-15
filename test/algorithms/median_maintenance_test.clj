(ns algorithms.median-maintenance-test
  (:use clojure.test
        algorithms.median-maintenance))

(deftest test-simple-median-sum
  (testing "The sum of the i-th medians is computed correctly"
    (is (= (median-sum [9,6,14,19,8,4])
           50))))
