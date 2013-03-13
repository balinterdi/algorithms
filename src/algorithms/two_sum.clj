;; Implements a solution for the 2-sum problem
;; see https://class.coursera.org/algo-003/forum/thread?thread_id=669 for test cases
(ns algorithms.strongly-connected-components
  (:require  [algorithms.core :as core]))

(defn load-set [file-name]
  (core/load-int-set "two-sum" file-name))

(defn sum-to [numbers t]
  (some
   (fn [n]
     (and (numbers (- t n)) (not= n (- t n))))
   numbers))

(sum-to #{1 2 3} 4)

(defn two-sum-count [numbers targets]
  (count (filter (partial sum-to numbers) targets)))

(two-sum-count #{1 2 3} (range 1 11))
(two-sum-count (load-set "test_case_1.txt") (range 30 61)) ;; should == 9
(two-sum-count (load-set "test_case_1.txt") (range 60 101)) ;; should == 28
(two-sum-count (load-set "HashInt.txt") (range 2500 4001)) ;; 1477
