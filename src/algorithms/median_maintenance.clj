;; Implements a solution for the median maintenance problem
;; see https://class.coursera.org/algo-003/forum/thread?thread_id=669 for test cases
(ns algorithms.median-maintenance
  (:require  [algorithms.core :as core])
  (:use      [data-structures.heap.core]))

(defn load-numbers [file-name]
  (core/load-ints-from-file "median-maintenance" file-name))

(defn medians [numbers]
  (let [cmp-h-high (fn [x y] (- (compare x y)))
        cmp-h-low compare
        median-index (fn [k] (if (odd? k)
                              (quot (inc k) 2)
                              (quot k 2)))]
      (loop [[x & xs] numbers
          h-high (heapify [] cmp-h-high)
          h-low  (heapify [] cmp-h-low)
          medians []
          i 1]
        (if x
          (let [
              heap-for-x
              (if (or (zero? (count h-low))
                      (< x (heap-peek h-low)))
                :low
                :high)
              hl
              (if (= heap-for-x :low)
                (heap-add h-low x cmp-h-low)
                h-low)
              hh
              (if (= heap-for-x :high)
                (heap-add h-high x cmp-h-high)
                h-high)
              to-extract (if (= heap-for-x :low)
                          (when (= 2 (- (count hl) (count hh)))
                            (heap-peek hl))
                          (when (= 2 (- (count hh) (count hl)))
                            (heap-peek hh)))
              hl (if to-extract
                   (if (= heap-for-x :low)
                     (heap-remove hl cmp-h-low)
                     (heap-add hl to-extract cmp-h-low))
                   hl)
              hh (if to-extract
                   (if (= heap-for-x :high)
                     (heap-remove hh cmp-h-high)
                     (heap-add hh to-extract cmp-h-high))
                   hh)
              med (if (>= (count hl) (median-index i))
                    (heap-peek hl)
                    (heap-peek hh))
                ]
            (recur xs hh hl (conj medians med) (inc i)))
            medians))))


(defn median-sum [numbers]
  (reduce + (medians numbers)))
