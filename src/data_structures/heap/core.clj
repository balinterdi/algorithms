;; Copied and slightly modified from https://github.com/Burn0ut07/Heap
(ns data-structures.heap.core
  (:import (clojure.lang Seqable IPersistentCollection ISeq IPersistentStack)))

(defn- vswap [v a b]
  (let [temp (v a)]
    (-> v (assoc a (v b)) (assoc b temp))))

(defn- vswap! [v a b]
  (let [temp (v a)]
    (-> v (assoc! a (v b)) (assoc! b temp))))

(defn heapify
  "Returns a vector satisfying the heap property, given a comparison function.
  Defaults to use compare."
  ([v f]
     (letfn [(heapifier ;really kludgey needs work
              [v i]     ;perhaps abstract out into private method
              (let [l (inc (* i 2)), r (inc l), vlen (count v)
                    li (if (and (< l vlen) (pos? (f (v l) (v i)))) l i)
                    largest (if (and (< r vlen) (pos? (f (v r) (v li)))) r li)]
                (if (not= largest i)
                  (recur (vswap! v i largest) largest), v)))]
       (loop [i (int (/ (count v) 2)), heap (transient v)]
         (if (neg? i) (persistent! heap) (recur (dec i) (heapifier heap i))))))
  ([v]
     (heapify v compare)))

(defn heap-add
  "Adds an element to a heap. Returns a vector satisfying the heap
  property with e added, given a comparison function. Defaults to
  use compare."
  ([heap e f]
     (letfn [(sift-up
              [heap i]
              (let [p (int (/ (dec i) 2))]
                (if (and (pos? i) (pos? (f (heap i) (heap p))))
                  (recur (vswap heap i p) p), heap)))]
       (sift-up (conj heap e) (count heap))))
  ([heap e]
     (heap-add heap e compare)))

(defn heap-remove
  "Removes the highest priority element from the heap. Returns a
  vector satisfying the heap property given a comparison function.
  Defaults to use compare."
  ([heap f]
     (letfn [(sift-down ;really kludgey needs work
              [heap i size]
              (let [l (inc (* i 2)), r (inc l)
                    li (if (and (< l size) (pos? (f (heap l) (heap i)))) l i)
                    largest (if (and (< r size)
                                     (pos? (f (heap r) (heap li))))
                              r li)]
                (if (not= largest i)
                  (recur (vswap heap i largest) largest size), heap)))]
       (let [last-i (dec (count heap))
             nheap (subvec (vswap heap 0 last-i) 0 last-i)]
         (sift-down nheap 0 (count nheap)))))
  ([heap]
     (heap-remove heap compare)))

(defn heap-peek
  "Returns the element with highest priority in this heap."
  ([heap]
     (first heap)))

(defprotocol PriorityQueue
  "Protocol for Priority Queues"
  (make-heap [self] "Ensures the heap property")
  (push [self e] "Add an item to the priority queue")
  (pop-item [self] "Pop the highest priority item")
  (top [self] "Get the top of the priority queue"))

(deftype Heap [cmpr heap]
  PriorityQueue
      (make-heap [_] (Heap. cmpr (heapify heap cmpr)))
      (push [_ e] (Heap. cmpr (heap-add heap e cmpr)))
      (pop-item [_] (Heap. cmpr (heap-remove heap cmpr)))
      (top [_] (heap-peek heap))
  java.lang.Object
      (toString [_] (str heap))
  Seqable
      (seq [self] (when (seq heap) self))
  ISeq
      (first [self] (peek self))
      (more [self] (pop self))
      (next [self] (pop self))
  IPersistentCollection
      (cons [self x] (push self x))
      (count [_] (count heap))
      (empty [_] (empty heap))
      (equiv [_ x] false) ;TBD
  IPersistentStack ;Queue
      (peek [self] (top self))
      (pop [self] (pop-item self)))

(defn heap
  "Makes a Heap out of the arguments passed with comparison
  fucntion f."
  ([f & args]
     (into (Heap. f []) args)))
