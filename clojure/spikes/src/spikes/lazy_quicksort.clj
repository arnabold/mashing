(ns spikes.lazy-quicksort
  (:use clojure.tools.trace))
;;; a lazy, tail-recursive version
;;; the problem can be executed incrementally
(defn nom 
  "Produce a list of n random number from 0 to n-1"
  [n]
  (take n (repeatedly #(rand-int n))))

(defn ^:dynamic sort-parts
  "Lazy, tail-recursive, incremental quicksort. Works against
  and creates partitions based on the pivot, defined as 'work'"
  [work]
  (lazy-seq 
    (loop [[part & parts] work] 
      (if-let [ [pivot & xs] (seq part) ] 
        (let [smaller? #(< % pivot)] 
          (recur (list* 
                   (filter smaller? xs) 
                   pivot 
                   (remove smaller? xs) 
                   parts)))
        (when-let [[x & parts] parts]
          (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))
                 

(time (qsort (nom 10000)))
(time (first (qsort (nom 10000))))
(time (take 10 (qsort (nom 10000))))
(dotrace [sort-parts] (sort-parts '([2 4 1 3])))
(dotrace [sort-parts] (sort-parts '([5 3 1 7 4 2 8 6])))
