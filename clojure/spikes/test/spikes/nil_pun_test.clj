(ns spikes.nil-pun-test
  (:use midje.sweet)
  (:use clojure.test))

(facts "nil pune with care 3.2"
       ; an idiom for testing if a collection is empty
       ; seq returns a sequence view of a collection
       [1 2 3] =>  '(1 2 3) 
       (seq [1 2 3]) =>  '(1 2 3) 
       [] => () 
       (seq []) => nil 
       ; empty? is not idiomatic
       (empty? [1 2 3]) => falsey
       (empty? []) => truthy
       (defn print-seq [s]
         (when-not (empty? s)
           (prn (first s))
           (recur (rest s))))

       ; use seq instead
       (defn print-seq [s]
         (when (seq s)
           (prn (first s))
           (recur (rest s))))

       ; another apprach using doseq
       (defn print-seq-2 [s]
         (doseq [e s] 
           (prn e)))

       ; next is like rest but returns nil on empty collections
       ; (next s) is (seq (rest s))
       (rest [1 2]) => [2]
       (next [1 2]) => [2]
       (rest [1]) => []
       (next [1]) => nil)


