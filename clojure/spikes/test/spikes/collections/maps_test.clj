(ns spikes.collections.maps-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))


(facts "maps 2.2.3"
       (fact "map stores unique keys and one value per key"
             (map? {1 "one" :2 "two" '3 "three"}) => truthy
             (map? {1 "one", :2 "two", '3 "three",}) => truthy)
       (fact "an empty map is not nil"
             (nil? {}) => falsey)
       (fact "map doesn't evaluates each item in order"
             ;TODO
             )
       )
(facts "thinking in maps 5.6"
       (facts "hash maps 5.6.1"
              ; unsorted key/value associative structure
              (hash-map :a 1, :b 2, :c 3 :d 4, :e 5) => {:a 1 :b 2 :c 3 :d 4 :e 5}
              (type (hash-map :a 1, :b 2, :c 3 :d 4, :e 5)) => clojure.lang.PersistentHashMap
              (type {:a 1 :b 2 :c 3 :d 4 :e 5}) => clojure.lang.PersistentArrayMap

              ; eterogeneous keys
              (let
                [m {:a 1, 1 :b, [1 2 3] "4 5 6"}]
                [(get m :a) 
                 (get m [1 2 3])]) => [1 "4 5 6"]

              ; a seq of map entries (vectors of a key and a value)
              (seq {:a 1, :b 2}) => '([:a 1] [:b 2])

              ; into create a new coll from two coll
              (into {} '([:a 1] [:b 2])) => {:a 1, :b 2}

              ; create a map using a vector 
              (apply hash-map [:a 1 :b 2]) => {:a 1, :b 2}
              (apply sorted-map [:a 1 :b 2]) => {:a 1, :b 2}
              (apply array-map [:a 1 :b 2]) => {:a 1, :b 2}

              ; create a map using two vactors: keys and values
              (zipmap [:a :b :c] [1 2 3]) => {:a 1 :b 2 :c 3}

              ; maps have no order guaranteed
              )
       (facts "keeping your keys in order with sorted maps 5.6.2"
              ; default ordering: key comparision
              (sorted-map :thx 1138 :r2d 2) => {:r2d 2 :thx 1138}

              (sorted-map "bac" 2 "abc" 9) => {"abc" 9, "bac" 2}
              (sorted-map-by
               #(compare (subs %1 1) (subs %2 1)) 
                "bac" 2 "abc" 9) => {"bac" 2, "abc" 9}
              ; (sorted-map :a 1, "b" 2) ; ClassCastException

              (assoc {1 :int} 1.0 :float) => {1.0 :float, 1 :int}
              (assoc (sorted-map 1 :int) 1.0 :float) => {1 :float}
              )
       (facts "keeping your insertions in order with array maps 5.6.3"
              ; array maps keep insertion order
              (seq (hash-map :a 1 :b 2 :c 3)) => '([:a 1][:c 3][:b 2])
              (seq (array-map :a 1 :b 2 :c 3)) => '([:a 1][:b 2][:c 3])
              )
       )
