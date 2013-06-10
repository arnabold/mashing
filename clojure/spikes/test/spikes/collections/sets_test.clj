(ns spikes.collections.sets-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "sets 2.2.4"
       (set? #{1 2 "three" :four 0x5}) => truthy
       (fact "an empty set is not nil"
             (nil? #{}) => falsey)
       (fact "set doesn't evaluates each item in order"
             ; TODO
             )
       )

(facts "persistent sets 5.5"
       (facts "basic properties 5.5.1"
              ; sets are functions of their elements
              (#{:a 1 \q :c} :c) => :c
              (#{:a 1 \q :c} :e) => nil
              (get #{:a 1 \q :c} :c) => :c
              (get #{:a 1 \q :c} :nothing-doing) => nil

              ; given two evluating equals element, set contains only one
              ; #{ () [] } ; => (throws IllegalArgumentException "Duplicate key: []")
              ; #{ () [] } => []
              #{ [1 2] '(1 2)} => #{ [1 2] }
              ; #{ () [] #{} {}} => #{ () #{} {} }

              ; finding items in a sequence using a set and some
              (#{1 2 3} 3) => 3 
              (#{1 2 3} 3) => truthy 
              (if (#{1 2 3} 3) 'ok 'ko) => 'ok
              
              (some #{3} [:a \b 3]) => 3
              (some #{:b} [:a 1 :b 2]) => :b
              (some #{1 :b} [:a 1 :b 2]) => 1
       )
       (facts "keeping your sets in order with soted-set 5.5.2"
              (sorted-set :b :c :a) => #{:a :b :c} ;??? what order?
              (sorted-set [3 4] [1 2]) => #{[1 2] [3 4]}
              ; (sorted-set :b 2 :c :a 3 1) ; ClassCastException
              (def my-set (sorted-set :a :b))
              ;... some time later
              ; (conj my-set "a")
              )
       (facts "contains? 5.5.3"
              ; contains? works with keys and sets are implemented as maps with the same
              ; element as the key and value
              (contains? #{1 2 4 3} 4) => true
              (contains? [1 2 4 3] 4) => false
              )
       (facts "clojure.set 5.5.4"

              (clojure.set/intersection 
                #{:humans :fruit-bats :zombies} 
                #{:chupacabra :zombies :humans}) => #{:humans :zombies}
              (clojure.set/intersection 
                #{:pez :gum :dots :skor}
                #{:pez :skor :pocky}
                #{:pocky :gum :skor}) => #{:skor}

              (clojure.set/union 
                #{:humans :fruit-bats :zombies} 
                #{:chupacabra :zombies :humans}) => #{:humans :fruit-bats :zombies :chupacabra }

              (clojure.set/difference
                #{1 2 3 4} #{3 4 5 6}) => #{1 2} ; relative complement: A - B: remove all the element in A that are also in B
              )
       )
