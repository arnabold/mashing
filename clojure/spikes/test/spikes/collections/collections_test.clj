(ns spikes.collections.collections-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

; composite data types = collections
; sequence abstraction

(facts "persistent collection 5.1.1"

       ; immutable in-memory collections with specific properties to preserve
       ; historical version of its state, with update and lookup complexity
       ; guarentees; sharing structural elements frome one version to another

       (let 
         ; a java array (a non persistent collection)
         [ds (into-array [:willie :barnaba :adam])] 
         (str (class ds)) => "class [Lclojure.lang.Keyword;" 
         (seq ds) => '(:willie :barnaba :adam) 
         (aset ds 1 :quentin) => :quentin ; aset is used to modify (in place) a java array 
         (seq ds) => '(:willie :quentin :adam))

       (let 
         ; a clojure vector (a persistent collection)
         [ds [:willie :barnaba :adam]
          ds1 (replace {:barnaba :quentin} ds)] 
         (class ds) => clojure.lang.PersistentVector 
         ds => [:willie :barnaba :adam] 
         ds1 => [:willie :quentin :adam]))

(facts "sequential collection, sequence and seq 5.1.2"

       ; SEQUENTIAL COLLECTION: holds a a series of value without reordering them. 

       (let
         ; clojure vector is sequential but not a sequence 
         [v1 [1 2 3]]
         (class v1) => clojure.lang.PersistentVector 
         (sequential? v1) => true  ; does implement Sequential?
         (seq? v1) => false        ; does implement ISeq?
         (seq v1) => '(1 2 3)
         (seq? (seq v1)) => true)

       (let
         ; clojure list is a sequence
         [l1 '(1 2 3)]
         (class l1) => clojure.lang.PersistentList 
         (sequential? l1) => true
         (seq? l1) => true)

       (let
         ; clojure map is not a sequence nor sequential, but is seq-able
         [m1 {'a 1 'b 2 'c 3}]
         (class m1) => clojure.lang.PersistentArrayMap
         (sequential? m1) => false
         (seq? m1) => false
         (seq m1) => '([a 1] [c 3] [b 2])
         (seq? (seq m1)) => true)

       (let
         ; clojure set is not a sequence nor sequential, but is seq-able
         [s1 #{1 2 3}]
         (class s1) => clojure.lang.PersistentHashSet 
         (sequential? s1) => false
         (seq? s1) => false
         (seq s1) => '(1 2 3) 
         (seq? (seq s1)) => true)

       (let 
         ; java array (logically a sequential collection)
         [ja (into-array [:willie :barnaba :adam])] 
         (sequential? ja) => false
         (seq? ja) => false
         (seq ja) => '(:willie :barnaba :adam) 
         (seq? (seq ja)) => true)

       (let
         ; java.util.List (logically a sequential collection)
         [jl (java.util.Arrays/asList (into-array [1 2 3]))]
         (class jl) =>  java.util.Arrays$ArrayList
         (sequential? jl) => false
         (seq? jl) => false
         (seq jl) => '(1 2 3) 
         (seq? (seq jl)) => true)

       ; rest, map and filter return sequences
       (rest []) => ()
       (map #(%) []) => ()
       (filter #(pos? %) []) => ()

       ; next and butlast returns a seq
       (next []) => nil
       (butlast []) => nil)

(facts "collections equality partitions: sequentials, maps and sets"

       (let 
         [v1 [1 2 3]
          l1 '(1 2 3)
          m1 {'a 1 'b 2 'c 3}
          s1 #{1 2 3}]
         ; different concrete sequential collections can be equals
         v1 => l1
         ; different partition collections cannot be equals
         v1 =not=> s1
         v1 =not=> m1
         m1 =not=> s1))

(facts "sequence abstraction"
       ; SEQUENCE: a sequential collections that represents a series of values 
       ; that may or may not exist yet.
       ;
       ; a 'seq object' is any object that implements the 'seq API' 
       ; (first coll): returns the first element or nil, 
       ; (rest coll): returns a sequence other than the first or the empty sequence.
       ;
       ; the 'seq function' produces a 'seq object' 
       (let
         [hm1 (hash-map :a 1) ; an hash map
          shm1 (seq hm1)]
         (class hm1) => clojure.lang.PersistentHashMap
         hm1 => {:a 1} 
         ; shm1 is a sequence (a view from map hm1)
         ; a seq of nodes on a map
         (class shm1) => clojure.lang.PersistentHashMap$NodeSeq
         shm1 => '([:a 1])
         ; a seq of keys from hm1
         (seq (keys hm1)) => '(:a) 
         (class (seq (keys hm1))) => clojure.lang.APersistentMap$KeySeq
         (first shm1) => [:a 1]
         (rest shm1) => ()))

