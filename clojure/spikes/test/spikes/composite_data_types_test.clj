(ns spikes.composite-data-types-test
  (:use spikes.composite-data-types clojure.test))

(deftest a-test

  (testing "persistent collection"

    ; ja is a java array (a not persistent collection)
    (is (= "class [Lclojure.lang.Keyword;" (str (class ja))))
    (is (= '(:willie :barnaba :adam) (seq ja)))
    ; aset is used to modify (in place) a java array 
    (is (= :quentin (aset ja 1 :quentin)))
    (is (= '(:willie :quentin :adam) (seq ja)))

    ; cv is a clojure vector (a persistent collection)
    (is (= clojure.lang.PersistentVector (class cv)))
    (is (= [:willie :barnaba :adam] cv))
    (is (= [:willie :quentin :adam] cv1)))

  (testing "sequential collection, sequence and seq"

    ; clojure vector is sequential but not a sequence 
    (is (= clojure.lang.PersistentVector (class v1)))
    (is (true? (sequential? v1)))
    (is (false? (seq? v1)))
    (is (= '(1 2 3) (seq v1)))
    (is (true? (seq? (seq v1))))

    ; clojure list is a sequence
    (is (= clojure.lang.PersistentList (class l1)))
    (is (true? (sequential? l1)))
    (is (true? (seq? l1)))

    ; clojure map is not a sequence nor sequential
    (is (= clojure.lang.PersistentHashMap (class m1)))
    (is (false? (sequential? m1)))
    (is (false? (seq? m1)))
    (is (= '([a 1] [c 3] [b 2]) (seq m1)))
    (is (true? (seq? (seq m1))))

    ; clojure set is not a sequence nor sequential
    (is (= clojure.lang.PersistentHashSet (class s1)))
    (is (false? (sequential? s1)))
    (is (false? (seq? s1)))
    (is (= '(1 2 3) (seq s1)))
    (is (true? (seq? (seq s1))))

    ; java array (logically a sequential collection)
    (is (= "class [Lclojure.lang.Keyword;" (str (class ja))))
    (is (false? (sequential? ja)))
    (is (false? (seq? ja)))
    (is (= '(:willie :quentin :adam) (seq ja)))
    (is (true? (seq? (seq ja))))

    ; java.util.List (logically a sequential collection)
    (is (= java.util.Arrays$ArrayList (class jl)))
    (is (false? (sequential? jl)))
    (is (false? (seq? jl)))
    (is (= '(1 2 3) (seq jl)))
    (is (true? (seq? (seq jl))))

    ; rest, map and filter return sequences
    (is (= () (rest [])))
    (is (= () (map #(%) [])))
    (is (= () (filter #(pos? %) [])))

    ; next and butlast returns a seq
    (is (nil? (next [])))
    (is (nil? (butlast [])))
    )

  (testing "collections equality partitions"

    ; different concrete sequential collections can be equals
    (is (= v1 l1))

    ; different partition collections cannot be equals
    (is (not (= v1 s1)))
    (is (not (= v1 m1)))
    (is (not (= m1 s1))))

  (testing "sequence abstraction"
    ; hm1 is an hash map
    (is (= clojure.lang.PersistentHashMap (class hm1)))
    (is (= {:a 1} hm1))
    ; shm1 is a sequence (a view from map hm1)
    ; a seq of nodes on a map
    (is (= clojure.lang.PersistentHashMap$NodeSeq (class shm1)))
    (is (= '([:a 1]) shm1))
    ; a seq of keys from hm1
    (is (= '(:a) (seq (keys hm1))))
    (is (= clojure.lang.APersistentMap$KeySeq (class (seq (keys hm1))))))

  (testing "vectors"
    (testing "building vectors"
      ; a literal vector
      [1 2 3]
      ; from a collection
      (is (= [0 1 2 3 4 5 6 7 8 9]
             (vec (range 10))))
      ; from another vector, conjoining
      (is (= [:a :b :c 0 1 2 3 4 5 6 7 8 9]
             (let
               [my-vector [:a :b :c]]
               (into my-vector (range 10)))))
      ; from arguments
      (is (= [1 'a :b]
             (vector 1 'a :b)))

      ; vectors of primitive types
      (is (= [3 2 1] (vector-of :int Math/PI 2 1.3)))
      (is (= [\d \e \f] (vector-of :char 100 101 102)))
      (is (= [3 2 1] (vector-of :long Math/PI 2 1.3)))
      ; (is (= [3.1415927 2.0 1.3] (vector-of :float Math/PI 2 1.3)))
      (is (= [3.141592653589793 2.0 1.3] (vector-of :double Math/PI 2 1.3)))
      (is (= [3 2 1] (vector-of :byte Math/PI 2 1.3)))
      (is (= [3 2 1] (vector-of :short Math/PI 2 1.3)))
      (is (= [true true true] (vector-of :boolean Math/PI 2 1.3)))
      ; (into (vector-of :int) [1 2 623876371267813267326786327863])
      (is (= [3.1415927 2.0 1.3] (into (vector-of :float) [Math/PI 2 1.3])))
    )
)
)


