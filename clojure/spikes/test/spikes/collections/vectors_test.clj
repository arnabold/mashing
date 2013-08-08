(ns spikes.collections.vectors-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "vectors 2.2.2"
       (vector? [1 2 :a :b :c]) => truthy
       (fact "A vector evaluates each item in order"
             (vector? [1 2 (+ 1 2) :a :b :c]) => truthy
             [1 2 (+ 1 2) :a :b :c] => [1 2 3 :a :b :c]
             )
       (fact "an empty vector is not nil"
             (nil? []) => falsey
             )
       )

(facts "building vectors 5.2.1"

       ; a literal vector
       (vector? [1 2 3]) => true ; does implement IPersistentVector?

       ; from a collection
       (vec (range 10)) => [0 1 2 3 4 5 6 7 8 9]

       ; from another vector, conjoining
       (let
         [my-vector [:a :b :c]]
         (into my-vector (range 10)) => [:a :b :c 0 1 2 3 4 5 6 7 8 9])

       ; from arguments
       (vector 1 'a :b) => [1 'a :b]

       ; vectors of primitive types
       (vector-of :int Math/PI 2 1.3) => [3 2 1]
       (into (vector-of :int) [1 2 623876371267813267326786327863]) => (throws IllegalArgumentException #"Value out of range for long: 623876371267813267326786327863")
       (vector-of :char 100 101 102) => [\d \e \f]
       (vector-of :long Math/PI 2 1.3) => [3 2 1] 
       (vector-of :float Math/PI 2 1.3) => [(Float. 3.1415927) (Float. 2.0) (Float. 1.3)]
       (into (vector-of :float) [Math/PI 2 1.3]) => [(Float. 3.1415927) (Float. 2.0) (Float. 1.3)] 
       (vector-of :double Math/PI 2 1.3) => [3.141592653589793 2.0 1.3]
       (vector-of :byte Math/PI 2 1.3) => [3 2 1]
       (vector-of :short Math/PI 2 1.3) => [3 2 1]
       (vector-of :boolean Math/PI 2 1.3) => [true true true])

(facts "large vectors 5.2.2"

       ; vector is efficent at
       ; adding or removing items from the right end of the coll
       ; accessing or changing items by numeric index
       ; traversing in reverse order

       (let
         [my-vector (range 100000)]
         (count my-vector) => 100000
         (nth my-vector 99999) => 99999
         ;(my-vector 99999) => 99999 ???
         ;(get my-vector 99999) => 99999 ???
         (nth my-vector 100000) => (throws IndexOutOfBoundsException)) 

       (let
         [a-to-j (vec (map char (range 65 75)))]
         a-to-j => [\A \B \C \D \E \F \G \H \I \J]
         (nth a-to-j 4) => \E
         (get a-to-j 4) => \E ; !treats a vector like a map
         (a-to-j 4) => \E)

       (nth nil 42) => nil
       (get nil 42) => nil
       ; (nil 42) !IllegalArgumentException

       (nth [42] 42) => (throws IndexOutOfBoundsException)
       (get [42] 42) => nil
       ([42] 42) => (throws IndexOutOfBoundsException)

       (nth [42] 42 :whoops) => :whoops
       (get [42] 42 :whoops) => :whoops

       (let
         [a-to-j (vec (map char (range 65 75)))]
         (seq a-to-j) => '(\A \B \C \D \E \F \G \H \I \J)
         (rseq a-to-j) => '(\J \I \H \G \F \E \D \C \B \A)
         ; changing a (returning a new) vector
         (assoc a-to-j 4 "no longer E") => [\A \B \C \D "no longer E" \F \G \H \I \J]
         (assoc a-to-j 42 "xxx") =>  (throws IndexOutOfBoundsException)
         ; growing by one item with assoc 
         (assoc a-to-j (count a-to-j) "one more") => [\A \B \C \D \E \F \G \H \I \J "one more"])

       (fact "replace function use assoc when given a vector"
             (replace {2 :a 4 :b} [1 2 3 2 3 4]) => [1 :a 3 :a 3 :b])

       (fact "assoc-in, update-in and get-in take a series of indeces to pick items from a nested array"
             (let
               [matrix [[1 2 3] ; not efficent way to represent matrices
                        [4 5 6]
                        [7 8 9]]]
               (get-in matrix [1 2]) => 6
               (assoc-in matrix [1 2] 'x) => [[1 2 3] [4 5 'x] [7 8 9]]
               (update-in matrix [1 2] * 100) => [[1 2 3] [4 5 600] [7 8 9]]))

       (fact "neighbors"

             (let
               [matrix [[1 2 3] ; not efficent way to represent matrices
                        [4 5 6]
                        [7 8 9]]]

               (map + [0 0] [-1 0]) => [-1 0]
               (map + [0 0] [1 0]) => [1 0]
               (map + [0 0] [0 -1]) => [0 -1]
               (map + [0 0] [0 1]) => [0 1]
               (map #(map + [0 0] %) [[-1 0][1 0][0 -1][0 1]]) => [[-1 0][1 0][0 -1][0 1]]

               (< -1 0 3) => true
               (#(< -1 % 3) 0) => true
               (every? #(< -1 % 3) [-1 0]) => false
               (every? #(< -1 % 3) [1 0]) => true
               (every? #(< -1 % 3) [0 -1]) => false
               (every? #(< -1 % 3) [0 1]) => true
               (filter (fn [new-yx] (every? #(< -1 % 3) new-yx)) [[-1 0][1 0][0 -1][0 1]]) => [[1 0] [0 1]]
               (neighbors [[-1 0][1 0][0 -1][0 1]] 3 [0 0]) => [[1 0] [0 1]]
               (neighbors 3 [0 0]) => [[1 0] [0 1]]

               ; in matrix neighbors of 1 are 4 and 2
               (map #(get-in matrix %) (neighbors 3 [0 0])) => [4 2]

               )
             )
       )

(facts "vectors as stacks 5.2.3"

       ; push is conj: adds items to the right side of the stack
       ; pop is pop: removes items from the right side of the stack, returns a new vector with the item removed
       ; peek gets an item from the top of the stack
       ; they are constant time operations

       (let
         [my-stack [1 2 3]] 
         (peek my-stack) => 3
         (pop my-stack) => [1 2]
         (conj my-stack 4) => [1 2 3 4]
         (+
          (peek my-stack)
          (peek (pop my-stack))) => 5)

       (last [1 2 3]) => (peek [1 2 3]) ; but last is slower and confusing on a stack
       (assoc [1 2 3] 3 4) => (conj [1 2 3] 4) ; do not use assoc  

       )

(facts "using vectors instead of reverse 5.2.4"

       ; most tail-recursive algorithms leave a list in a reverse order

       (let 
         [strict-map1 (fn [f coll]
                        (loop 
                          [coll coll
                           acc nil]
                          (if (empty? coll)
                            (reverse acc) ; to get the right order
                            (recur (next coll) (cons (f (first coll)) acc)))))]
         (strict-map1 - (range 5)) => '(0 -1 -2 -3 -4))

       ; use a vector instead of a list

       (let 
         [strict-map2 (fn [f vect]
                        (loop 
                          [vect vect
                           acc []] ; not nil otherwise we get a list
                          (if (empty? vect)
                            acc 
                            (recur (next vect) (conj acc (f (first vect)))))))]
         (seq (strict-map2 - (range 5))) => '(0 -1 -2 -3 -4))
       )

(facts "subvectors 5.2.5"

       (let
         [a-to-j (vec (map char (range 65 75)))]
         ; first index is inclusive
         ; last index is exclusive
         (subvec a-to-j 3 6) => [\D \E \F]
         ; subvectors keep a reference of the underlying vector
         ; subvector of subvector keeps a reference to the original vector 
         (subvec (subvec a-to-j 3 6) 0 2) => [\D \E]
         )
       )

(facts "vectors as mapentries 5.2.6"
       ; a mapentry looks like a vector
       (first {:width 10 :height 20 :dept 15}) => [:width 10]
       ; ooops!: order is not guarentee
       (first {:width 10 :height 20 :depth 15}) => [:depth 15]
       ; ... and it is a vector
       (vector? (first {:width 10 :height 20 :depth 15})) => true
       ; ... so you can use all the vector functions
       (doseq 
         [[dimension amount] {:width 10 :height 20 :depth 15}] 
         (str (name dimension) ":" ) amount "inches")
       ; key is like (nth my-mapentry 0)
       ; val is like (nth my-mapentry 1)
       (key (first {:width 10 :height 20 :depth 15})) => :depth
       (nth (first {:width 10 :height 20 :depth 15}) 0) => :depth
       (val (first {:width 10 :height 20 :depth 15})) => 15
       (nth (first {:width 10 :height 20 :depth 15}) 1) => 15 )

(facts "what vectors aren't 5.2.7"
       (facts "vectors aren't sparse"
              (let
                [v [0 1 2 3 4 5 6 7 8 9]]
                (count v) => 10
                ; only adding element at the end
                (conj v 10) => [0 1 2 3 4 5 6 7 8 9 10]
                ; i cannot insert/delete items in vectors
                )
              )
       (facts "vectors aren't queues"
              ; TODO
              )
       (facts "vectors aren't sets"
              ; TODO
              )
       )












