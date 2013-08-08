(ns spikes.laziness-test
  (:use midje.sweet)
  (:import org.arnabold.clojure.AnImmutableObject))

(facts "on immutability 6.1"
       (fact "defining immutability 6.1.1"
             ;; all of the possible properties of immutable objects are defined at the time 
             ;; of their construction and can't be changed thereafter
             ;;
             ;; immutable classes in java
             (def im1 (AnImmutableObject.  1 "1")) 
             (def im2 (AnImmutableObject.  1 "1")) 
             (identical? im1 im2) => false
             (= im1 im2) => true
             (.getLong im1) => (.getLong im2) 
             (.getString im1) => (.getString im2) 
             ; (set! (. im1 l) 2)     ; no matching filed found : l
             ; (set! (. im1 s) "2")   ; no matching field found : s
             )
       (fact "being set in your ways - immutability 6.1.2"
             ;; invariants
             ;; reasoning
             ;; equality has meaning
             ;; sharing is cheap
             ;; flattening the levels of indirection
             ;; immutability fosters concurrent programming: immutable object are always 
             ;; thread safe.
             )
       )

(facts "designing a persistent toy - structural sharing 6.2"
       ;; structural sharing
       ;; the simplest shared structure is the list
       (def baselist (list :barnabas :adam))
       (def lst1 (cons :willie baselist))
       (def lst2 (cons :phoenix baselist))
       ; two different lists
       lst1 => '(:willie :barnabas :adam)
       lst2 => '(:phoenix :barnabas :adam)
       ; share their next parts
       (next lst1) => (next lst2) => baselist
       ; sharing at the java level objects
       (identical? (next lst1) (next lst2)) => true
       (identical? (next lst1) baselist) => true
       (identical? (next lst1) baselist) => true


       ;;; Clojure’s vectors and maps also provide structural sharing, 
       ;;; while allowing you to change values anywhere in the collection, 
       ;;; not just on one end. 

       ;;; We’ll now build a simple tree to help demonstrate how a tree can allow interior 
       ;;; changes and maintain shared structure at the same time.
       (fact "shared structure tree"
             ;; a node with tree field: value, left node, right node 
             ;; empty tree: nil

             ;; building a tree
             (defn xconj [t v]
               (cond
                 (nil? t) {:val v :L nil :R nil}))
             (xconj nil 5) => {:val 5 :L nil :R nil}

             ;; We keep our tree in order by putting values less than a node’s :val 
             ;; in the left branch, and other values in the right branch. 
             (defn xconj [t v]
               (cond
                 (nil? t) {:val v :L nil :R nil}
                 (< v (:val t)) {:val (:val t)
                                 :L (xconj (:L t) v)
                                 :R (:R t)}))
             (def tree1 (xconj nil 5))
             tree1 => {:L nil :val 5 :R nil}
             (:L tree1) => nil     (tree1 :L) => nil
             (:val tree1) => 5     (tree1 :val) => 5
             (:R tree1) => nil     (tree1 :R) => nil

             (def tree1 (xconj tree1 3))
             tree1 => {:L {:L nil :val 3 :R nil} 
                       :val 5 
                       :R nil}
             (:L (:L tree1)) => nil  ((tree1 :L) :L) => nil
             (:val (:L tree1)) => 3  ((tree1 :L) :val) => 3
             (:R (:L tree1)) => nil  ((tree1 :L) :R) => nil
             (:val tree1) => 5       (tree1 :val) => 5
             (:R tree1) => nil       (tree1 :R) => nil

             (def tree1 (xconj tree1 2))
             tree1 => {:L 
                       {:L 
                        {:L nil :val 2 :R nil} 
                        :val 3 
                        :R nil} 
                       :val 5 
                       :R nil}
             (:L (:L (:L tree1))) => nil   (((tree1 :L) :L) :L) => nil
             (:val (:L (:L tree1))) => 2   (((tree1 :L) :L) :val) => 2
             (:R (:L (:L tree1))) => nil   (((tree1 :L) :L) :R) => nil
             (:val (:L tree1)) => 3        ((tree1 :L) :val) => 3
             (:R (:L tree1)) => nil        ((tree1 :L) :R) => nil
             (:val tree1) => 5             (tree1 :val) => 5
             (:R tree1) => nil             (tree1 :R) => nil

             ;; to traverse the three in sorter order
             (defn xseq [t]
               (when t
                 (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))
             (xseq tree1) => '(2 3 5)

             ;; adding the condition to add items not less than the node value
             (defn xconj [t v]
               (cond
                 (nil? t) {:val v :L nil :R nil}
                 (< v (:val t)) {:val (:val t)
                                 :L (xconj (:L t) v)
                                 :R (:R t)}
                 :else {:val (:val t)
                        :L (:L t)
                        :R (xconj (:R t) v)}))

             (def tree2 (xconj tree1 7))
             tree2 => {:L 
                       {:L 
                        {:L nil :val 2 :R nil} 
                        :val 3 
                        :R nil} 
                       :val 5 
                       :R 
                       {:L nil :val 7 :R nil} }
             (:L (:L (:L tree2))) => nil     (((tree2 :L) :L) :L) => nil
             (:val (:L (:L tree2))) => 2     (((tree2 :L) :L) :val) => 2
             (:R (:L (:L tree2))) => nil     (((tree2 :L) :L) :R) => nil
             (:val (:L tree2)) => 3          ((tree2 :L) :val) => 3
             (:R (:L tree2)) => nil          ((tree2 :L) :R) => nil
             (:val tree2) => 5               (tree2 :val) => 5
             (:L (:R tree2)) => nil          ((tree2 :R) :L) => nil
             (:val (:R tree2)) => 7          ((tree2 :R) :val) => 7
             (:R (:R tree2)) => nil          ((tree2 :R) :R) => nil
             (xseq tree2) => '(2 3 5 7)

             (= tree1 tree2) => false
             (identical? tree1 tree2) => false
             (= (:L tree1) (:L tree2)) => true
             (identical? (:L tree1) (:L tree2)) => true ; structure sharing

             (reduce xconj nil (shuffle (range 10)))
             )
)
(facts "laziness 6.3"
       (fact "familiar laziness with logical-and 6.3.1"
             (defn if-chain [x y z]
               (if x 
                 (if y
                   (if z
                     (do
                       ; (println "Made it!")
                       :all-truthy)))))
             (if-chain () 42 true) => :all-truthy
             (if-chain false true true) => nil ; only x is evaluated 

             (defn and-chain [x y z]
               (and x y z (do 
                            ; (println "Made it!") 
                            :all-truthy)))
             (and-chain () 42 true) => :all-truthy
             (and-chain false true true) => false; only x is evaluated 
             )
       (fact "understanding the lazy-seq recipe 6.3.2"
             ;; given a sequence produce a deepley nested structure
             ;; (steps [1 2 3 4]) => [1 [2 [3 [4 []]]]]
             ;; naive implementation
             (defn steps [x]
               (if-not (empty? x)
                 [(first x) (steps (rest x))]
                 []))
             (steps nil) => []
             (steps []) => []
             (steps [1]) => [1 []]
             (steps [1 2]) => [1 [2 []]]
             (steps [1 2 3 4]) => [1 [2 [3 [4 []]]]]

             ;; another naive implementation
             (defn steps [[x & xs]]
               (if x
                 [x (steps xs)]
                 []))
             (steps nil) => []
             (steps []) => []
             (steps [1]) => [1 []]
             (steps [1 2]) => [1 [2 []]]
             (steps [1 2 3 4]) => [1 [2 [3 [4 []]]]]
             ; (steps (range 200000)) ; StackOverflowError
             )
       (fact "rest versusu next"
             ;; rest is more lazy than next

             ; iterate produce a list like (x, (f x), (f (f x)), ...
             (take 5 (iterate #(do 
                                 ; (print \.) 
                                 (inc %1)) 1)) => '(1 2 3 4 5)
             (def very-lazy (-> 
                              (iterate #(do 
                                          ;(print \.) 
                                          (inc %)) 1) 
                              rest rest rest))
             ; -> threads the expr trought the forms
             ; like :
             ;(rest (rest (rest 
             ;              (iterate #(do (print \.) (inc %)) 1)))) 
             (take 5 (rest (iterate #(do 
                                       ; (print \.) 
                                       (inc %1)) 1))) => '(2 3 4 5 6)
             (take 5 (rest (rest (rest (iterate #(do 
                                                   ; (print \.) 
                                                   (inc %1)) 1))))) 
             => '(4 5 6 7 8)
             (first (rest (rest (rest (iterate #(do 
                                                  ; (print \.) 
                                                  (inc %1)) 1))))) => 4
             (first very-lazy) => 4
             (def less-lazy (-> (iterate #(do 
                                            ; (print \.) 
                                            (inc %)) 1)
                                next next next))
             (first less-lazy) => 4
             )
       (fact "utilizing lazy-seq and rest"
             (defn lz-rec-step [s]
               (lazy-seq
                 (if (seq s)
                   [(first s) (lz-rec-step (rest s))]
                   [])))
             (lz-rec-step [1 2 3 4]) => [1 [2 [3 [4 []]]]]
             (class (lz-rec-step [1 2 3 4])) => clojure.lang.LazySeq
             ; dorun force any side effects
             ; (lz-rec-step (range 200000))StackOverflowError
             (dorun (lz-rec-step (range 200000))) => nil

             (defn simple-range [i limit]
               (lazy-seq
                 (when (< i limit)
                   (cons i (simple-range (inc i) limit)))))
             (simple-range 0 9) => '(0 1 2 3 4 5 6 7 8)
             )
       (fact "losing your head 6.3.3"
             (let [r (range 1e4)] ; try with 1e9
               [(first r) (last r)]) => [0 9999] ; retention of r is not needed when the 
             ; computation of (last r) occurs
             ;(let [r (range 1e9)] 
             ;  [(last r) (first r)]) ; OutOfMemoryError
             ) 
(fact "employing infinite sequences 6.3.4"
      ; (iterate (fn [n] (/ n 2)) 1))  produces an infinite sequence 
      (take 5 (iterate (fn [n] (/ n 2)) 1)) => '(1 1/2 1/4 1/8 1/16)

      ;; a function that calculates a triangle number for a given integer
      (defn triangle [n]
        (/ (* n (+ n 1)) 2))
      (triangle 10) => 55
      ;; a sequnce of the first 10 triangle numbers
      (map triangle (range 1 11)) => '(1 3 6 10 15 21 28 36 45 55)
      ;; define a sequence of all the triangle numbers
      (def tri-nums (map triangle (iterate inc 1)))
      (take 10 tri-nums) => '(1 3 6 10 15 21 28 36 45 55)
      (take 10 (filter even? tri-nums)) => '(6 10 28 36 66 78 120 136 190 210)
      ;; what Gauss found
      (nth tri-nums 99) => 5050
      ;; convergence to 2 (geometric progression)
      (/ 1) => 1
      (/ 2) => 1/2
      (/ 3) => 1/3
      ;...
      (double (reduce + (take 1000 (map / tri-nums)))) => 1.998001998001998
      ;; first 2 greater than 10000
      (take 2 (drop-while #(< % 10000) tri-nums)) => '(10011 10153)
      ;; map, reduce and filter retains the laziness of a sequence
      ) 
(fact "the delay and force macros 6.5.3"
      ;; call by need semantic
      ;; delay macro defer thr evaluation of an expression  
      ;; until explicitly forced using the force macro
      (defn defer-expensive [cheap expensive]
        (if-let [good-enough (force cheap)]
          good-enough
          (force expensive)))
      (defer-expensive 
        (delay :cheap)
        (delay (do (Thread/sleep 5000) :expensive))) => :cheap
      (defer-expensive 
        (delay false)
        (delay (do (Thread/sleep 100) :expensive))) => :expensive
      ;; if-let and when-let
      (if :truthy-thing
        (let [res :truthy-thing]
          res)) => :truthy-thing
      (if-let [res :truthy-thing] res) => :truthy-thing
      ;; infinite triangle sequence using delay and force (lazy linked list)
      (defn inf-triangles [n]
        {:head (triangle n)
         :tail (delay (inf-triangles (inc n)))})
      (defn head [l] (:head l)) 
      (defn tail [l] (force (:tail l)))
      (def tri-nums (inf-triangles 1))
      (:head tri-nums) => 1
      (type (:tail tri-nums)) => clojure.lang.Delay
      (:head (.deref (:tail tri-nums))) => 3
      (head tri-nums) => 1
      (head (tail tri-nums)) => 3
      (head (tail (tail tri-nums))) => 6
      (class (:tail (tail (tail tri-nums)))) => clojure.lang.Delay
      (:head (.deref (:tail (tail (tail tri-nums))))) => 10
      ;; navigatio functions using head and tail
      (defn taker [n l]
        (loop [t n
               src l
               ret []]
          (if (zero? t)
            ret
            (recur (dec t) 
                   (tail src)
                   (conj ret (head src))))))
      (defn nthr [l n]
        (if (zero? n)
          (head l)
          (recur (tail l) (dec n))))
      (taker 10 tri-nums) => [1 3 6 10 15 21 28 36 45 55]
      (nthr tri-nums 99) => 5050

      )
)


