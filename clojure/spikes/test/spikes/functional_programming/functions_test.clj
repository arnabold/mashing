(ns spikes.functional-programming.functions-test
  (:use midje.sweet))

(facts "functions 2.3"
       ;; functions are first-class types
       
       (facts "calling functions 2.3.1"
              (fact "prefix notation" (+ 1 2 3) => 6))
       
       (facts "defining functions 2.3.2"
              
              (fact "anonymous (unnamed) function definition"
                    ;; here the 'mk-set' symbol is optional and
                    ;; doesn't correspond to a globally accessible
                    ;; name but to an internal name (used for
                    ;; self-calls)
                    (fn? (fn mk-set [x y] #{x y})) => true)
              
              (fact "calling anonymous function"
                    ((fn [x y] #{x y}) 1 2) => #{1 2} => #{2 1})

              (fact "function parameters arity (arguments count) overloading"
                    ((fn 
                       ([x] #{x}) 
                       ([x y] #{x y})) 42) => #{42}
                       ((fn 
                          ([x] #{x}) 
                          ([x y] #{x y})) 1 2) => #{1 2})
              
              (facts "variable number of arguments"
                     (fact "wrong number of argument passed"
                           ((fn arity2 [x y] [x y]) 1 2 3) =>
                           (throws clojure.lang.ArityException
                                   #"Wrong number of args \(3\) passed to: "))
                     (fact "optional arguments default to nil"
                           ((fn arity2+ [x y & z] [x y z]) 1 2) => [1 2 nil])
                     (fact "optional arguments are reated as a list"
                           ((fn arity2+ [x y & z] [x y z]) 1 2 3 4) => [1 2 '(3 4)])
                     ((fn arity2+ [x y & z] [x y z]) 1) =>
                     (throws IllegalArgumentException
                             #"Wrong number of args \(1\) passed to: ")))
       
       (facts "defining functions with def and defn 2.3.3"
              (fact "def is a special form to associate a symbol to (here) a function"
                    (def make-a-set
                      (fn
                        ([x] #{x})
                        ([x y] #{x y}))))
              (fact "'make-a-set' is a function that build a set"
                    (fn? make-a-set) => true
                    (make-a-set 1) => #{1}
                    (make-a-set 1 2) => #{1 2})
              (fact "defn creates named functions (bound to a symbol) and
                     allows documentation"
                    (defn make-a-set
                      "Takes either one or two values and makes a set from them"
                      ([x] #{x})
                      ([x y] #{x y}))
                    (make-a-set 1) => #{1}
                    (make-a-set 1 2) => #{1 2})              
              (facts "inplace (anonymous) functions"
                     (def make-a-list_ #(list %))
                     (make-a-list_ 1) => '(1)
                     
                     (def make-a-list1 #(list %1))
                     (make-a-list1 2) => '(2)
                     
                     (def make-a-list2 #(list %1 %2))
                     (make-a-list2 3 4) => '(3 4)
                     
                     (def make-a-list3 #(list %1 %2 %3))
                     (make-a-list3 5 6 7) => '(5 6 7)
                     
                     (def make-a-list3+ #(list %1 %2 %3 %&))
                     ;; optional arguments are treated as a list
                     (make-a-list3+ 1 2 3 4 5 ) => '(1 2 3 (4 5)))))

(facts "functions in all their forms 7.1"
       
       ;; lambda calculus: 
       ;; function composition
       ;; partial evaluiation
       ;; recursion
       ;; lexical closures
       ;; pure functions
       ;; function constraints
       ;; high-order functions
       ;; first-class functions

       (facts "most of composite types are functions of their elements"
              (fact "a vector is function of its indices"
                    ([:a :b] 0) => :a
                    ;; here a vector is passed to map as a function
                    (map
                     [:chthon :phthor :beowulf :grendel] ; a function
                     #{0 3}) => [:chthon :grendel]))
       
       
       (facts "first class functions 7.1.1"

              ;; first-class function:
              ;; It can be created on demand.
              ;; It can be stored in a data structure.
              ;; It can be passed as an argument to a function.
              ;; It can be returned as the value of a function.

              (facts "creating functions"
                     
                     (facts "creating functions using composition"
                            
                            (fact "creating functions on demand using composition"
                                  (def fifth (comp first rest rest rest rest))
                                  (fn? fifth) => true
                                  ((comp first rest rest rest rest) [1 2 3 4 5])  =>
                                  (first (rest (rest (rest (rest [1 2 3 4 5])))))
                                  (fifth [1 2 3 4 5]) => 5)
                            
                            (fact "creating a function on demand using composition:
                            build arbitrary nth function"
                                  (defn fnth [n]
                                    (apply comp
                                           (cons first (take (dec n) (repeat rest)))))
                                  ;; repeat return a (lazy) sequence of its
                                  ;; argument (here the function rest)
                                  (take 4 (repeat rest)) => [rest rest rest rest]
                                  ;; cons returns a new seq with first
                                  ;; element prepended
                                  (cons first (take 4 (repeat rest))) =>
                                  [first rest rest rest rest]
                                  ;; apply applies the function 'comp' to
                                  ;; arguments of the given sequence
                                  ((apply comp [first rest rest rest rest]) [1 2 3 4 5]) =>
                                  ((comp first rest rest rest rest) [1 2 3 4 5])
                                  (first (rest (rest (rest (rest [1 2 3 4 5]))))) => 5
                                  ;; (fnth 5) returns a function
                                  (fn? (fnth 5)) => true
                                  ((fnth 5) [1 2 3 4 5]) => 5
                                  ((fnth 5) '[a b c d e]) => 'e)
                            
                            (fact "creating functions on demand using composition:
                            apply a number of functions serially"
                                  (map
                                   (comp
                                    keyword
                                    #(.toLowerCase %)
                                    name)
                                   '(a B C)) => '(:a :b :c)
                                   ;; a keyword of a lowercase of a name of a symbol
                                   (name 'a) => "a"
                                   (#(.toLowerCase %) (name 'a)) => "a"
                                   (keyword (#(.toLowerCase %) (name 'a))) => :a
                                   (keyword (#(.toLowerCase %) (name 'B))) => :b))
                     (facts "creating functions on demand using partial functions"
                            (fn? (partial + 5)) => true
                            ((partial + 5) 100) => 105 
                            ((partial + 5) 100 200) => 305 ; not 310
                            ;; partial application is not currying
                            ((partial + 5) 100) => (#(apply + 5 %&) 100) => (apply + [5 100])
                            ((partial + 5) 100 200) => (#(apply + 5 %&) 100 200) =>
                            (apply + [5 100 200]))
                     (fact "creating functions reversing truth with complement"
                           (let [truthiness (fn [v] v)]
                             [((complement truthiness) true)
                              ((complement truthiness) false) 
                              ((complement truthiness) nil)]) => [false true true]
                              (even? 2) => true
                              ((complement even?) 2) => false
                              ((comp not even?) 2) => false)
                     )
              
              (facts "using functions as data"
                     ;; functions
                     ;; can be stored in a container expecting a piece of data, 
                     ;; be it a local,
                     ;; a reference,
                     ;; collections,
                     ;; or anything able to store a java.lang.Object. 
                     )
              )
       (facts "high-order functions 7.1.2"
              
              ;; Takes one or more functions as arguments
              ;; Returns a function as a result
              
              (fact "functions as arguments"
                    (def plays [{:band "Burial",     :plays 979,  :loved 9}
                                {:band "Eno",        :plays 2333, :loved 15}
                                {:band "Bill Evans", :plays 979,  :loved 9}
                                {:band "Magma",      :plays 2665, :loved 31}])
                    ;; an anonymous function is passed as argument to
                    ;; sort-by as the comparator function
                    ;; sort-by-loved-ration order the vector of maps
                    ;; based on the loved-ratio
                    (def sort-by-loved-ratio
                      (partial
                       sort-by
                       #(/ (:plays %) (:loved %))))
                    (plays 0) => {:band "Burial" :plays 979 :loved 9}
                    (/ (:plays (plays 0)) (:loved (plays 0))) => 979/9
                    (int (/ (:plays (plays 0)) (:loved (plays 0)))) => 108
                    (int (/ (:plays (plays 1)) (:loved (plays 1))))  => 155
                    (int (/ (:plays (plays 2)) (:loved (plays 2))))  => 108 
                    (int (/ (:plays (plays 3)) (:loved (plays 3))))  => 85
                    (sort-by #(/ (:plays %) (:loved %)) plays) =>
                    '({:band "Magma", :loved 31, :plays 2665}
                      {:band "Burial", :loved 9, :plays 979}
                      {:band "Bill Evans", :loved 9, :plays 979}
                      {:band "Eno", :loved 15, :plays 2333})
                    (sort-by-loved-ratio plays) =>
                    '({:band "Magma", :loved 31, :plays 2665}
                      {:band "Burial", :loved 9, :plays 979}
                      {:band "Bill Evans", :loved 9, :plays 979}
                      {:band "Eno", :loved 15, :plays 2333}))
              
              (fact "functions as return values"
                    (defn columns
                      "columns is a function that returns a function that
                      returns a vector of values given a vector of keys (column-names)
                      and a map (row)"
                      [column-names]
                      (fn [row]
                        (vec (map row column-names))))
                    (vec (map (plays 0) [:plays :loved :band])) => [979 9 "Burial"]
                    (map plays '(0 1 2 3)) => '({:band "Burial", :loved 9, :plays 979} 
                                                {:band "Eno", :loved 15, :plays 2333} 
                                                {:band "Bill Evans", :loved 9, :plays 979} 
                                                {:band "Magma", :loved 31, :plays 2665})
                    (sort-by (columns [:plays :loved :band])
                             plays) => '({:band "Bill Evans", :loved 9, :plays 979} 
                                         {:band "Burial", :loved 9, :plays 979} 
                                         {:band "Eno", :loved 15, :plays 2333} 
                                         {:band "Magma", :loved 31, :plays 2665}))
              
              (fact "prefer high-order functions when processing sequences"
                    ;; built-in high-order functions:
                    ;; map,
                    ;; reduce,
                    ;; filter,
                    ;; for,
                    ;; some,
                    ;; repeatedly,
                    ;; sort-by,
                    ;; keep,
                    ;; take-while,
                    ;; drop-while
                    ))
       
       (fact "pure functions 7.1.3"
             
             ;; returns the same result
             ;; does not cause any observable side-effects
             
             (fact "referential transparency"
                   (defn keys-apply [f ks m]
                     "'keys-apply' takes a function f, a set of keys ks, and a map m.
                      Applies the function to the value of the map of the given keys set.
                      A new map of the results of the function applied to the keyed entries
                      is returned."
                     (let [only (select-keys m ks)]
                       (zipmap (keys only) (map f (vals only)))))
                   
                   ;; 'select-keys return a part of the map given a
                   ;; key set
                   (select-keys (plays 0) #{:band}) => {:band "Burial"}
                   ;; 'vals' return a sequence of the map values 
                   (vals {:band "Burial"}) => '("Burial")
                   ;; 'keys' returns a sequence of the map keys
                   (keys {:band "Burial"}) => '(:band)
                   ;; 'zipmap' returns a map with the given key and
                   ;; value sequences
                   (zipmap '(:band) '("Burial")) => {:band "Burial"}
                   (keys-apply
                    #(.toUpperCase %) ; function to apply
                    #{:band}          ; keyset
                    (plays 0)         ; map
                    ) => {:band "BURIAL"}
                      
                      ;; manipulate a set of keys based on a given
                      ;; function
                      (defn manip-map [f ks m]
                        "Takes a function, a set of keys, and a map.
                         Applies the function to the map on the given keys.
                         A modified version of the original map is returned with the
                         results of the function applied to each keyed entry."
                        (conj m (keys-apply f ks m)))
                      
                      ;; conj a map to another map with some
                      ;; key-value pair in common results in a map
                      ;; with only one common key-value pair
                      (conj {:a 1 :b 2 :c 3} {:a 1 :b 2 :c 3 :d 4}) => {:a 1 :b 2 :c 3 :d 4} 
                      
                      (manip-map
                       #(int (/ % 2))
                       #{:plays :loved}
                       (plays 0)) => {:band "Burial", :plays 489, :loved 4}

                       ;; no referential transparency
                       ;; side-effects: plays can change from one
                       ;; invocation to another of halve! function
                       (defn halve! [ks]
                         (map
                          (partial
                           manip-map
                           #(int (/ % 2))
                           ks)
                          plays))
                       (halve! [:plays]) =>
                       '({:band "Burial", :loved 9, :plays 489}
                         {:band "Eno", :loved 15, :plays 1166}
                         {:band "Bill Evans", :loved 9, :plays 489}
                         {:band "Magma", :loved 31, :plays 1332}))
             (fact "optimization: memoization or algebraic manipulation")
             (fact "testability"))

       (fact "function named arguments 7.1.4"
             (defn slope
               [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
               (float (/ (- (p2 1) (p1 1)) (- (p2 0) (p1 0)))))
             (slope [4 15] [3 21]) => 1.0
             (slope :p1 [4 15] :p2 [3 21]) => -6.0
                                        ; (slope [2 1]) wrong
             (slope :p2 [2 1]) => 0.5
             (slope) => 1.0)

       (fact "constraining functions with pre and post conditions 7.1.5"
             (defn slope [p1 p2]
               {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
                :post [(float? %)]}
               (float (/ (- (p2 1) (p1 1)) (- (p2 0) (p1 0)))))
                                        ; (slope [10 10] [10 10]) Assert failed (not= p1 p2)
                                        ; (slope [10 1] '(1 20)) Assert failed (vector? p2)
             (slope [10 1] [1 20]) ; ?Assert failed (float? %)
             (slope [10.0 1] [1 20]) => (float -2.1111112)

             ;; decoupling assertions from functions
             (defn put-things [m]
               (into m {:meat "beef" :veggie "broccoli"}))
             (put-things {}) => {:meat "beef", :veggie "broccoli"}
             
             (defn vegan-constraints [f m]
               {:pre [(:veggie m)]
                :post [(:veggie %) (nil? (:meat %))]}
               (f m))
             ; (vegan-constraints put-things {:veggie "carrot"})
                                        ; Assert failed: (nil? (:meat %))
             (vegan-constraints :veggie {:veggie {:veggie "broccoli"}}) => {:veggie "broccoli"}
             
             ;; create contextual constraint based on the appropriate expected result
             (defn balanced-diet [f m]
               {:post [(:meat %) (:veggie %)]}
               (f m))
             (balanced-diet put-things {}) => {:meat "beef", :veggie "broccoli"}
             
             (defn finiky [f m]
               {:post [(= (:meat %) (:meat m))]}(f m))
             ; (finiky put-things {:meat "chicken"}) Assert failed: (=
                                        ; (:meat %) (:meat m))
             )
       )





