(ns spikes.scalars-test
  (:use clojure.test))
(deftest a-test
  (testing "scalars 2.1"
    (println "Scalars")
    (testing "numbers 2.1.1"
      (is (= 42 (eval 42))); Numbers evaluate to themselves
      (testing "integers 2.1.2"
        (is (= java.lang.Long (type 42)))
        (is (= 9 +9))
        (is (= java.lang.Long (type +9)))
        (is (= (- 0 107) -107))
        (is (= java.lang.Long (type -107)))
        (is (= clojure.lang.BigInt (type 991778647261948849222819828311491035886734385827028118707676848307166514)))
        (is (= [127 127 127 127 127] [127 0x7F 0177 32r3V 2r01111111]))
        (is (= 1 36r1))) ; the radix notation supports up to base 36
      (testing "floating point numbers 2.1.3"
        (is (= java.lang.Double (type 1.17)))
        (is (= java.lang.Double (type +1.22)))
        (is (= java.lang.Double (type -2.)))
        (is (not (= -2 -2.)))
        (is (= -2.0 -2.))
        (is (= java.lang.Double (type 366e7))) ; 366 * 10^7 
        (is (= (* 366 (* 10 10 10 10 10 10 10) 366e7))) 
        (is (= java.lang.Double (type 32e-14)))
        (is (= java.lang.Double (type 10.7e-3))))
      (testing "rationals 2.1.4"
        (is (= clojure.lang.Ratio (type 22/7)))
        (is (= clojure.lang.Ratio (type 7/22)))
        (is (= clojure.lang.Ratio (type 1028798300297636767687409028872/88829897008789478784)))
        (is (= clojure.lang.Ratio (type -103/4)))
        (is (= 25 100/4)))); simplification of rational
    (testing "symbols 2.1.5"
      (println "TODO: symbols")
      (def a 1)
      (def b 1)
      (is (= a b))
      (is (not (= `a `b)))
      (is (= java.lang.Long (type a)))
      (is (= clojure.lang.Symbol (type `a)))
      (is (= clojure.lang.Symbol (type 'a)))
      (is (symbol? `a))
      (is (= 'a (symbol "a")))
      (is (= "a" (name `a))))
    (testing "keywords 2.1.6"
      (is (= :pyotr (eval :pyotr))) ; Keywords evaluate to themselves
      (is (= clojure.lang.Keyword (type :chumby)))
      (is (= clojure.lang.Keyword (type :2)))
      (is (= clojure.lang.Keyword (type :?)))
      (is (= clojure.lang.Keyword (type :ThisIsTheNameOfaKeyword)))
      (is (keyword? :chumby))
      (is (= :chumby (find-keyword "chumby")))
      (is (= :chumby (keyword "chumby")))
      (is (= "chumby" (name :chumby))))
    (testing "strings 2.1.7" 
      (is (= "The Misfits" (eval "The Misfits"))); Strings evaluate to themselves
      (is (= java.lang.String (type "The Misfits")))
      (is (string? "This is a string"))
      (is (string? "This is also a 
                   string"))
      (is (string? "This is also a\n string")))
    (testing "characters 2.1.8"
      (is (= java.lang.Character (type \a)))
      (is (= java.lang.Character (type \A)))
      (is (= java.lang.Character (type \u0042))) ; unicode character
      (is (= \B \u0042))
      (is (char? \\)) ; back-slash character
      (is (= \マ \u30DE)) ; katanga character
      )
    )
  (testing "collections 2.2"
    (testing "lists 2.2.1"
      (is (list? '('yankee 'hotel 'foxtrot)))
      (is (list? '(yankee hotel foxtrot)))
      ; when a list is evaluated, the first item of the list will be resolved to a function
      (defn yankee [a b] (seq [a b]))
      (def hotel 'hotel)
      (def foxtrot 'foxtrot)
      (is (fn? yankee))
      (is (= '(hotel foxtrot) (yankee hotel foxtrot)))
      (is (list? '(1 2 3 4)))
      (is (list? ()))
      (is (list? '(1 2 (a b c) 4 5)))
      (is (not (nil? ()))))
    (testing "vectors 2.2.2"
      (is (vector? [1 2 :a :b :c]))
      (is (not (nil? [])))
      (is (vector? [1 2 (+ 1 2) :a :b :c]))
      ; if a list appears within the vector, taht list is evaluated
      (is (= [1 2 3 :a :b :c] [1 2 (+ 1 2) :a :b :c])))
    (testing "maps 2.2.3"
      (is (map? {1 "one" :2 "two" '3 "three"}))
      (is (not (nil? {}))))
    ; unlinke vectors evaluation order is not guaranteed
    (testing "sets 2.2.4"
      (is (set? #{1 2 "three" :four 0x5}))
      (is (not (nil? #{})))
      ; unlinke vectors and like maps evaluation order is not guaranteed
      ))
  (testing "functions 2.3"
    (testing "calling functions 2.3.1"
      (is (= 6 (+ 1 2 3))) ; prefix notation
      )
    (testing "defining functions 2.3.2"
      ; mk-set symbol is optional and doesn't correspond to a globally accessible name
      ; but to an internal name used for self-calls
      (is (fn? 
            (fn mk-set [x y] 
              #{x y})))
      ; calling a function
      (is (= #{1 2} ((fn [x y] #{x y}) 1 2)))
      (is (= #{2 1} ((fn [x y] #{x y}) 1 2)))
      ; arity overloading
      (is (= #{42}
             ((fn 
                ([x] #{x}) 
                ([x y] #{x y})) 42)))
      (is (= #{1 2} 
             ((fn 
                ([x] #{x}) 
                ([x y] #{x y})) 1 2)))
      (is (thrown-with-msg? 
            clojure.lang.ArityException  
            #"Wrong number of args \(3\) passed to: " 
            ((fn arity2 [x y] [x y]) 1 2 3)))
      ; optional arguments
      (is (= [1 2 nil]
             ((fn arity2+ [x y & z] [x y z]) 1 2)))
      (is (= [1 2 '(3 4)]
             ((fn arity2+ [x y & z] [x y z]) 1 2 3 4)))
      (is (thrown-with-msg?
            IllegalArgumentException
            #"Wrong number of args \(1\) passed to: " 
            ((fn arity2+ [x y & z] [x y z]) 1))))
    (testing "def and defn 2.3.3"
      (def make-a-set
        (fn
          ([x] #{x})
          ([x y] #{x y})))
      (is (= #{1} (make-a-set 1)))
      (is (= #{1 2} (make-a-set 1 2)))
      (defn make-a-set
        "Takes either one or two values and makes a set from them"
        ([x] #{x})
        ([x y] #{x y}))
      (is (= #{1} (make-a-set 1)))
      (is (= #{1 2} (make-a-set 1 2))))
    (testing "inplace or anonymous functions"
      (def make-a-list_ #(list %))
      (is (= '(1) (make-a-list_ 1)))
      (def make-a-list1 #(list %1))
      (is (= '(2) (make-a-list1 2)))
      (def make-a-list2 #(list %1 %2))
      (is (= '(3 4) (make-a-list2 3 4)))
      (def make-a-list3 #(list %1 %2 %3))
      (is (= '(5 6 7) (make-a-list3 5 6 7)))
      ; optional arguments
      (def make-a-list3+ #(list %1 %2 %3 %&))
      (is (= '(1 2 3 (4 5)) (make-a-list3+ 1 2 3 4 5 )))))
(testing "vars 2.4"
  (testing "declaring bindings using def 2.4.1"
    (def x 42); binds symbol x to value 42 (root binding: not thread-bound)
    (.start (Thread. #(is (= 42 x)))); every thread sees the binding
    (def y) ; an unbound symbol
    ))
(testing "locals, loop and blocks 2.5"
  (testing "blocks 2.5.1"
    ; do: sequence of expr. All the expr are evaluated. Only the last returned
    (is (= 3 (do 6 (+ 5 4) 3))))
  (testing "locals 2.5.2"
    (is (= 78.53750000000001 
           (let 
             [r 5 ; bindings
              pi 3.1415 
              r-squared (* r r)] 
             (* pi r-squared) ; body is an implicit do (sequence of exprs)
             ))))
  (testing "loops 2.5.3"
    (defn print-down-from [x]
      (when (pos? x) ; no else part is associated with the condictional result
        ; an implicit do in order to perform side effects
        (print x)
        (recur (dec x)))) ; recur evaluates argument in order
    ; rebinds x to the new value and 
    ; returns control to the top of print-down-from 
    (print-down-from 10)
    (defn sum-down-from [sum x]
      (if (pos? x)
        (recur (+ sum x) (dec x))
        sum)) ; else
    (is (= 55 (sum-down-from 0 10)))
    (defn sum-down-from [initial-x]
      (loop ; like let and provides a target for recur to jump to
        [sum 0
         x initial-x] 
        (if (pos? x) 
          (recur (+ sum x) (dec x)) 
          sum))) 
    (is (= 55 (sum-down-from 10)))
    ; recur must appear only in a tail position
    (defn absolute-value [x]
      (if (pos? x) ; if form is in tail position
        x ; x is in tail position
        (- x))) ; x is not in tail position, (- x) is in tail position
    ; (fn [x] (recur x) (println x)) compiletime error
    ))
  (testing "quoting 2.6"
    ; quote
    ; syntax-quote
    (testing "evaluation 2.6.1"
      ; (eval cons) #<core$cons clojure.core$cons@553d26fd>
      (is (= '(1 2 3) (cons 1 [2 3]))))
    (testing "quoting 2.6.2"
      (is (= 'tena (quote tena)))
      (def tena 9)
      (is (= 'tena (quote tena)))
      (is (= '(cons 1 (2 3))) (quote (cons 1 [2 3])))
      (is (thrown-with-msg? 
            ClassCastException 
            #"java.lang.Long cannot be cast to clojure.lang.IFn" 
            (cons 1 (2 3)))) ; a number cannot be used as a function
      (is (= '(cons 1 (2 3))) (quote (cons 1 '(2 3))))
      (is (= '(cons 1 (2 3))) (quote (cons 1 (quote(2 3)))))
      (is (= [1 5] [1 (+ 2 3)]))
      (is (= '(1 '(+ 2 3) '(1 (+ 2 3)))))
      (is (= () ()))
      (is (= () '()))
      ; syntax-quote expands to ...
      (is (= '(1 2 3) `(1 2 3)))
      (is (= 'clojure.core/map `map))
      (is (= 'java.lang.Integer `Integer))
      (is (= '(clojure.core/map clojure.core/even? [1 2 3]) `(map even? [1 2 3])))
      (is (= 'spikes.scalars-test/is-always-right `is-always-right)))
    (testing "unquote 2.6.3"
      (is (= '(clojure.core/+ 10 (clojure.core/* 3 2)) `(+ 10 (* 3 2))))
      (is (= '(clojure.core/+ 10 6) `(+ 10 ~(* 3 2))))
      (is (= '(1 2 3) `(1 2 ~3)))
      (is (= '(1 2 3) (let [x 2] `(1 ~x 3))))
      (is (= `(1 2 3) (let [x 2] `(1 ~x 3))))
      (is (thrown-with-msg? 
            ClassCastException 
            #"java.lang.Long cannot be cast to clojure.lang.IFn" 
            `(1 ~(2 3)))) 
      (is (= '(1 (2 3)) (let [x '(2 3)] `(1 ~x))))
      ; unquote-splicing
      ; ; unpack the sequence x
      (is (= '(1 2 3) (let [x '(2 3)] `(1 ~@x)))))
    (testing "auto-gensym 2.6.5"
      ; to generate a new unqualified symbol
      (println `potion#)
      )
    )
)
