(ns spikes.quoting-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "quoting 2.6"
       ; quote and syntax-quote
       (facts "evaluation 2.6.1"
              (type cons) => clojure.core$cons
              (cons 1 [2 3]) => '(1 2 3)
              )
       (facts "quoting 2.6.2"
              (quote tena) => 'tena 
              (def tena 9)
              (quote tena) => 'tena 
              (quote (cons 1 [2 3])) => '(cons 1 (2 3)) 
              ; a number cannot be used as a function
              (cons 1 (2 3)) => (throws ClassCastException #"java.lang.Long cannot be cast to clojure.lang.IFn")
              (quote (cons 1 '(2 3))) => '(cons 1 '(2 3)) 
              (quote (cons 1 (quote(2 3)))) => '(cons 1 '(2 3))
              [1 (+ 2 3)] => [1 5] 
              '(1 (+ 2 3)) => '(1 (+ 2 3))
              () => ()
              '() => ()
              ; syntax-quote expands to ...
              '(1 2 3) => `(1 2 3)
              ; ... symbol auto-qualification
              `map => 'clojure.core/map 
              `Integer => 'java.lang.Integer 
              `(map even? [1 2 3]) => '(clojure.core/map clojure.core/even? [1 2 3])
              `is-always-right => (symbol (str *ns*) "is-always-right") 
              )
       (facts "unquote 2.6.3"
              ; quoting everything
              `(+ 10 (* 3 2)) => '(clojure.core/+ 10 (clojure.core/* 3 2)) 
              ; unquoting
              `(+ 10 ~(* 3 2)) => '(clojure.core/+ 10 6) 
              `(1 2 ~3) => '(1 2 3) 
              (let [x 2] `(1 ~x 3)) => '(1 2 3) 
              (let [x 2] `(1 ~x 3)) => `(1 2 3) 
              `(1 ~(2 3)) => (throws ClassCastException #"java.lang.Long cannot be cast to clojure.lang.IFn")
              (let [x '(2 3)] `(1 ~x)) => '(1 (2 3)) 
              )
       (facts "unquote-splicing 2.6.4"
              ; unpack the sequence x
              (let [x '(2 3)] `(1 ~@x)) => '(1 2 3) 
              )
       (facts "auto-gensym 2.6.5"
              ; to generate a new unqualified symbol
              ;`potion# => #"potion"
              )
       )
