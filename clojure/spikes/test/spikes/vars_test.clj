(ns spikes.vars-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "vars 2.4"
       (facts "declaring bindings using def 2.4.1"
              ; create Vars
              ; binds symbol x to value 42 (root binding: not thread-bound)
              (def x 42)
              x => 42
              ; every thread sees the binding
              (.start (Thread. #(is (= x 42))))
              (def y) ; an unbound symbol
              ;y => nil
              )
       )

