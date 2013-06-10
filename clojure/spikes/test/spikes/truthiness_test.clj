(ns spikes.truthiness-test
  (:use midje.sweet)
  (:use clojure.test))

(facts "truthiness 3.1"
       (facts "what's truth 3.1.1"
              ; every value looks like true in the boolean context (if)
              ; except for false and nil
              (if true :truthy :falsey) => :truthy
              (if [] :truthy :falsey) => :truthy
              (if nil :truthy :falsey) => :falsey
              (if false :truthy :falsey) => :falsey)
       (facts "don't create boolean objects 3.1.2"
              (def evil-false (Boolean. "false")) ; never do this!
              evil-false => false; acts like false
              (false? evil-false) => false ; but it is not false
              (if evil-false :truthy :falsey) => :truthy
              ; do this!
              (if (Boolean/valueOf "false") :truthy :falsey) => :falsey
              (Boolean/valueOf "false") => false
              (type (Boolean/valueOf "false")) => Boolean
              (Boolean/TRUE) => true
              (type true) => Boolean
              (Boolean/FALSE) => false
              (type false) => Boolean)
       (facts "nil versus false 3.1.3"
              (when (nil? nil) "nil not false") => "nil not false" 
              (when (false? nil) "impossible") => nil 
              (when (false? false) "false not nil") =>  "false not nil" 
              (when (nil? false) "impossible") => nil)
)

