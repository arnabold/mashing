(ns spikes.collections.lists-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "lists 2.2.1"

       (fact "a list of three symbols" 
             (list? '(yankee hotel foxtrot)) => truthy)

       (fact "when a list is evaluated, the first item of the list will be resolved to a function"
             (defn yankee [a b] (seq [a b]))
             (fn? yankee) => truthy
             (yankee 'hotel 'foxtrot) => '(hotel foxtrot))

       (fact "list can contains items of any type"
             (list? '(1 2 3 4)) => truthy
             (list? ()) => truthy
             (list? '(fred :ethel)) => truthy
             (list? '(1 2 (a b c) 4 5)) => truthy)

       (fact "an empty list is not nil"
             (nil? ()) => falsey)
       )

(facts "lists: clojure's code from data structure 5.3"
       (class ()) => clojure.lang.PersistentList$EmptyList
       (class '(1)) => clojure.lang.PersistentList
       (count '(1 2 3)) => 3

       ;; lists are used to represent code forms

       (facts "lists like lisps like 5.3.1"
              (first '(1 2 3)) => 1     ; like lisp car
              (next '(1 2 3)) => '(2 3) ; like lisp cdr
              (cons 1 '(2 3)) => '(1 2 3) 
              (conj '(2 3) 1) => '(1 2 3) ; idiomatic in clojure
              (conj [2 3] 1) => [2 3 1]
              )

       (facts "lists as stacks 5.3.2"
              (peek '(1 2 3)) => 1
              (peek ()) => nil
              (pop '(1 2 3)) => '(2 3)
              (pop '(1)) => ()
              (pop ()) => (throws IllegalStateException "Can't pop empty list"))

       (facts "what lists aren't 5.3.3"
              (nth '(1 2 3 4 5 6) 4) => 5 ; walks from the beginning to find it
              ([1 2 3 4 5 6] 4) => 5      ; efficient
              ; (contains? '(1 2 3 4 5 6) 3) => false ; always!
              ; lists are not queues
              )

       )
