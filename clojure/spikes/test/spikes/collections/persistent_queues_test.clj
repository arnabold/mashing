(ns spikes.collections.persistent-queues-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "how to use persistent queues 5.4"
       (facts "5.4.1"
              (class clojure.lang.PersistentQueue/EMPTY) => clojure.lang.PersistentQueue

              (defmethod print-method
                clojure.lang.PersistentQueue
                [q, w]
                (print-method '<- w)
                (print-method (seq q) w)
                (print-method '-< w))

              ; (print clojure.lang.PersistentQueue/EMPTY)
              clojure.lang.PersistentQueue/EMPTY => ()
              (pop clojure.lang.PersistentQueue/EMPTY) => ()
              (peek clojure.lang.PersistentQueue/EMPTY) => nil

              (conj clojure.lang.PersistentQueue/EMPTY 1) => '(1)
              (pop (conj clojure.lang.PersistentQueue/EMPTY 1)) => ()
              (peek (conj clojure.lang.PersistentQueue/EMPTY 1)) => 1

              (conj clojure.lang.PersistentQueue/EMPTY 1 2 3) => '(1 2 3)
              (class (conj clojure.lang.PersistentQueue/EMPTY 1 2 3)) => clojure.lang.PersistentQueue
              (pop (conj clojure.lang.PersistentQueue/EMPTY 1 2 3)) => '(2 3)
              (peek (conj clojure.lang.PersistentQueue/EMPTY 1 2 3)) => 1 
              )

       (facts "5.4.2 5.4.3 5.4.4"
              (def schedule
                (conj clojure.lang.PersistentQueue/EMPTY 
                      :wake-up :shower :brush-teeth)) 
              (peek schedule) => :wake-up
              (pop schedule) => '(:shower :brush-teeth)
              ))
