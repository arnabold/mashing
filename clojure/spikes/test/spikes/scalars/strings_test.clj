(ns spikes.scalars.integers-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "strings 2.1.7" 
       (fact "Strings evaluate to themselves"
             (eval "The Misfits") => "The Misfits") 
       (type-of ["The Misfits"]) => String
       (string? "This is a string") => truthy
       (string? "This is also a 
                string") => truthy
       (string? "This is also a\n string") => truthy)


