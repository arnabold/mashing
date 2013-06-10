(ns spikes.scalars.characters-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "characters 2.1.8"
       (type-of [\a \A \u0042]) => Character
       \B => \u0042
       (char? \\) => truthy ; back-slash character
       \マ => \u30DE ; katanga character
       )

