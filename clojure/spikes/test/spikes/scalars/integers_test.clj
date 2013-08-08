(ns spikes.scalars.integers-test
  (:use midje.sweet)
  ;(:use clojure.test)
  (:use spikes.core))

(facts "integers 2.1.2"
       (fact "Any number starting with an optional sign or digit followed exclusively 
             by digits is considered and stored as an integer"
             [+9 -107] => [9 (- 0 107)])
       (type-of [42 +9 -107]) => Long
       (type-of [42M]) => java.math.BigDecimal
       (type-of [42N
                 991778647261948849222819828311491035886734385827028118707676848307166514]) =>
                 clojure.lang.BigInt
       (fact "decimal, hexadecimal, octal, radix-32, and binary literals"
             [127 0x7F 0177 32r3V 2r01111111] => [127 127 127 127 127])
       (fact "the radix notation supports up to base 36" 
             36r1 => 1)
       )


