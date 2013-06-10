(ns spikes.scalars.rationals-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "rationals 2.1.4"
       (type-of [22/7 7/22 1028798300297636767687409028872/88829897008789478784 -103/4]) => clojure.lang.Ratio
       (fact "rational simplification"
             100/4 => 25))

(facts "try to be rational 4.2"

       (facts "why be rational? 4.2.1"
              (class 1.0e-430000000M) =>  BigDecimal
              1.0e-430000000M => 1.0e-430000000M 
              ; BigDecimal uses 32 bit to represent the decimal part of a floating point number
              ; 1.0e-4300000000M => (throws NumberFormatException)
              (/ 2.0 3.0) => 0.6666666666666666 

              (facts "floating point aritmetic isn't associative or distributive"
                     (let [a 1.0e50 
                           b -1.0e50 
                           c 17.0e00]
                       (+ (+ a b) c) => 17.0
                       (+ a (+ b c)) => 0.0)

                     (let [a (float 0.1)
                           b (float 0.2)
                           c (float 0.3)]
                       (* a (+ b c)) => (+ (* a b) (* a c)))))
       (facts "how to be rational 4.2.2"
              ;(ratio?)
              ;(rational?)
              ;(rationalize)
              (rational? 1.0e50) => falsey
              (ratio? (/ 2 3)) => truthy
              (let [a (rationalize 1.0e50) 
                    b (rationalize -1.0e50) 
                    c (rationalize 17.0e00)]
                (+ (+ a b) c) => 17
                (+ a (+ b c)) => 17)

              (let [a (rationalize 0.1)
                    b (rationalize 0.2)
                    c (rationalize 0.3)]
                (* a (+ b c)) => (+ (* a b) (* a c)))
              (numerator (/ 123 10)) => 123
              (denominator (/ 123 10)) => 10)

       (facts "caveats of rationality 4.2.3"
              ; speed
              )

       )

