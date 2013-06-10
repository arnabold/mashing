(ns spikes.scalars.floating-point-numbers-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "floating point numbers 2.1.3"
       -2. =not=> -2
       -2. => -2.0
       366e7 => 3.66e9 ;  "366 * 10^7" 
       (type-of [1.17 +1.22 -2. 366e7 32e-14 10.7e-3]) => Double)

(facts "truncation 4.1.1"
       ; BigDecimal has infinite precision
       (let [imadeuapi 3.14159265358979323846264338327950288419716939937M]
         (class imadeuapi) => java.math.BigDecimal
         imadeuapi =>  3.14159265358979323846264338327950288419716939937M)
       ; a floating pint number is truncated by default to fit a Double
       (let [butieatedit 3.14159265358979323846264338327950288419716939937]
         (class butieatedit) => java.lang.Double
         butieatedit => 3.141592653589793))

