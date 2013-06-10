(ns spikes.scalars.numbers-test
  (:use midje.sweet)
  ;(:use clojure.test)
  (:use spikes.core))

(facts "numbers 2.1.1"
       (fact "Numbers evaluate to themselves"
             42 => 42))

(facts "automatic promotion 4.1.2"
       (class 9) => Long
       (class (+ 9 9000000000000000)) => Long
       (class (+ 9 90000000000000000000)) => clojure.lang.BigInt
       (class (+ 9 9.0)) => java.lang.Double
       (class 9M) => java.math.BigDecimal
       (class 9.0M) => java.math.BigDecimal
       (class 9N) => clojure.lang.BigInt)

(facts "overflow 4.1.3"
       (class (+ Long/MAX_VALUE Long/MAX_VALUE)) => (throws ArithmeticException 
                                                            "integer overflow")
       (class (unchecked-add Long/MAX_VALUE Long/MAX_VALUE)) => Long
       (unchecked-add Long/MAX_VALUE Long/MAX_VALUE) => -2)

(facts "underflow 4.1.4"
       ; occours only for floating point numbers
       1.0e-323 => 1.0e-323
       1.0e-324 => 0.0
       0.0000000000000000000000000000000000000000000001 => 1.0e-46
       (class 0.0000000000000000000000000000000000000000000001) => Double
       (class (float 0.0000000000000000000000000000000000000000000001)) => Float
       (float 0.0000000000000000000000000000000000000000000001) => 0.0)

(facts "rounding errors 4.1.5"
       (let [aprox-interval (/ 209715 2097152) ; patriot approximation
             actual-interval (/ 1 10)
             hours (* 3600 100 10)
             actual-total (double (* hours actual-interval))
             aprox-total (double (* hours aprox-interval))]
         (- actual-total aprox-total)) => 0.34332275390625

       (fact "any computation involving a single double will result in a value that’s a double"
             (+ 0.1M 0.1M 0.1M 0.1 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M) => 0.9999999999999999 )
       )

