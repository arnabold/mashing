#lang racket

(+ 1 1)
(+ 3 (* 2 4))
(sqrt 9)
(sqrt -9) ; a complex number 0+3i
(+ 1 2 3 4 5 6 7 8 9 0) ; a function with a variable number of arguments
(sqrt (+ (sqr 3) (sqr 4))) ; nested functions
'((1 3 5 7 9)(2 4 6 8 0)) ; nested list of numbers
'(sqrt (+ (sqr 3) (sqr 4))) ; nested list of symbols of functions
