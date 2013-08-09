#lang racket

#|
1. Determine or set the upper and lower limits of the player’s number.
2. Guess a number halfway between those two numbers. (guess)
3. If the player says the number is smaller, lower the upper limit. (smaller)
4. If the player says the number is bigger, raise the lower limit. (bigger) 
Binary Search
TODO: try with immutable values :)
|#

(define lower 1)
(define upper 100)

; "Tab" key to indent code while typing
; Cmd+i to indent all the file

; guess :
; produce a number half-way between LOWER and UPPER
(define (guess)
  (quotient (+ lower upper) 2))

;;(define (return-five) 5)
;;(return-five)

; smaller :
; guess a new number, given that it is smaller than the previous guess 
(define (smaller)
  ; Since we know the maximum number must be smaller than the last guess, 
  ; the biggest it can be is one less than that guess
  (set! upper (max lower (sub1 (guess)))) ; set changes the value of a variable 
  ; By taking the max of lower and guess - 1 we ensure that upper is never
  ; smalller that lower
  (guess))

; bigger :
; guess a new number, given that it is bigger than the previous guess
(define (bigger)
  (set! lower (min upper (add1 (guess))))
  (guess))

; start : Number Number ->
; the main function start starts the game 
; it determines the initial lower and upper given two numbers
(define (start n m)
  (set! lower (min n m))
  (set! upper (max n m))
  (guess))

;; guess number is 42
(start 200 13)
(smaller)
(smaller)
(bigger)
(smaller)
(bigger)
(smaller)