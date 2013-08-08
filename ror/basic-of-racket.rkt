#lang racket

#|
The part of a Racket compiler that reads in the code is called the Reader.

|#

(define (square n) ; a comment
  (* n n))

;; a line comment 
#|
a block comment 
|#