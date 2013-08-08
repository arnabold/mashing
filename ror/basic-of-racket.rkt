#lang racket

#|
The part of a Racket compiler that reads in the code is called the Reader.

|#

(define (square n) ; a comment
  (* n n))

;; a line comment 

#|
A block comment.
Useful at the beginning of the file. 
|#

#; (An S-expression comment: tells Racket to ignore the next parenthesized expression)

;; Data Types
;; Booleans
(zero? 1) ; #f
(zero? (sub1 1)) ; #t
;; Symbols
(symbol? 'foo)
(symbol? 'ice9)
(symbol? 'my-killer-app27)
(symbol? '--<<==>>--)
(symbol? '42) ; quoting a number gives a number
(symbol? 42) ; a number is not a symbol
(symbol? '42app)
(symbol=? 'foo 'fOo) ; symbols are case sensitive
;; Numbers
;; Numbers are integers, floating-point, rationals, complex and ...
1 ; an integer
1.0 ; a floating-point number
3/4 ; a rational
0+1i ; a complex number
(expt 53 53) ; the 53rd power of 53
(sqrt -1) ; the complex number 0+1i
(* (sqrt -1) (sqrt -1)) ; -1
(/ 4 6) ; the rational 2/3
(/ 4.0 6) ; an approxumated (floating-point) number
(exact? 4) ; 4 is an exact number
(exact? 4.0) ; 4.0 is an inexact number (a number very close to 4)
(inexact? 4.0)
(inexact? (/ 4.0 6)) ; an inexact number very close to 2/3
;; Strings
;; strings evaluates to themselves
"tutti frutti"
(string-append "tutti" "frutti")
(string-append "tutti" " " "frutti") ; string-append takes a variable number of arguments
(substring "tuttifrutti" 5 11)
(string-ref "tuttifrutti" 5) ; the character f or #\f
(string=? "frutti" "frutti")
;; Lists
;; CONS cells
;; list are composed of cons cells
(list 1 2 3) ; three cons cells: each cell points to a number and to another cons cell
; the final cons cell points to empty
(cons 1 (cons 2 (cons 3 empty)))
;; list is a linked list of cons cell
;; raw cons cell (dotted pair)
(cons 1 2) ; '(1 . 2)
(define cell (cons 'a 'b))
(car cell) ; 'a, the left piece of data of a cons cell
(cdr cell) ; 'b, the right piece of data of a cons cell
(cons 1 (cons 2 (cons 3 4)))
;; List functions
empty ; an empty list
'() ; an empty list
(list) ; an empty list
;; CONS function
(cons 'chicken empty) ; empty is a special value used to terminate a list
(cons 'chicken '())
(cons 'chicken (list))
(cons 'beef (cons 'chicken '()))
(cons 'beef '(chicken))
(cons 'pork '(beef chiken))
(cons 'pork (cons 'beef (cons 'chicken '())))
;; LIST function
(list 'pork 'beef 'chicken)
(cons 'pork (cons 'beef (cons 'chicken empty)))
'(pork beef chicken)
;; FIRST and REST functions
(first (cons 'pork (cons 'beef (cons 'chicken empty))))
(rest (list 'pork 'beef 'chiken))
(first (rest '(pork beef chicken))) ; 'beef
(define (my-second a-list)
  (first (rest a-list)))
(my-second '(pork beef chicken))
