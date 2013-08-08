#lang racket

(* 1 1) 
1
(- 8 (* 2 3)) 
2
(sqrt 9) 
3
(+ 1 2 3 4 5 6 7 8 9 0) 
45
(sqrt (+ (sqr 3) (sqr 4))) 
5
(list 1 2 3 4 5 6 7 8 9 0) 
'(1 2 3 4 5 6 7 8 9 0)
(list (list 1 3 5 7 9) (list 2 4 6 8 0)) 
'((1 3 5 7 9) (2 4 6 8 0))
(list (list 'hello 'world)
      (list (list 'it 'is) 2063)
      (list 'and 'we 'love 'Racket)) 
'((hello world) ((it is) 2063) (and we love Racket))
'(sqrt (+ (sqr 3) (sqr 4))) ; a list of symbols
(list 'sqrt 
      (list '+ 
            (list 'sqr 3) 
            (list 'sqr 4))) ; a list of symbols ('3 is evaluated like 3)
"(sqrt (+ (sqr 3) (sqr 4)))" ; a string

