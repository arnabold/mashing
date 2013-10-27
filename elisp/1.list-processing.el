;;;; 1 List Processing

;;; 1.2 Run a Program

;; place your cursor immediately after the right hand s-expression
;; then type `C-x C-e':
(+ 2 2)
'(this is a quoted list)

;;; 1.3 Generate an Error Message
(this is an unquoted list)

;;; 1.6.1 Evaluating Inner Lists
;; If you put the cursor  right after the next to last parenthesis (so
;; it appears to sit on top of the last parenthesis), you will get a 6
;; printed in the echo area!  
(+ 2 (+ 3 3))

;;; 1.7 Variables
fill-column ;; 70
(fill-column) ;; error, symbol without a function
+ ;; error, symbol without a value

;;; 1.8 Arguments
(concat "abc" "def") ;; "abcdef"
(substring "The quick brown fox jumped." 16 19) ;; "fox"
(+ 2 fill-column) ;; 72
(concat "The "
        (number-to-string (+ 2 fill-column)) ;; "72"
        " red foxes.") ;; "The 72 red foxes."
;;; 1.8.3 Variable Number of Arguments
(+) ;; 0
(*) ;; 1
(+ 3) ;; 3
(* 3) ;; 3
(+ 3 4 5) ;; 12
(* 3 4 5) ;; 60

(+ 2 'hello) ;; error, wrong type argument

(message "This message appears in the echo area!")
(message "The name of this buffer is: %s." (buffer-name))
(message "The value of fill-column is %d." fill-column)
(message "There are %d %s in the office!"
         (- fill-column 14) "pink elephants")
(message "He saw %d %s"
         (- fill-column 32)
         (concat "red "
                 (substring
                  "The quick brown foxes jumped." 16 21)
                 " leaping."))
(message "Actually, you can use %%s to print a number. It is non specific: i.e.: %s"
         fill-column)

;;; 1.9 Setting the Value of a Variable
(set 'flowers '(rose violet daisy buttercup)) ;; (rose violet daisy
;; buttercup), the symbol flowers is bounded to the list
flowers ;; (rose violet daisy buttercup), side effect
'flowers ;; flowers, the symbol
(set not-a-symbol '(a list)) ;; error, try to evaluate first
;; not-a-symbol
;; setq name is like set symbol, setq is set quotes
(setq carnivores '(lion tiger leopard))
;; is like (set 'carnivores '(lion tiger leopard))
carnivores ;; (lion tiger leopard)
;; setting more variables
(setq
 trees '(pine fir oak maple)
 herbivorse '(gazelle antelope zebra)) ;; (gazelle antelope zebra)
trees ;; (pine fir oak maple)
herbivorse ;; (gazelle antelope zebra)

(setq counter 0) ;; 0 and counter is 0
(setq counter (+ counter 1)) ;; 1 and counter is 1
counter ;; 1









