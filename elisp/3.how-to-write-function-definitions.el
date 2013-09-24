;;;; 3 How To Write Function Definitions

;; (defun FUNCTION-NAME (ARGUMENTS...)
;;   "OPTIONAL-DOCUMENTATION..."
;;   (interactive ARGUMENT-PASSING-INFO)     ; optional
;;   BODY...)

;; a non interactive example
(defun multiply-by-seven (number)
  "Multiply NUMBER by seven.
The argument list is followed by the documentation string that
describes the function.  This is what you see when you type `C-h f' and
the name of a function.  Incidentally, when you write a documentation
string like this, you should make the first line a complete sentence
since some commands, such as `apropos', print only the first line of a
multi-line documentation string.  Also, you should not indent the
second line of a documentation string, if you have one, because that
looks odd when you use `C-h f' (`describe-function').  The
documentation string is optional, but it is so useful, it should be
included in almost every function you write."
  (* 7 number))
(multiply-by-seven 7) ;; 49

;; Type `C-h f' and the name of a function to see its description

(defun multiply-by-seven (number)       ; Second version.
  "Multiply NUMBER by seven."
  (+ number number number number number number number))
(multiply-by-seven 3) ;; 21



