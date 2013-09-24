;;;; 3 How To Write Function Definitions

(defun multiply-by-seven (number)
  "Multiply NUMBER by seven."
  (interactive "p")
  (message "The result is %d" (* 7 number)))

;; Then, you can use this code by typing `C-u' and a number and then
;; typing `M-x multiply-by-seven' and pressing <RET>.

(multiply-by-seven 10) ;; "The result is 70"

;; In this function, the expression, `(interactive "p")', is a list of
;; two elements.  The `"p"' tells Emacs to pass the prefix argument to
;; the function and use its value for the argument of the function.

;; You may want to have code installed automatically whenever you
;; start a new session of Emacs.
;; * Initialization files.
;; * Use the `load' to evaluate and thereby install each of the
;; functions in the files.
;; * Put it in a file called `site-init.el' that is loaded when Emacs
;; is built.  

;; in Emacs Lisp, scoping is dynamic, not lexical.

(let (thread
      (needles 3))
  (message "thread is %s and needles is %d." thread needles)) ;; "thread is nil and needles
;; is 3".
;; Emacs binds the symbol
;; `thread' to an initial value of `nil', and binds the symbol
;; `needles' to an initial value of 3.

(let ((zebra "stripes" )
      (tiger "fierce" ))
  (message "One kind of animal has %s and another is %s." zebra tiger)) ;; "One kind of animal has stripes and another is fierce."

(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
   "Here are %d variables with %s, %s, and %s value."
   birch pine fir oak)) ;; "Here are 3 variables with nil, nil, and some value."

(if (> 5 4)                             ; if-part
    (message "5 is greater than 4!"))   ; then-part
;; "5 is greater than 4!"

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
     If the CHARACTERISTIC is the symbol `fierce',
     then warn of a tiger."
  (if (equal characteristic 'fierce)
      (message "It's a tiger!")))
(type-of-animal 'fierce) ;; "It's a tiger!"
(type-of-animal 'stripes) ;; nil

(if (> 4 5)                               ; if-part
    (message "4 falsely greater than 5!") ; then-part
  (message "4 is not greater than 5!"))   ; else-part
;; "4 is not greater than 5!"

(defun type-of-animal (characteristic)  ; Second version.
  "Print message in echo area depending on CHARACTERISTIC.
     If the CHARACTERISTIC is the symbol `fierce',
     then warn of a tiger;
     else say it's not fierce."
  (if (equal characteristic 'fierce)
      (message "It's a tiger!")
    (message "It's not fierce!")))
(type-of-animal 'fierce) ;; "It's a tiger!"
(type-of-animal 'stripes) ;; "It's not fierce!"

;; `false' is just our old friend `nil'.  Anything else--anything at
;; all--is `true'.

(if 4
    'true
  'false) ;; true

(if nil
    'true
  'false) ;; false

;; The `save-excursion' saves the location of point and mark,
;; executes the body of the function, and then restores point and mark
;; to their previous positions if their locations were changed.  Its
;; primary purpose is to keep the user from being surprised and
;; disturbed by unexpected movement of point or mark.

;; On terminals where the cursor appears to be on top of a character,
;; point is immediately before the character.

;; The "mark" is another position in the buffer; its value can be set
;; with a command such as `C-<SPC>' (`set-mark-command').  If a mark
;; has been set, you can use the command `C-x C-x'
;; (`exchange-point-and-mark') to cause the cursor to jump to the mark
;; and set the mark to be the previous position of point.  In
;; addition, if you set another mark, the position of the previous
;; mark is saved in the mark ring.  Many mark positions can be saved
;; this way.  You can jump the cursor to a saved mark by typing `C-u
;; C-<SPC>' one or more times.

;; The part of the buffer between point and mark is called "the
;; region".

;; The `save-excursion' special form saves the locations of point and
;; mark and restores those positions after the code within the body of
;; the special form is evaluated by the Lisp interpreter.  Thus, if
;; point were in the beginning of a piece of text and some code moved
;; point to the end of the buffer, the `save-excursion' would put
;; point back to where it was before, after the expressions in the
;; body of the function were evaluated.

;; `eval-last-sexp' The value is printed in the echo area unless the
;; function is invoked with an argument (using C-u); in that case, the
;; output is printed in the current buffer.  This command is normally
;; bound to `C-x C-e'.

;; `defun'
;; `interactive' This  special form may be followed by a string with
;; one or more parts that pass the information to the arguments of the
;; function, in sequence.  These parts may also tell the interpreter
;; to prompt for information.  Parts of the string are separated by
;; newlines, `\n'. Common code characters are: 
;; `b'  The name of an existing buffer.
;; `f' The name of an existing file.
;; `p' The numeric prefix argument. (Note that this `p' is lower
;; case.) 
;; `r' Point and the mark, as two numeric arguments, smallest first.
;; This is the only code letter that specifies two successive
;; arguments rather than one.
;; `let'
(let ((foo (buffer-name))
      (bar (buffer-size)))
  (message
   "This buffer is %s and has %d characters."
   foo bar)) ;; "This buffer is 3.how-to-write-function-definitions.el and has 10146 characters."
;; `save-excursion'
(message "We are %d characters into this buffer."
         (- (point)
            (save-excursion
              (goto-char (point-min)) (point)))) ;; "We are 5583 characters into this buffer."
;; `if'
(if (= 22 emacs-major-version)
    (message "This is version 22 Emacs")
  (message "This is not version 22 Emacs")) ;;"This is not version 22 Emacs"

;; `<'
;; `>'
;; `<='
;; `>='
;; `='
;; `equal'
;; `eq'

(let ((s1 "a-string")
      (s2 "a-string"))
  (if (equal s1 s2)
      (message "s1 and s2 are equal and the value is %s" s1))) ;; "s1 and s2 are equal and the value is a-string"
(let ((s1 "a-string")
      (s2 "a-string"))
  (if (not (eq s1 s2))
      (message "s1 and s2 are not the same object"))) ;; "s1 and s2 are not the same object"
;; `string<'
;; `string-lessp'
;; `string='
;; `string-equal'

;; `message'
;; `setq'
;; `set'

;; `buffer-name'
;; `buffer-file-name'
;; `current-buffer'
;; `other-buffer'
;; `switch-to-buffer'
;; `set-buffer'
;; `buffer-size'
;; `point'
;; `point-min'
;; `point-max'

