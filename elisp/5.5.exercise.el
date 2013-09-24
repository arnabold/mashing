;; Write an interactive function with an optional argument that tests
;; whether its argument, a number, is greater than or equal to, or
;; else, less than the value of `fill-column', and tells you which, in
;; a message.  However, if you do not pass an argument to the
;; function, use 56 as a default value.

(defun foo (&optional arg)
  (if arg (if (>= arg fill-column)
              (message "optional argument %d is greater then or equal to fill-column %d."
                       arg fill-column)
            (message "optional argument %d is less than fill-column %d."
                     arg fill-column))
    56))

(foo 71)
(foo 70)
(foo 1)
(foo)
