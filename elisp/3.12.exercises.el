;; * Write a non-interactive function that doubles the value of its
;;    argument, a number.  Make that function interactive. 
(defun my-double (number)
  "Doubles the value of its argument: a NUMBER."
  (* number 2))
(my-double 2) ;; 4

(defun my-double-interactive (number)
  "Doubles the value of its argument: a NUMBER."
  (interactive "p\nnEnter a number: ")
  (message "The double of %d is %d." n (* 2 number)))

;; * Write a function that tests whether the current value of
;;    `fill-column' is greater than the argument passed to the
;;    function, and if so, prints an appropriate message.

