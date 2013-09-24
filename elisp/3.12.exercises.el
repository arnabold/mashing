;; * Write a non-interactive function that doubles the value of its
;;    argument, a number.  Make that function interactive. 
(defun my-double (number)
  "Doubles the value of its argument: a NUMBER."
  (* number 2))
(my-double 2) ;; 4

(defun my-double-interactive (number)
  "Doubles the value of its argument: a NUMBER."
  (interactive "nEnter a number: ")
  (message "The double of %d is %d." number (* 2 number)))

;; * Write a function that tests whether the current value of
;;    `fill-column' is greater than the argument passed to the
;;    function, and if so, prints an appropriate message.
(defun check-fill-column (arg)
  (if (> fill-column arg)
      (message "The current fill-column value %d is greater than the argument passed %d."
               fill-column arg)))
(check-fill-column 69) ;; "The current fill-column value 70 is greater
;; than the argument passed 69."



