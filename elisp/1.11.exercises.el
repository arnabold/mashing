;; * Generate an error message by evaluating an appropriate symbol that
;;   is not within parentheses.
a ;; void variable a

;; * Generate an error message by evaluating an appropriate symbol that
;;   is between parentheses.
(a) ;; void function a

;; * Create a counter that increments by two rather than one.
(setq counter 0)
(setq counter (+ counter 2))

;; * Write an expression that prints a message in the echo area when
;;   evaluated.
(message "I am in the echo area and counter is %d." counter)
