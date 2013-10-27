;;;; 2 Practicing Evaluation

(buffer-name) ;; "2.practicing-evaluation.el"
(buffer-file-name) ;; "/home/arnabold/Development/github/mashing/elisp/2.practicing-evaluation.el"

;; type `C-u C-x C-e' instead of `C-x C-e'.  This causes the value
;; returned to appear after the expression.

(current-buffer) ;; #<buffer 2.practicing-evaluation.el>, the buffer
;; itself
(other-buffer) ;; #<buffer *Messages*>, the most recently selected
;; buffer other than the one you are in currently (and that you cannot
;; see)

(switch-to-buffer (other-buffer)) ;; #<buffer *Messages*> and switch
;; to *Messages* buffer

(set-buffer "*Messages*") ;; #<buffer *Messages*>, switches the
;; attention of the computer program to a different buffer, The buffer
;; on the screen remains unchanged

;; If you really want to go to your most recently selected buffer,
;; even if you can still see it, you need to evaluate the following
;; more complex expression:
(switch-to-buffer (other-buffer (current-buffer) t)) ;; #<buffer
;; *info*>

(buffer-size) ;; 1039, the size (number of characters) of the current
;; buffer

(point) ;; 1108, point is the current position of the cursor in number
;; of characters

(point-min) ;; 1, the minimum permissible value of point in the
;; current buffer
(point-max) ;; 1228, the maximum permissible value of point in the
;; current buffer












