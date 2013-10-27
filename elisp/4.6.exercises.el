;; * Write your own `simplified-end-of-buffer' function definition;
;;   then test it to see whether it works.

(defun simplified-end-of-buffer ()
  "Move point to the end of the buffer;
     leave mark at previous position."
  (interactive)
  (push-mark) 
  (goto-char (point-max)))

(simplified-end-of-buffer)

;; * Use `if' and `get-buffer' to write a function that prints a
;;   message telling you whether a buffer exists.

(defun buffer-exists-p (name)
  (if (get-buffer name)
      (message "Buffer %s exists." name)))
(buffer-exists-p "*info*")

;; * Using `find-tag', find the source for the `copy-to-buffer'
;;   function.

