;;;; 5 A Few More Complex Functions

;;; 5.1 The Definition of `copy-to-buffer'

;; earlier code
(defun insert-buffer (buffer)
  "Insert after point the contents of BUFFER.
     Puts mark after the inserted text.
     BUFFER may be a buffer or a buffer name."
  (interactive "*bInsert buffer: ")
  (or (bufferp buffer)
      (setq buffer (get-buffer buffer)))
  (let (start end newmark)
    (save-excursion
      (save-excursion
        (set-buffer buffer)
        (setq start (point-min) end (point-max)))
      (insert-buffer-substring buffer start end)
      (setq newmark (point)))
    (push-mark newmark)))
