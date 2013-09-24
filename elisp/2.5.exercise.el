;; Find a file with which you are working and move towards its middle.
;; Find its buffer name, file name, length, and your position in the
;; file.

;; I am in the middle of the file

(message "Current buffer name is %s" (buffer-name)) ;; "Current buffer name is 2.5.exercise.el"
(message "File name of the current buffer is %s" (buffer-file-name)) ;; "File name of the current buffer is /home/arnabold/Development/github/mashing/elisp/2.5.exercise.el"
(message "Current buffer length is %d"(buffer-size)) ;; "Current buffer length is 690"
(message "Current position in the current buffer is %d" (point)) ;; "Current position in the current buffer is 606"


;; This is the end of the file
