;;;; 4 A Few Buffer-Related Functions

;;; 4.1 Finding More Information

;; describe-function: `C-h f' and then the name of the function (and then
;; <RET>).
;; describe-variable: `C-h v' and then the name of the variable (and
;; then <RET>).

;; find-tag; lookup the current tags table. Type `M-.' and then, at
;; the prompt, type in the name of the function whose source code you
;; want to see.

;; To create a `TAGS' file in a specific directory, switch to that
;; directory in Emacs using `M-x cd' command, or list the directory
;; with `C-x d' (`dired').  Then run the compile command, with `etags
;; *.el' as the command to execute: M-x compile RET etags *.el RET

;; beginning-of-buffer bound to `M-<'
(defun simplified-beginning-of-buffer ()
  "Move point to the beginning of the buffer;
     leave mark at previous position."
  (interactive)
  (push-mark) ;; it sets a mark at the current position of the cursor,
  ;; wherever that may be.  The position of this mark is saved in the
  ;; markring.
  (goto-char (point-min))
  (message "point is %d and mark is %d" (point) (mark)))
;; you can, if you wish, go back to where you were originally by
;; typing `C-x C-x'.

(defun simplified-end-of-buffer ()
  "Move point to the end of the buffer;
     leave mark at previous position."
  (interactive)
  (push-mark) 
  (goto-char (point-max))
  (message "point is %d and mark is %d" (point) (mark)))

;; mark-whole-buffer it marks a whole buffer as a region by putting
;; point at the beginning and a mark at the end of the buffer.  It is
;; generally bound to `C-xh'.
(defun mark-whole-buffer ()
  "Put point at beginning and mark at end of buffer.
     You probably should not use this function in Lisp programs;
     it is usually a mistake for a Lisp function to use any subroutine
     that uses or sets the mark."
  (interactive)
  (push-mark (point))
  (push-mark (point-max) nil t)
  (goto-char (point-min))
  (message "point is %d and mark is %d" (point) (mark)))

;; As a result of this, point is placed at the beginning of the buffer
;; and mark is set at the end of the buffer.  The whole buffer is,
;; therefore, the region.

;; append-to-buffer copy the region from the current buffer to a
;; specified buffer.
;; insert-buffer-substring takes a string of characters from part of a
;; buffer, a "substring", and inserts them into another buffer.

(defun append-to-buffer (buffer start end)
  "Append to specified buffer the text of the region.
     It is inserted into that buffer before its point.

     When calling from a program, give three arguments:
     BUFFER (or buffer name), START and END.
     START and END specify the portion of the current buffer to be copied."

  (interactive
   (list (read-buffer
          "Append to buffer: "
          (other-buffer (current-buffer) t))
         (region-beginning)
         (region-end)))
  
  (let (
        (oldbuf (current-buffer)))
    
    (save-excursion ;; it brings you back to your original buffer.
      
      (let* ( ;; bind in sequence
             (append-to (get-buffer-create buffer)) ;; get buffer or
             ;; create it
             (windows (get-buffer-window-list append-to t t)) ;; get
             ;; the list of windows displaying the current buffer
             point)
        
        (set-buffer append-to) ;; make append-to buffer the current one
        
        (setq point (point)) ;; bind point to the current point
        
        (barf-if-buffer-read-only) ;; Signal a `buffer-read-only'
        ;; error if the current buffer is read-only.
        
        (insert-buffer-substring oldbuf start end) ;; copies a
        ;; substring from oldbuf in to the current buffer
        
        (dolist (window windows) ;; for each window
          (when (= (window-point window) point) ;; if the current
            ;; value of point in window is the saved point, set it
            (set-window-point window (point))))))))

