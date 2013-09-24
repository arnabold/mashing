;; the kill ring: list of stored text by cutting
("a piece of text" "previous piece")

(cons "another piece"
      '("a piece of text" "previous piece")) ;; ("another piece" "a
;; piece of text" "previous piece")

(car (nthcdr 1 '("another piece"
                 "a piece of text"
                 "previous piece"))) ;; "a piece of text"

(char-table-p translation-table-for-input) ;; nil

(characterp ?z) ;; t
(char-to-string ?z) ;; "z"
;; search forward from point to string, set the point to the end of
;; the occourrence found, and return point
(defun foo (c)
  (save-excursion
    (goto-char (point-min))
    (search-forward (char-to-string c)
                    nil ;; search bound, unbounded
                    nil ;; to signal an error when the search fails
                    1 ;; repeat count: how many occourrence of the string
                    ;; to look for
                    )))
(foo ?z) ;; 407

