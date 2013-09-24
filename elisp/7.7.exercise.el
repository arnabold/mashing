;; Construct a list of four birds by evaluating several expressions
;; with `cons'.  Find out what happens when you `cons' a list onto
;; itself. Replace the first element of the list of four birds with a
;; fish. Replace the rest of that list with a list of other fish.

(setq storks
      (cons 'wood-stork
            (cons 'african-openbill
               (cons 'yellow-billed-stork
                     (cons 'saddle-billed-stork
                           (cons 'white-stork
                                 (cons 'jabiru
                                       (cons 'marabou-stork ())))))))) 

storks ;; (wood-stork african-openbill yellow-billed-stork
;; saddle-billed-stork white-stork jabiru marabou-stork)

(setq storks2 (cons storks storks)) ;; ((wood-stork african-openbill
;; yellow-billed-stork saddle-billed-stork white-stork jabiru
;; marabou-stork) wood-stork african-openbill yellow-billed-stork
;; saddle-billed-stork white-stork jabiru marabou-stork)

(setq four-birds '(wood-stork
                   african-openbill
                   yellow-billed-stork
                   saddle-billed-stork))
(setcar four-birds 'grouper)
four-birds ;; (grouper african-openbill yellow-billed-stork
;; saddle-billed-stork)

(setcdr four-birds '(catfish shark herring trout salmon))
four-birds ;; (grouper catfish shark herring trout salmon)


