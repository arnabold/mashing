(car '(rose violet daisy buttercap)) ;; rose
(cdr '(rose violet daisy buttercap)) ;; (violet daisy buttercap)
(car '((lion tiger cheetah)
       (gazelle antelope zebra)
       (whale dolphin seal))) ;; (lion tiger cheetah)
(cdr '((lion tiger cheetah)
       (gazelle antelope zebra)
       (whale dolphin seal))) ;; ((gazelle antelope zebra)
;; (whale dolphin seal))
(cons 'pine '(fir oak maple)) ;; (pine fir oak maple)
(cons 'buttercup ()) ;; (buttercup)
(cons 'daisy '(buttercup)) ;; (daisy buttercup)
(cons 'violet '(daisy buttercup)) ;; (violet daisy buttercup)
(cons 'rose '(violet daisy buttercup)) ;; (rose violet daisy buttercup)
(length '(buttercup)) ;; 1
(length '(daisy buttercup)) ;; 2
(length (cons 'violet '(daisy buttercup))) ;; 3
(length ()) ;; 0
(cdr '(pine fir oak maple)) ;; (fir oak maple)
(cdr '(fir oak maple)) ;; (oak maple)
(cdr '(oak maple)) ;; (maple)
(cdr '(maple)) ;; nil
(cdr 'nil) ;; nil
(cdr ()) ;; nil
(cdr nil) ;; nil
(cdr (cdr '(pine fir oak maple))) ;; (oak maple)
(nthcdr 0 '(pine fir oak maple)) ;; (pine fir oak maple)
(nthcdr 1 '(pine fir oak maple)) ;; (fir oak maple)
(nthcdr 3 '(pine fir oak maple)) ;; (maple)
(nthcdr 4 '(pine fir oak maple)) ;; nil
(nthcdr 5 '(pine fir oak maple)) ;; nil
(nth 0 '("one" "two" "three")) ;; "one"
(nth 1 '("one" "two" "three")) ;; "two"
(car (cdr '("one" "two" "three"))) ;; "two"
(setq animals '(antelope giraffe lion tiger)) ;; (antelope giraffe lion
                                        ;tiger)
animals ;; (antelope giraffe lion tiger)
(setcar animals 'hippopotamus) ;; hippopotamus
animals ;; (hippopotamus giraffe lion tiger)
(setq domesticated-animals '(horse cow sheep goat))
(setcdr domesticated-animals '(cat dog)) ;; (cat dog)
domesticated-animals ;; (horse cat dog)








