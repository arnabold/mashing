(ns spikes.blocks-locals-loops-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "locals, loop and blocks 2.5"
       (facts "blocks 2.5.1"
              ; do: sequence of expr. 
              ; All the expr are evaluated. Only the last returned
              (do 6 (+ 5 4) 3) => 3
              )
       (facts "locals 2.5.2"
              (let 
                [r 5 
                 pi 3.1415 
                 r-squared (* r r)] 
                (* pi r-squared) ; body is an implicit do (sequence of exprs)
                ) => 78.53750000000001 
              )
       (facts "loops 2.5.3"
              (fact "recur"
                    (defn print-down-from [x]
                      (when (pos? x) ; no else part is associated with the condictional result
                        ; an implicit do in order to perform side effects
                        (print x)
                        (recur (dec x)))) ; recur evaluates argument in order
                    ; rebinds x to the new value and 
                    ; returns control to the top of print-down-from 

                    (defn sum-down-from [sum x]
                      (if (pos? x)
                        (recur (+ sum x) (dec x))
                        sum)) ; else
                    (sum-down-from 0 10) => 55

                    (defn sum-down-from [initial-x]
                      (loop ; like let and provides a target for recur to jump to
                        [sum 0
                         x initial-x] 
                        (if (pos? x) 
                          (recur (+ sum x) (dec x)) 
                          sum))) 
                    (sum-down-from 10) => 55

                    ; recur must appear only in a tail position
                    (defn absolute-value [x]
                      (if (pos? x) ; if form is in tail position
                        x ; x is in tail position
                        (- x))) ; x is not in tail position, (- x) is in tail position
                    ; (fn [x] (recur x) (println x)) compiletime error
                    )
              )
       )

