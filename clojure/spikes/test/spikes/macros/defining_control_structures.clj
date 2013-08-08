(ns spikes.macros.defining-control-structures "defining control structures 8.2"
    (:use midje.sweet))

(facts "defining control structures without syntax-quote 8.2.1"

       ;; eval the unevaluated argument
       (defmacro m1 [c] c)
       (m1 1) => (eval '1)             ;; 1
       (m1 'c) => (eval ''c)           ;; 'c
       (m1 [1 2 3]) => (eval '[1 2 3]) ;; [1 2 3]

       ;; eval the unevaluated list of a variable number of arguments
       (defmacro m2 [& c] c)
       (m2 + 1 1) => (eval '(+ 1 1))                 ;; (+ 1 1) => 2
       (m2 + 1 2 3 4) => (eval '(+ 1 2 3 4))         ;; (+ 1 2 3 4) => 10 
       (m2 even? 2) => (eval '(even? 2))             ;; (even? 2) => true
       (m2 #(+ 1 %1) 2) => (eval '(#(+ 1 %1) 2))     ;; (#(+ 1 %1) 2) => (+ 1 2) => 3
       (m2 str 'a 'b 'c) => (eval '(str 'a 'b 'c))   ;; "abc"
       (m2 list 'a 'b 'c) => (eval '(list 'a 'b 'c)) ;; '(a b c)

       (defmacro m3 [& c] (list c))
       (m3 (fn [] (fn [] (+ 1 1)))) => (((fn [] (fn [] (+ 1 1))))) ;; 2

       (defmacro m4 [& c] (list (first c) (next c)))
       (m4 + + 1 1) => (+ (+ 1 1))     ;; 2
       (m4 str + 1 1) => (str (+ 1 1)) ;; "2"

       (defmacro m5 [& clauses]
         (if (next clauses)
           (list (first clauses)
                 (cons 'm5 (next clauses)))
           1
           ))
       (m5 + + +) => (+ (+ (m5 +)))
       (m5 +) => 1

       ;; when evaluates its body in an implicit do
       ;; (nnext x) = ((next (next x))

       (defmacro do-until [& clauses]
         (when clauses
           (list `when (first clauses)
                 (if (next clauses)
                   (second clauses)
                   (throw (IllegalArgumentException.
                           "do-until requires an even number of forms")))
                 (cons 'do-until (nnext clauses)))))
       
       (do-until (even? 2) "Even") => (when (even? 2) "Even" (do-until))
       (do-until (even? 2) "Even" (odd? 3) "Odd") => (when (even? 2) "Even" (do-until (odd? 3) "Odd"))
       (do-until (even? 2) "Even" (odd? 3) "Odd") => (when (even? 2) "Even" (when (odd? 3) "Odd" (do-until)))
       (do-until
        (even? 2) "Even"
        (odd? 3) "Odd"
        (zero? 1) "Ynsm") => (when
                                 (even? 2) "Even"
                                 (when (odd? 3) "Odd"
                                       (when (zero? 1) "Ynsm"
                                             (do-until))))
        (do-until
         (even? 2) "Even"
         (odd? 3) "Odd"
         (zero? 1) "Ynsm"
         :lollipop "Tt") => (when
                                (even? 2) "Even"
                                (when (odd? 3) "Odd"
                                      (when (zero? 1) "Ynsm"
                                            (when :lollipop "Tt"
                                                  (do-until)))))

         (do-until true 1 false 2) => (when true 1
                                            (when false 2
                                                  (do-until)))
         
         (require '[clojure.walk :as walk])
         (clojure.walk/macroexpand-all '(do-until true 1 false 2))

         )

(fact "Defining control structures using syntax-quote and unquoting 8.2.2"

      (defmacro unless [condition & body]
        `(if (not ~condition)
           (do ~@body)))
      
      (unless (even? 3) "Now we see it...") => (if (not (even? 3)) (do "Now we see it..."))
      (unless (even? 2) "Now we don't.") => (if (not (even? 2)) (do "Now we don't."))

      (defn from-end [s n]
        (let [delta (dec (- (count s) n))]
          (unless (neg? delta)
                  (nth s delta))))

      (from-end (range 1 101) 10) => 90

      ;; The proper way to define unless is either
      (defmacro unless [& args]
        `(when-not ~@args))

      ;; or even
      ;(def unless when-not)

      ;;—or just use when-not from the start.
      (when-not false "OK") => "OK"
      
      )
