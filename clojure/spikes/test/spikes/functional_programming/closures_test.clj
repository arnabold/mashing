(ns spikes.functional-programming.closures-test
  (:use midje.sweet))

(facts "closures 7.2"
       ;; a clojure is a function that has access to locals from a larger scope (the context)
       (def times-two
         (let [x 2]
           (fn [y] (* y x))))
       (fact "the function closes over the local x that its defined outside."
             (times-two 5) => 10)
       (fact "closing over something mutable"
             (def add-and-get
               (let
                   [ai (java.util.concurrent.atomic.AtomicInteger.)]
                 (fn [y] (.addAndGet ai y))))
             ;; ai is bound at define time, not runtime
             (add-and-get 2) => 2
             (add-and-get 2) => 4
             (add-and-get 7) => 11
             ;; A point of note about this technique is that when
             ;; closing over something mutable, you run the risk of
             ;; making your functions impure and thus more difficult
             ;; to test and reason about, especially if the mutable local is shared.
             )
       (fact "functions returning closures"
             (defn times-n [n]
               (let [x n]
                 (fn [y] (* y x))))
             (fn? (times-n 2)) => true
             ((times-n 2) 5) => 10
             ((times-n 4) 10) => 40
             (def times-four (times-n 4))
             (times-four 10) => 40
             )
       (fact "closing over parameters"
             ;; is possible to close over a parameter
             ;; avoiding the local binding
             (defn times-n [n]
               (fn [y] (* y n)))
             ((times-n 2) 5) => 10
             ((times-n 4) 10) => 40
             (defn divisible [denom]
               (fn [numer] (zero? (rem numer denom))))
             ;; calling returned closure immediately
             ((divisible 3) 6) => true
             ((divisible 3) 7) => false
             )
       (fact "passing closures as function argument"
             (filter even? (range 10)) => '(0 2 4 6 8)
             (filter (divisible 4) (range 10)) => '(0 4 8)
             (defn filter-divisible [denom s]
               (filter (fn [numer] (zero? (rem numer denom))) s))
             (filter-divisible 4 (range 10)) => '(0 4 8)
             (defn filter-divisible [denom s]
               (filter #(zero? (rem % denom)) s))
             (filter-divisible 4 (range 10)) => '(0 4 8)
             (filter-divisible 5 (range 20)) => '(0 5 10 15))
       (fact "sharing closure context"
             ;; bearings are directions
             (def bearings [{:x  0, :y  1}    ; north
                            {:x  1, :y  0}    ; east
                            {:x  0, :y -1}    ; south
                            {:x -1, :y  0}])  ; west
             ;; bearings is accessed from outer scope
             ;; (but is not a clojure)
             (defn forward [x y bearing-num]
               [(+ x (:x (bearings bearing-num)))
                (+ y (:y (bearings bearing-num)))])
             ;; go to north
             (forward 5 5 0) => [5 6]
             ;; go to east
             (forward 5 5 1) => [6 5]
             ;; go to south
             (forward 5 5 2) => [5 4]
             ;; a bot (like a java object?)
             ;; coords is the initial point
             ;; bearing is the direction point
             ;; forward return a new bot with the new point
             (defn bot [x y bearing-num]
               {:coords [x y]
                :bearing ([:north :east :south :west] bearing-num)
                :forward (fn []
                           (bot (+ x (:x (bearings bearing-num)))
                                (+ y (:y (bearings bearing-num)))
                                bearing-num))})
             (:coords (bot 5 5 0)) => [5 5]
             (:bearing (bot 5 5 0)) => :north
             (:coords ((:forward (bot 5 5 0)))) => [5 6]
             ;; :forward is a closure over the arguments of bot

             ;; adding other methods (closures)
             (defn bot [x y bearing-num]
               {:coords [x y]
                :bearing ([:north :east :south :west] bearing-num)
                :forward (fn []
                           (bot (+ x (:x (bearings bearing-num)))
                                (+ y (:y (bearings bearing-num)))
                                bearing-num))
                :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
                :turn-left (fn [] (bot x y (mod (- 1 bearing-num) 4)))})
             (:bearing (bot 5 5 0)) => :north
             (:bearing ((:turn-right (bot 5 5 0)))) => :east
             (:coords ((:forward ((:turn-right (bot 5 5 0)))))) => [6 5]
             (:coords ((:forward ((:forward ((:turn-right (bot 5 5 0)))))))) => [7 5]
             (:bearing ((:turn-right ((:forward ((:forward ((:turn-right (bot 5 5 0)))))))))) =>
             :south

             ;; polymorphism
             ;; a bot that support all the previous features but
             ;; has its wired crossed!!!
             (defn mirror-bot [x y bearing-num]
               {:coords     [x y]
                :bearing    ([:north :east :south :west] bearing-num)
                :forward    (fn [] (mirror-bot (- x (:x (bearings bearing-num)))
                                              (- y (:y (bearings bearing-num)))
                                              bearing-num))
                :turn-right (fn [] (mirror-bot x y (mod (- 1 bearing-num) 4)))
                :turn-left  (fn [] (mirror-bot x y (mod (+ 1 bearing-num) 4)))})
             ;; MY COMMENT: poor's man polymorphism. See reify macro
             (:bearing (mirror-bot 5 5 0)) => :north
             (:bearing ((:turn-right (mirror-bot 5 5 0)))) => :east
             (:coords ((:forward ((:turn-right (mirror-bot 5 5 0)))))) => [4 5]
             (:coords ((:forward ((:forward ((:turn-right (mirror-bot 5 5 0)))))))) => [3 5]
             (:bearing ((:turn-right ((:forward ((:forward ((:turn-right
                                                             (mirror-bot 5 5 0)))))))))) =>
                                                             :north
                                                             )
       (fact "compiletime versus runtime"
             (defn massive-calculation [q w e] 1)
             (defn do-thing-builder [x y z]
               (fn do-thing [a b]
                 (massive-calculation x y z)))
             ;; every function is transformed in a class at compile-time
             ((do-thing-builder 'a 'b 'c) 'd 'e) => 1 
             
             )
       )
