(ns spikes.functional-programming.pathfinder
  "Source for the A* implementation in section 7.4"
  (:use midje.sweet))

(def world [[  1   1   1   1   1]
            [999 999 999 999   1]
            [  1   1   1   1   1]
            [  1 999 999 999 999]
            [  1   1   1   1   1]])

(defn neighbors
  ([size yx]
     (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
     (filter (fn [new-yx] (every? #(< -1 % size)
                                 new-yx))
             (map #(vec (map + yx %)) deltas))))

(fact "neighbors"
      (neighbors 5 [0 0]) => '([1 0] [0 1])) 

(defn estimate-cost [step-cost-est sz y x]
  (* step-cost-est 
     (- (+ sz sz) y x 2)))

(fact "estimate-cost"
      (estimate-cost 900 5 0 0) => 7200
      (estimate-cost 900 5 4 4) => 0) 

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost 
     (estimate-cost step-cost-est size y x)))

(total-cost 0 900 5 0 0) => 7200
(total-cost 1000 900 5 3 4) => 1900

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
            coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 9}]) => {:cost 9}

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)                    ;; #: Check done
        [(peek (peek routes)) :steps steps] ;; #: Grab the first route
        (let [[_ yx :as work-item] (first work-todo) ;; #: Get next work item
              rest-work-todo (disj work-todo work-item) ;; #: Clear from todo
              nbr-yxs (neighbors size yx)    ;; #: Get neighbors
              cheapest-nbr (min-by :cost     ;; #: Calc least-cost
                                   (keep #(get-in routes %) 
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx) ;; #: Calc path so-far
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost)) ;; #: Check if new is worse
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps) ;; #: Place new path in the routes
                   (assoc-in routes yx
                             {:cost newcost 
                              :yxs (conj (:yxs cheapest-nbr []) 
                                         yx)})
                   (into rest-work-todo ;; #: Add the estimated path to the todo and recur
                         (map 
                          (fn [w] 
                            (let [[y x] w]
                              [(total-cost newcost step-est size y x) w]))
                          nbr-yxs)))))))))

(astar [0 0] 900 world) => [{:cost 17,
                             :yxs [[0 0] [0 1] [0 2] [0 3] [0 4]
                                   [1 4]
                                   [2 4] [2 3] [2 2] [2 1] [2 0]
                                   [3 0]
                                   [4 0] [4 1] [4 2] [4 3] [4 4]]}
                            :steps 94]

(astar [0 0] 900 [[  1   1   1   2   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1   1   1]]) => [{:cost 9,
                                               :yxs [[0 0] [0 1] [0 2]
                                                     [1 2]
                                                     [2 2]
                                                     [3 2]
                                                     [4 2] [4 3] [4 4]]}
                                              :steps 134]

(astar [0 0] 900 [[  1   1   1   2   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1 999   1]
                  [  1   1   1 666   1]]) => [{:cost 10,
                                               :yxs [[0 0] [0 1] [0 2] [0 3] [0 4]
                                                     [1 4]
                                                     [2 4]
                                                     [3 4]
                                                     [4 4]]}
                                              :steps 132]




