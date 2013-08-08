(ns spikes.find-pos
  (:use midje.sweet))

;;; locate the positional index of an element within a sequence

;;; work on any composite type returning indices corresponding to some value
;;; return a numerical index ford sequential collections or associatrd key for maps and sets
;;; otherwise retun nil
(defn pos [e coll] 
  (let
    [cmp (if (map? coll)
           #(= (second %1) %2)  ; map compare
           #(= %1 %2))]         ; default compare
    (loop
      [s coll
       idx 0]
      (when (seq s)
        (if (cmp (first s) e)
          (if (map? coll)
            (first (first s))
            idx)
          (recur (next s) (inc idx)))))))

(fact "testing pos version 1"
      (pos 3 [:a 1 :b 2 :c 3 :d 4]) => 5
      (pos :foo [:a 1 :b 2 :c 3 :d 4]) => nil 
      (pos 3 {:a 1 :b 2 :c 3 :d 4}) => :c
      (pos \3 ":a 1 :b 2 :c 3 :d 4") => 13
      )

; transform each collection in a sequence of pairs index-value
(defn index [coll]
  (cond 
    ; differentiate for equality partition: maps, sets, seqs
    (map? coll) (seq coll)
    (set? coll) (map vector coll coll)
    :else (map vector (iterate inc 0) coll)))

(fact "testing index"
      (index {:a 1 :b 2 :c 3 :d 4}) => '([:a 1] [:c 3] [:b 2] [:d 4])
      (index #{:a 1 :b 2 :c 3 :d 4}) => '([1 1] [2 2] [3 3] [4 4] [:a :a] [:c :c] [:b :b] [:d :d])
      (index [:a 1 :b 2 :c 3 :d 4]) => '([0 :a] [1 1] [2 :b] [3 2] [4 :c] [5 3] [6 :d] [7 4])
      )

(defn pos [e coll]
  (for 
    [[i v] (index coll) :when (= e v)] 
    i))

(fact "testing pos version 2"
      (pos 3 [:a 1 :b 2 :c 3 :d 4]) => '(5)
      (pos :foo [:a 1 :b 2 :c 3 :d 4]) => () 
      (pos 3 {:a 1 :b 2 :c 3 :d 4}) => '(:c)
      (pos 3 #{:a 1 :b 2 :c 3 :d 4}) => '(3)
      (pos \3 ":a 1 :b 2 :c 3 :d 4") => '(13)
      )

(defn pos [pred coll]
  (for 
    [[i v] (index coll) :when (pred v)] 
    i))

(fact "testing pos version 3"
      (pos #{3 4} [:a 1 :b 2 :c 3 :d 4]) => '(5 7)
      (pos :foo [:a 1 :b 2 :c 3 :d 4]) => () 
      (pos #{3 4} {:a 1 :b 2 :c 3 :d 4}) => '(:c :d)
      (pos #{3 4} #{:a 1 :b 2 :c 3 :d 4}) => '(3 4)
      (pos even? [2 3 6 7]) => '(0 2)
      )
