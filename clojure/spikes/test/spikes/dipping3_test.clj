(ns spikes.dipping3-test
  (:use clojure.test))

(deftest a-test
  (testing "truthiness 3.1"
    ; every value looks like true in the boolean context (if)
    ; except for false and nil
    (is (= :thruthy (if true :thruthy :falsey)))
    (is (= :thruthy (if [] :thruthy :falsey)))
    (is (= :falsey (if nil :thruthy :falsey)))
    (is (= :falsey (if false :thruthy :falsey)))
    (testing "don't create boolean objects 3.1.2"
      (is (= true Boolean/TRUE))
      (is (= java.lang.Boolean (type true)))
      (is (= false Boolean/FALSE))
      (is (= java.lang.Boolean (type false)))
      (def evil-false (Boolean. "false")) ; never do this!
      (is (= false evil-false)) ; acts like false
      (is (not (false? evil-false))) ; but it is not false
      (is (= :thruthy (if evil-false :thruthy :falsey)))
      ; do this!
      (is (= :falsey (if (Boolean/valueOf "false") :truthy :falsey))) 
      (is (= false (Boolean/valueOf "false")))
      (is (= java.lang.Boolean (type (Boolean/valueOf "false")))))
    (testing "nil versus false 3.1.3"
      (is (= "nil not false" (when (nil? nil) "nil not false")))
      (is (= nil (when (false? nil) "impossible")))
      (is (= "false not nil" (when (false? false) "false not nil")))
      (is (= nil (when (nil? false) "impossible")))))
  (testing "nil pune with care 3.2"
    ; an idiom for testing if a collection is empty
    ; seq returns a sequence view of a collection
    (is (= '(1 2 3) [1 2 3]))  
    (is (= '(1 2 3) (seq [1 2 3])))  
    (is (= () []))  
    (is (= nil (seq [])))  
    ; empty? is not idiomatic
    (is (false? (empty? [1 2 3])))
    (is (true? (empty? [])))
    (defn print-seq [s]
      (when-not (empty? s)
        (prn (first s))
        (recur (rest s))))
    (print-seq [1 2])
    (print-seq [])
    ; use seq instead
    (defn print-seq [s]
      (when (seq s)
        (prn (first s))
        (recur (rest s))))
    (print-seq [1 2])
    (print-seq [])
    ; another apprach with doseq
    (defn print-seq-2 [s]
      (doseq [e s] 
        (prn e)))
    (print-seq-2 [1 2])
    (print-seq-2 [])
    ; next is like rest but returns nil on empty collections
    ; (next s) is (seq (rest s))
    (is (= [2] (rest [1 2])))
    (is (= [2] (next [1 2])))
    (is (= [] (rest [1])))
    (is (= nil (next [1]))))
  (testing "desctructuring 3.3"
    (testing "3.3.1"
      (def guys-whole-name ["Guy" "Lewis" "Steele"])
      ; using indexes (not idiomatic)
      (is (= "Steele, Guy Lewis"
             (str (nth guys-whole-name 2) 
                  ", " 
                  (nth guys-whole-name 0) 
                  " "
                  (nth guys-whole-name 1)))))
    (testing "destructuring with a vector 3.3.2"
      (is (= "Steele, Guy Lewis"
             (let [[f-name m-name l-name] guys-whole-name]
               (str l-name ", " f-name " " m-name))))
      ; descrtucturing a sequence
      (let [[a b c & more] (range 10)]
        (println "a b c are:" a b c)
        (println "more is:" more))
      ; reteining the whole collection with :as
      (let [range-vec (vec (range 10)) ; to show that range-vec remains a vector
            [a b c & more :as all] range-vec]
        (println "a b c are:" a b c)
        (println "more is:" more)
        (println "all is:" all)))
    (testing "destructuring with a map 3.3.3"
      (def guys-name-map {:f-name "Guy", :m-name "Lewis", :l-name "Steele"})
      (is (= "Steele, Guy Lewis" 
             (let [ {lname :l-name, fname :f-name, mname :m-name} guys-name-map ] 
               (str lname ", " fname " " mname))))
      (is (= "Steele, Guy Lewis" 
              (let [{:keys [f-name m-name l-name]} guys-name-map] 
                (str l-name ", " f-name " " m-name))))
      (def faa-name-map {"f-name" "Fabio", "m-name" "Augusto", "l-name" "Arnaboldi"})
      (is (= "Arnaboldi, Fabio Augusto" 
              (let [{:strs [f-name m-name l-name]} faa-name-map] 
                (str l-name ", " f-name " " m-name))))
      ; :syms ???
      ; to get the original map
      (def guys-name-map {:f-name "Guy", :m-name "Lewis", :l-name "Steele"})
      (is (= "first name: Guy whole-name: {:f-name \"Guy\", :m-name \"Lewis\", :l-name \"Steele\"}" 
             (let [{f-name :f-name, :as whole-name} guys-name-map] 
                 (str "first name: " f-name " whole-name: " whole-name))))
      (is (= "Mr.GuyLewisSteele" 
             (let [{:keys [title f-name m-name l-name], :or {title "Mr."}} guys-name-map] 
               (str title f-name m-name l-name))))
      ; associative destructuring
     (is (= [1 4] 
            (let [{first-thing 0, last-thing 3} [1 2 3 4]] 
              [first-thing last-thing]))))
    (testing "destructuring in function parameters 3.3.4"
      (defn last-name [{:keys [l-name]}]
        l-name)
      (is (= "Steele" (last-name guys-name-map))))
    )
  )
