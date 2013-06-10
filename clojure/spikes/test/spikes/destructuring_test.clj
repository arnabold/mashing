(ns spikes.destructuring-test
  (:use midje.sweet)
  (:use clojure.test))

(facts "destructuring 3.3"

       (facts "3.3.1"
              (def guys-whole-name ["Guy" "Lewis" "Steele"])
              ; using indexes (not idiomatic)
              (str (nth guys-whole-name 2) 
                   ", " 
                   (nth guys-whole-name 0) 
                   " "
                   (nth guys-whole-name 1)) => "Steele, Guy Lewis"
              )

       (facts "destructuring with a vector 3.3.2"
              (let [[f-name m-name l-name] guys-whole-name]
                (str l-name ", " f-name " " m-name)) => "Steele, Guy Lewis"

              ; descrtucturing a sequence
              (let [[a b c & more] (range 10)]
                a => 0
                b => 1
                c => 2
                more => '(3 4 5 6 7 8 9))

              ; reteining the whole collection with :as
              (let [range-vec (vec (range 10)) ; to show that range-vec remains a vector
                    [a b c & more :as all] range-vec]
                a => 0
                b => 1
                c => 2
                more => '(3 4 5 6 7 8 9)
                all => [0 1 2 3 4 5 6 7 8 9])
              )

       (facts "destructuring with a map 3.3.3" 

              (def guys-name-map {:f-name "Guy", :m-name "Lewis", :l-name "Steele"})

              (let [ {lname :l-name, fname :f-name, mname :m-name} guys-name-map ] 
                (str lname ", " fname " " mname)) => "Steele, Guy Lewis" 

              (let [{:keys [f-name m-name l-name]} guys-name-map] 
                (str l-name ", " f-name " " m-name)) => "Steele, Guy Lewis" 

              (def faa-name-map {"f-name" "Fabio", "m-name" "Augusto", "l-name" "Arnaboldi"})

              (let [{:strs [f-name m-name l-name]} faa-name-map] 
                (str l-name ", " f-name " " m-name)) => "Arnaboldi, Fabio Augusto" 

              ; :syms ???

              ; to get the original map
              (def guys-name-map {:f-name "Guy", :m-name "Lewis", :l-name "Steele"})

              (let [{f-name :f-name, :as whole-name} guys-name-map] 
                (str "first name: " f-name " whole-name: " whole-name)) => "first name: Guy whole-name: {:f-name \"Guy\", :m-name \"Lewis\", :l-name \"Steele\"}" 

              (let [{:keys [title f-name m-name l-name], :or {title "Mr."}} guys-name-map] 
                (str title f-name m-name l-name)) => "Mr.GuyLewisSteele" 

              ; associative destructuring
              (let [{first-thing 0, last-thing 3} [1 2 3 4]] 
                [first-thing last-thing]) => [1 4] 
              )

       (facts "destructuring in function parameters 3.3.4"
              (defn last-name [{:keys [l-name]}] l-name)
              (last-name guys-name-map) => "Steele" 
              )
       )
