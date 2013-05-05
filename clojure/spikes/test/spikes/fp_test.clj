(ns spikes.fp-test (:use clojure.test))

(deftest a-test
  (testing "functions in all their forms 7.1"
    ; vectors are functions of theri indices
    (is (= :a ([:a :b] 0)))
    (is (= '(:chthon :grendel) 
           (map [:chthon :phthor :beowulf :grendel] #{0 3})))
    (testing "first class functions 7.1.1"
      ; creating functions on demand using composition
      (def fifth (comp first rest rest rest rest))
      (is (= 5 (fifth [1 2 3 4 5]))))
    ))

