(ns spikes.macros.macros "macros 8"
    (:use midje.sweet))

(facts "threading macros"

       ;; -> inserts as the second item in the first form
       ;; ->> inserts as the last item in the  first form
       (-> 25 Math/sqrt int list) => '(5)
       (-> 25 Math/sqrt int list) => (list (int (Math/sqrt 25)))
       
       ;; , is treated as a whitespace
       ;; could be used a s a stitch point
       (-> (/ 144 12) (/ ,,, 2 3) str keyword list) => '(:2)
       (/ ,,, 2 3) => 2/3
       (/ (/ 144 12) 2 3) => 2
       (-> (/ 144 12) (/ ,,, 2 3) str keyword list) => (list (keyword (str (/ (/ 144 12) 2 3))))
       
       (-> (/ 144 12) (* ,,, 4 (/ 2 3)) str keyword (list ,,, :33)) => '(:32 :33)
       (-> (/ 144 12) (* ,,, 4 (/ 2 3)) str keyword (list ,,, :33)) => (list (keyword (str (* (/ 144 12) 4 (/ 2 3)))) :33)
       
       (->> a (+ 5 ,,,) (let [a 5] ,,,)) => 10
       (->> a (+ 5 ,,,) (let [a 5] ,,,)) => (let [a 5] (+ 5 a)))
