(ns spikes.repl-experience)

;; to run 
;; lein -m spikes.repl-experience/-main
;; java -cp ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar:src clojure.main --main spikes.repl-experience

(defn -main []

  ;; using REPL to experiment 3.4

  ;; experimenting with seqs 3.4.1"

  (range 5) ; '(0 1 2 3 4)

  (for [x (range 2) y (range 2)] [x y]) ; '([0 0][0 1][1 0][1 1])

  ; (xor 1 2)
  ; (find-doc "xor")
  (bit-xor 1 2) ; 3

  (for [x (range 2) y (range 2)] [x y (bit-xor x y)]) ; '([0 0 0][0 1 1][1 0 1][1 1 0])

  (defn xors [max-x max-y] 
    (for [x (range max-x) 
          y (range max-y)] 
      [x y (bit-xor x y)])) 

  (xors 2 2) ; '([0 0 0][0 1 1][1 0 1][1 1 0]) )

  ; "experimenting with graphics 3.4.2"

  (def frame (java.awt.Frame.)) ; create an awt frame

  (for [method (seq (.getMethods java.awt.Frame)) 
        :let [method-name (.getName method)] 
        :when (re-find #"Vis" method-name)] 
    method-name) ; '("setVisible" "isVisible") 

  (.isVisible frame) ; falsey

  (.setVisible frame true)

  (.setSize frame (java.awt.Dimension. 200 200))
  ;(javadoc frame)

  (def gfx (.getGraphics frame))
  (.fillRect gfx 100 100 50 75)
  (.setColor gfx (java.awt.Color. 255 128 0))
  (.fillRect gfx 100 150 75 50)

  ; "putting it all together 3.4.3"
  ;
  (doseq [[x y xor] (xors 200 200)] 
    (.setColor gfx (java.awt.Color. xor xor xor)) 
    (.fillRect gfx x y 1 1))

  ; "when things go wrong 3.4.4"
  ;
  (try 
    (doseq [[x y xor] (xors 500 500)] 
      (.setColor gfx (java.awt.Color. xor xor xor)) 
      (.fillRect gfx x y 1 1)) 
    (catch IllegalArgumentException e "something wrong"))

  ;(.printStackTrace *e)

  (defn xors [max-x max-y] 
    (for [x (range max-x) y 
          (range max-y)] 
      [x y (rem (bit-xor x y) 256)]))

  (defn f-values [f xs ys]
    "produces a vector of values for points and color"
    (for [x (range xs)
          y (range ys)]
      [x y (rem (f x y) 256)]))

  (defn clear [g x y] 
    "clear the grafic context g"
    (.clearRect g 0 0 x y))

  (defn draw-values [frame f xs ys]
    "clear the frame.
    set the frame size to xs ys.
    draws every pixel based on f values"
    (let [gfx (.getGraphics frame)]
      (clear gfx xs ys)
      (.setSize frame (java.awt.Dimension. xs ys))
      (doseq [[x y v] (f-values f xs ys)]
        (.setColor gfx (java.awt.Color. v v v))
        (.fillRect gfx x y 1 1))))


  (clear gfx 200 200)

  ; "just for fun 3.4.5"

  (draw-values frame bit-and 256 256) 
  (draw-values frame + 256 256) 
  (draw-values frame * 256 256) 

  (System/exit 0)
  )
