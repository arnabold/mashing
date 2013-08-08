(ns spikes.core)

(defn type-of [coll] 
  "the type/class of coll elements. 
  nil if they differ."
  (reduce 
    #(if (= %1 %2) %1 nil) 
    (type (first coll)) 
    (map type coll)))


; given a point yx
; generate 4 point moving by one to the 4 directions
; filter those point with coordinates between -1 and size
(defn neighbors
  ([size yx] 
   (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(map + yx %) deltas))))


(defn parse-args [args] 
  (into {} (map (fn [[k v]] 
                  [(keyword (.replace k "--" "")) v])
                (partition 2 args))))

(defn report-ns [] 
  (str "The current namespace is " *ns*))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn check [coll] 
  (if (= () (filter false? coll)) 
    (println "OK") 
    (println "Error"))) 
