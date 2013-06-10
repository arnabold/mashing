(ns spikes.scalars.symbols-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "symbols 2.1.5"
       (type-of ['a `a 'b `b]) => clojure.lang.Symbol
       (def a 1)
       (def b 1)
       a => b
       'a =not=> `a =not=> 'b =not=> `b
       (symbol? `a) => truthy
       (symbol "a") => 'a
       (symbol (str *ns*) "a") => `a
       (name `a) => (name 'a) => "a")

(facts "symbolic resolution 4.4"
       ; symbol
       ; quote
       ; '
       
       ; symbols are not unique based solely on name alone
       (identical? 'goat 'goat) => false ; two different java object
       (= 'goat 'goat) => true
       (name 'goat) => "goat"
       (let [x 'goat
             y x]
         (identical? x y) => true)

       ; attaching metadata to symbols
       (facts "metadata 4.4.1"
              ; each symbol can have its own metadata (even if the name is equal)
              (let [x (with-meta 'goat {:orney true})
                    y (with-meta 'goat {:orney false})]
                [(= x y) (identical? x y) (meta x) (meta y) ]) => [true false {:orney true} {:orney false}])
       )


