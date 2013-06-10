(ns spikes.scalars.keywords-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "keywords 2.1.6"
       (fact "Keywords evaluate to themselves" 
             (eval :pyotr) => :pyotr)
       (type-of [:chumby :2 :? :ThisIsTheNameOfaKeyword]) => clojure.lang.Keyword
       (keyword? :chumby) => truthy
       (keyword? :foo) => truthy
       (find-keyword "chumby") => :chumby
       (if (find-keyword "boo") nil (find-keyword "boo")) => nil
       (keyword "chumby") => :chumby
       (if (keyword "bar") (keyword "bar") nil) => :bar
       (name :chumby) => "chumby"
       (if (name :baz) (name :baz) nil) => "baz")

(facts "when to use keywords 4.3"
       ; keywords or symbolic identifiers
       (facts "how are keywords different from symbols 4.3.1"
              ; keywords always refer to themselves and provide fast equality checks

              ; as keys
              (let [population {:zombies 2700 :humans 9}] 
                (:zombies population) => 2700 
                (/ (:zombies population) (:humans population)) => 300)

              ; as enumeration
              (let [constants {:small 1
                               :medium 5
                               :large 10}]
                (:small constants) => 1
                (:medium constants) => 5
                (:large constants) => 10)

              ; as directives
              (defn pour [lb ub]
                (cond
                  (= ub :tojours) (iterate inc lb)
                  :else (range lb ub))) ; :else is a directive for cond
              (pour 1 10) => '(1 2 3 4 5 6 7 8 9)
              ; (pour 1 :tojours) => ... runs forever
              )
       (facts "qualifying your keywords 4.3.2"
              ; keywords don't belong to any specific namespace
              :not-in-ns => :not-in-ns
              `:not-in-ns => :not-in-ns
              ':not-in-ns => :not-in-ns
              ; if namespace qualification is used ...
              ::not-in-ns => :spikes.scalars.keywords-test/not-in-ns
              ; the prefix portion :spikes.scalars.keywords-test/
              ; is not denoting a namespace but it's inserted by the reader
              :user/in-another => :user/in-another

              ;(defn do-blowfish [directive]
              ;  (case directive
              ;    :aquarium/blowfish (println "feed the fish")
              ;    :crypto/blowfish   (println "encode the message")
              ;    :blowfish          (println "not sure what to do")))

              ;(ns crypto)
              ;::blowfish => 0
              ;(spikes.scalars.keywords-test/do-blowfish :blowfish) => "not sure what to do"
              ;(spikes.scalars.keywords-test/do-blowfish ::blowfish) => "encode the message"

              ;(ns aquarium)
              ;(spikes.scalars.keywords-test/do-blowfish :blowfish) => "not sure what to do"
              ;(spikes.scalars.keywords-test/do-blowfish ::blowfish) => "feed the fish"
              )
       )

