(ns spikes.exceptions-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts  "exceptional circumstances 2.8"
       ; throw
       ; catch
       (facts "a little pitch and catch 2.8.1"
              (throw (Exception. "I done throwed")) => (throws Exception "I done throwed")
              (defn throw-catch [f]
                [
                 (try
                   (f)
                   (catch ArithmeticException e "No dividing by zero!")
                   (catch Exception e (str "You are so bad " (.getMessage e)))
                   ;(finally (println "returning...")))])
                   (finally ()))
                 ])
              (throw-catch #(/ 10 5)) => [2]
              (throw-catch #(/ 10 0)) => ["No dividing by zero!"]
              (throw-catch #(throw (Exception. "foo"))) => ["You are so bad foo"]
              )
       )
