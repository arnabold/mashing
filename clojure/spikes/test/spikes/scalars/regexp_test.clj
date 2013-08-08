(ns spikes.scalars.regexp-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "regular expressions 4.5"
       (facts "syntax 4.5.1"
              ; a literal regex is compiled to a Pattern at read-time
              (type #"an example pattern") => java.util.regex.Pattern  
              (java.util.regex.Pattern/compile "\\d") => #"\d"
              #"(?d)yo" ; unix lines
              #"(?i)yo" ; case insensitive yo
              #"(?x)yo" ; comments
              #"(?m)yo" ; multilines
              #"(?s)yo" ; dotall
              #"(?u)yo" ; unicode case
              )
       (facts "functions 4.5.2"
              ; split method from Pattern object
              (vec (.split #"," "one,two,three")) => ["one" "two" "three"]
              (seq (.split #"," "one,two,three")) => '("one" "two" "three")

              ; re-seq returns a lazy seq of all matches in a string
              (re-seq #"\w*(\w)" "one-two/three") => '(["one" "e"]
                                                       ["two" "o"]
                                                       ["three" "e"]))
       (facts "beware of mutable matchers 4.5.3"
              ; don't use Matcher or re-matcher, re-groups, re-find
              )
       )

