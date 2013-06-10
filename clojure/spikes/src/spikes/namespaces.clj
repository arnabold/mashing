(ns spikes.namespaces)

;;; how to run:
;;; java -cp ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar:src clojure.main src/spikes/namespaces.clj

(defn check [coll] 
  (if (= () (filter false? coll)) 
    (println "OK") 
    (println "Error"))) 

(println ";; clojure is a lisp-1 (no funcall needed)")
(defn best [f xs]
  (reduce #(if (f %1 %2) %1 %2) xs))
(check [(= 7 (best > [1 3 4 2 7 5 3]))
        (= 'spikes.namespaces/best `best)
        ])

(println ";; creating namespaces using ns 2.9.1")
(ns joy.ch2 (:refer spikes.namespaces))

(defn hello [] (str "Hello Cleveland"))
(check [(= "joy.ch2" (str *ns*))
        (= 'joy.ch2/hello `hello)
        (= "Hello Cleveland" (hello))]) ; hello is lookedup in joy.ch2

(ns joy.another (:refer spikes.namespaces))
(check [(= "joy.another" (str *ns*))
        (= 'joy.another/hello `hello)
        ; (= "Hello Cleveland" (hello)) ; hello is not resolvable in joy.another
        ])

(println ";; loading other namespaces with :require 2.9.2")

(ns joy.req (:refer spikes.namespaces)
  (:require clojure.set)) ; require loads a namespace but not maps symbols to the current 
 namespace 
(check [(= "joy.req" (str *ns*))
        (= #{ 3 } (clojure.set/intersection #{1 2 3} #{3 4 5}))])

(ns joy.req-alias (:refer spikes.namespaces)
  (:require [clojure.set :as s])) ; loads a namespace and map to a new symbol
(check [(= "joy.req-alias" (str *ns*))
        (= #{ 3 } (s/intersection #{1 2 3} #{3 4 5})) ; namespace symbols can only be used 
        ; as a qualifier
        ])

(println "loading and creating mappings with :use 2.9.3")
(ns joy.use-ex (:refer spikes.namespaces)
  (:use [clojure.string :only [capitalize]]))
(check [(= "joy.use-ex" (str *ns*))
        (= '("Kilgore" "Trout") (map capitalize ["kilgore" "trout"]))])

(ns joy.exclusion (:refer spikes.namespaces)
  (:use [clojure.string :exclude [capitalize]]))
(check [(= "joy.exclusion" (str *ns*))
        ;(map capitalize ["kilgore" "trout"]) ; capitalize is not resolvable
        (= "cba" (reverse "abc"))
        (= "cba" (clojure.string/reverse "abc"))
        (= [\c \b \a] (clojure.core/reverse [\a \b \c]))
        (= "axc" (replace "abc" \b \x))
        (= "axc" (clojure.string/replace "abc" \b \x))
        (= [\a \x \c] (clojure.core/replace {\b \x} [\a \b \c]))])


(println ";; creating mappings with :refer 2.9.4")
; refer load a lib and creates a mapping only for lib already loaded
(ns joy.yet-another 
  (:refer spikes.namespaces))
(check [(= "joy.yet-another" (str *ns*))])

(ns joy.yet-another2 (:refer spikes.namespaces)
  (:refer joy.ch2 :rename {hello hi}))
(check [(= "joy.yet-another2" (str *ns*))
        (= "Hello Cleveland" (hi))])

(println "loading java classes with :import 2.9.5")
(ns joy.java (:refer spikes.namespaces)
  (:import java.util.HashMap 
           java.util.concurrent.atomic.AtomicLong))
(check [(= "joy.java" (str *ns*))
        (= true (get (HashMap. {"happy?" true}) "happy?"))
        (= 42 (.get (AtomicLong. 42)))
        ])

