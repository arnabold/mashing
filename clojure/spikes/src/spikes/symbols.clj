(ns spikes.symbols
  (:use spikes.core))

;;; how to run:
;;; java -cp ~/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar:src \
;;; clojure.main --main spikes.symbols


(defn -main []
  (println ";; symbols (instances of clojure.lang.Symbol) are identifiers")
  (check [(= clojure.lang.Symbol (type 'a))
          (= clojure.lang.Symbol (type `a))])

  (println ";; clojure try to read (eventually compile) and eval forms (i.e.: identifiers)")
  ; a                       ; Unable to resolve symbol: a in this context 
  ; spikes.symbols/a        ; No such var: spikes.symbols/a
  (println ";; reader macro quote (or ') yields the unevaluated form")
  (check [(println "unqualified symbol:" 'a) 
          (= "a" (name 'a)) 
          (= nil (namespace 'a))
          (println "namespace qualified symbol:" 'spikes.symbols/a) 
          (= "a" (name 'spikes.symbols/a)) 
          (= "spikes.symbols" (namespace 'spikes.symbols/a))
          (println "namespace qualified symbol:" 'foo.bar/a) 
          (= "a" (name 'foo.bar/a)) 
          (= "foo.bar" (namespace 'foo.bar/a))]) 

  (println ";; symbols with the same name, but different namespace, are not equal")
  (check [(= (name 'a) (name 'spikes.symbols/a))
          (not (= (namespace 'a) (namespace 'spikes.symbols/a)))
          (not (= 'a 'spikes.symbols/a)) 
          (not (= 'a 'foo.bar/a)) 
          (not (= 'spikes.symbols/a 'foo.bar/a))
          (= 'a (symbol "a"))
          (= 'foo.bar/a (symbol "foo.bar" "a"))
          (= `a (symbol "spikes.symbols" "a"))
          ]) 

  (println ";; reader macro syntax-quote (or `) resolve the form in the current context, yielding a qualified symbol") 
  (check [(= 'spikes.symbols/a `a)
          (= 'spikes.symbols/a `spikes.symbols/a)
          (= 'foo.bar/a `foo.bar/a)       ; already qualified symbol
          (= 'java.lang.String `String)   ; a fully qualified class name
          (= 'spikes.symbols/NonExistingClass `NonExistingClass)
          ])

  (println "unqualified symbol auto-generated:" `a#)

  (println ";; quoting collections, quotes also each items")
  (check ['[a (+ 1 1) 3 b]    ; vector
          `[a (+ 1 1) 3 b]   
          '(a (+ 1 1) 3 b)    ; list
          `(a (+ 1 1) 3 b) 
          '{a (+ 1 1) 3 b}    ; map (must contain an even number of forms)
          `{a (+ 1 1) 3 b} 
          '#{a (+ 1 1) 3 b}
          `#{a (+ 1 1) 3 b}]) ; set

  (println ";; symbols with the same name and namespace are not unique (are not the same object)")
  (check [(= 'a 'a)
          (not (identical? 'a 'a))
          (= `a `a)
          (not (identical? `a `a))
          (= `String `String)
          (not (identical? `String `String))
          ])

  (println ";; each symbol can have its own different metadata (even if name and namespace are equals)")
  (let [x (with-meta 'a {:number 1})
        y (with-meta 'a {:number 2})]
    (check [(symbol? x)
            (= 1 (:number (meta x)))
            (symbol? y)
            (= 2 (:number (meta y)))]))


  (def a 1) ; => #'spikes.symbols/a (a var) 
  (println ";; def macro creates (or retrieves) a var with the name of a symbol")
  (check [(= a spikes.symbols/a)
          (not (= 'a 'spikes.symbols/a))
          (= `a 'spikes.symbols/a)
          (= nil (resolve 'a))              ; unqualified symbol resolves to nil
          (= (var a) (resolve `a))          ; qualified symbol resolves to var
          (= #'spikes.symbols/a (resolve `a))
          (= 1 (var-get (resolve `a)))      ; get the value of the var given the symbol
          ])
  ; (def foo.bar/a 2)       ; can't refer to qualified var that doesn't exists
  ; (def spikes.core/a 2)   ; can't refer to qualified var that doesn't exists

  (println ";; creating a var with a qualified symbol as value")
  (def a-symbol `a)
  ;; a-symbol -> `a 
  ;; a -> 1
  (check [(= 'spikes.symbols/a-symbol `a-symbol)
          (= #'spikes.symbols/a-symbol (resolve `a-symbol))
          (= `a (var-get (resolve `a-symbol)))
          (= 'spikes.symbols/a (var-get (resolve `a-symbol)))
          (= 'spikes.symbols/a a-symbol)
          (= #'spikes.symbols/a (resolve a-symbol))
          (= 1 (var-get (resolve a-symbol)))])

  (println ";; creating a var with an unqualified symbol as value") 
  (def another-symbol 'a) 
  (check [(= 'a another-symbol) 
          (= nil (resolve another-symbol)) ; unqualified symbol a is not resolving to any var 
          ])
  )
