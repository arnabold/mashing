(ns spikes.macros.data-is-code-is-data "data is code is data 8.1"
    (:use midje.sweet))

(facts "data is code is data 8.1"
       (eval 42) => 42
       (eval '(list 1 2)) => (list 1 2)
       (list 1 2) => '(1 2)
                                        ; (eval (list 1 2)) ; (1 2)
                                        ; ClassCastException
       (symbol "+") => '+
       (eval (list (symbol "+") 1 2)) => (eval '(+ 1 2))
       (eval '(+ 1 2)) => (+ 1 2)
       (+ 1 2) => 3
       
       (facts "syntax-quote, unquote and splicing 8.1.1"
              
              (fact "an implementation of eval taking a local context"
                    ;; unquoting 
                    (let [v 123
                          k 'a]
                      
                      v => 123
                      'v => 'v
                      `v => 'spikes.macros.data-is-code-is-data/v
                      '~v => '(clojure.core/unquote v)
                      `~v => 123
                      ''~v => '(quote (clojure.core/unquote v))
                      ``~v => 'spikes.macros.data-is-code-is-data/v
                      '`~v => 'v
                      `'~v => '(quote 123)
                      `'~123 => '(quote 123)
                      [k `'~v] => ['a '(quote 123)]
                      
                      ((fn [[k v]] [k `'~v])
                       ['a 123]) => ['a '(quote 123)])
                    
                    (mapcat
                     (fn [[k v]] [k `'~v])
                     [['a 123] ['b 124]]) => ['a '(quote 123) 'b '(quote 124)]
                     
                     (mapcat
                      (fn [[k v]] [k `'~v])
                      {'a 123 'b 124}) => ['a '(quote 123) 'b '(quote 124)]

                     `(~@(mapcat (fn [[k v]] [k `'~v]) {'a 123 'b 124})) => ['a '(quote 123) 'b '(quote 124)]

                     `(~@(mapcat (fn [[k v]] [k `'~v]) {'a 123 'b 124})) => '[a (quote 123) b (quote 124)]
                      
                     `(let [~@(mapcat (fn [[k v]] [k `'~v]) {'a 123 'b 124})]) => '(clojure.core/let [a (quote 123) b (quote 124)])

                     `(let [~@(mapcat (fn [[k v]] [k `'~v]) {'a 123 'b 124})] ~'(+ a b)) => '(clojure.core/let [a (quote 123) b (quote 124)] (+ a b))

                     (eval `(let [~@(mapcat (fn [[k v]] [k `'~v]) {'a 123 'b 124})] ~'(+ a b))) => (eval '(clojure.core/let [a (quote 123) b (quote 124)] (+ a b)))

                     (eval '(quote 123)) => (eval '123)
                     (eval ''123) => (eval '123)

                     (eval '(clojure.core/let [a (quote 123) b (quote 124)] (+ a b))) => (let [a 123 b 124] (+ a b))
                      
                     (defn contextual-eval [ctx expr]               
                        (eval                                        
                         `(let [~@(mapcat
                                   (fn [[k v]] [k `'~v])
                                   ctx)
                                ] 
                            ~expr)))
                      (contextual-eval {'a 1 'b 2} '(+ a b)) => 3
                      (contextual-eval {'a 1 'b 2} '(let [b 1000] (+ a b))) => 1001)
              
              (facts "handling nexted syntax-quote"
                     (let [x 9
                           y '(- x)]
                       y => '(- x)
                       `y => 'spikes.macros.data-is-code-is-data/y 
                       ``y => '(quote spikes.macros.data-is-code-is-data/y)
                       ``~y => 'spikes.macros.data-is-code-is-data/y 
                       ``~~y => '(- x)
                       (contextual-eval {'x 36} ``~~y)) => -36)       
              )
       (facts "macros: rules of thumb 8.1.2"
              "Don’t write a macro if a function will do.
                 Reserve macros to provide syntactic abstractions or create binding forms.
               Write an example usage.
               Expand your example usage by hand.
               Use macroexpand, macroexpand-1, and clojure.walk/macroexpand-all[4] liberally
                 to understand how your implementation works.
               Experiment at the REPL.
               Break complicated macros into smaller functions whenever possible."
              )
       )

