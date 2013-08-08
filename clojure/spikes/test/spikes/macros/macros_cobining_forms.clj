(ns spikes.macros.macros-combining-forms "Macros combining forms 8.3"
    (:use midje.sweet))

(facts "Macros combining forms 8.3"
;; you want to be able to define Vars that call a function whenever
       ;; their root bindings change

       ;; define a var
       ;; define a function that will be the watcher
       ;; call add-watch with the proper values

       ;; add-watch
       ;; reference
       ;; keyword
       ;; function

       (defmacro def-watched [name & value]
         `(do
            (def ~name ~@value)
            (add-watch
             (var ~name)
             :re-bind
             (fn [~'key ~'r old# new#]
               (println old# " -> " new#)))))

       (def-watched x 144)
       (def x 0)
       )
