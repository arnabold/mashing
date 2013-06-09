(ns spike-ring.wrapper
  (:use [spike-ring.core :only [handler]] 
        [clojure.pprint :only [pprint]]))

;; we wrap the handler in a wrapper app that prints the incoming and
;; outgoing data. We need to check the server definition too.
;; Output of println will be on a terminal if launched with lein run
;; or on the *nrepl-server* if launched from repl
(defn app [request]
  (println "-------------------------------")
  (println "Incoming Request:")
  (pprint request)
  (let [response (handler request)] ;; calling the handler
    (println "Outgoing Response Map:")
    (pprint response)
    (println "-------------------------------")
    response))
