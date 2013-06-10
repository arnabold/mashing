(ns spike-ring.run
  (:use [ring.adapter.jetty :only [run-jetty]] 
     ;; [spike-ring.wrapper :only [app]] 
        [spike-ring.middleware :only [app]] ))

;; I like to leave the web browser of the gods:
;; $ watch -d -n 1 curl -sv http://localhost:8080/ 
;; running in a terminal somewhere.

(defn -main []
  ;; (run-jetty handler {:port 8080})
  (defonce ;; if we reload this file or re-evaluate this line nothing
    ;; will happen.That prevents us from accidentaly creating multiple
    ;; copies of the server.
    server
    (run-jetty
     ;; #'handler
     #'app
     ;; referring to the application via #' means that ring
     ;; sees the variable #'handler rather than the function handler
     ;; which that variable evaluates to. If we reevaluate the
     ;; definition, the behaviour the browser sees will change.
     {:port 8080
      :join? false ;; the evaluating thread won't wait for the server
      ;; to finish
      }))
  ;; to stop the server
  ;; (.stop server)
  ;; to (re)start the server
  ;; (.start server)
  )

