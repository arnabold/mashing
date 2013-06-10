(ns spike-ring.middleware
  (:use [spike-ring.core :only [handler
                                html-escape
                                format-request]] 
        ;;[clojure.pprint :only [pprint]] 
        [ring.middleware.stacktrace :only [wrap-stacktrace]]
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.cookies :only [wrap-cookies]]
        ))

;; Another way to wrap an handler is to define a middleware.
;; This is a concept from python, and a good demonstration
;; of why dynamically typed functional languages are such pleasant
;; things to use

;; We define wrap-spy as a function which does to any handler what app does to our handler

;; (defn wrap-spy [handler]
;;   (fn [request]
;;     (println "-------------------------------")
;;     (println "Incoming Request:")
;;     (clojure.pprint/pprint request)
;;     (let [response (handler request)]
;;       (println "Outgoing Response Map:")
;;       (clojure.pprint/pprint response)
;;       (println "-------------------------------")
;;       response)))

(defn wrap-spy [handler spyname include-body]
  (fn [request]
    (let [incoming (format-request (str spyname ":\n Incoming Request:") request) ]
      (println incoming)
      (let [response (handler request)]
        (let [r (if include-body response (assoc response :body "#<?>"))
              outgoing (format-request (str spyname ":\n Outgoing Response Map:") r)]
          (println outgoing)
          (update-in response  [:body] (fn[x] (str (html-escape incoming) x (html-escape outgoing)))))))))


;; (def app (wrap-spy handler))
;; (def app (-> handler (wrap-spy)))
;; (def app (-> #'handler (wrap-spy)))

;; (def app (-> #'handler
;;              (wrap-stacktrace) ;; ring.middleware.stacktrace/wrap-stacktrace
;;              (wrap-spy)))

;; (def app (-> #'handler
;;              (wrap-spy "what the handler sees" true)
;;              (wrap-stacktrace) ;; ring.middleware.stacktrace/wrap-stacktrace
;;              (wrap-spy "what the webserver sees" false)))

(def app (-> #'handler
             (wrap-spy "what the handler sees" true)
             (wrap-stacktrace)
             (wrap-params) ;; insert a :query-params key valued by a key
             ;; value map
             (wrap-cookies) ;; insert a key :cookies
             (wrap-spy "what the webserver sees" false)))



