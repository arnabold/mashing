(ns spikes.core-test
  (:use clojure.test spikes.core))

(deftest pairs-of-values
  (let [args ["--server" "localhost"
              "--port" "8080"
              "--environment" "production"]]
    (is (= {:server "localhost"
            :port "8080"
            :environment "production"}
           (parse-args args)))))

