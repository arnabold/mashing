(ns spikes.learn-a-language.important-phrases
  (:gen-class)) ; generate a named java class when it compiles the namespace
                ; so we can use -main as an entry point

;; It's time for some German love! 
(def german
  [["I love you." "Ich liebe dich."]
   ["You make me so happy!" "Du machst mich so glucklich!"]
   ["I miss you." "Ich vermisse dich./Du fehlst mir."]
   ["Pass me the mustard." "Gib mir den Senf."]   
   ["Kiss me!" "Kuss mich!"]])

(defn -main
  [which]
  (let [phrases (get german (Integer. which))]
    (println "English: " (first phrases))
    (println "German: " (second phrases))))

