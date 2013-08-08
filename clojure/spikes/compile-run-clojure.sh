mkdir -p target/classes

java -cp .:src:target/classes:/home/arnabold/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar clojure.main compile.clj

ls -la target/classes/spikes/learn_a_language

java -cp target/classes:/home/arnabold/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar spikes/learn_a_language/important_phrases 0

java -cp target/classes:/home/arnabold/.m2/repository/org/clojure/clojure/1.5.1/clojure-1.5.1.jar spikes/learn_a_language/important_phrases 3
