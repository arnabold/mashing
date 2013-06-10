; load in classpath: src
(load "spikes/learn_a_language/important_phrases") 
; compiles in classpath: target/classes
(set! *compile-path* "target/classes")
(compile 'spikes.learn-a-language.important-phrases) 
