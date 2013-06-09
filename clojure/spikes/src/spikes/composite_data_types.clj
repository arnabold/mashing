(ns spikes.composite-data-types)

; COMPOSITE DATA TYPES (a.k.a. COLLECTIONS): vectors, lists, queues, sets and maps.

; PERSISTENT COLLECTION: allows to preserve historical versions of its state
(def ja (into-array [:willie :barnaba :adam])) ; a java array
(def cv [:willie :barnaba :adam])
(def cv1 (replace {:barnaba :quentin} cv))

; SEQUENCE ABSTRACTION
;
; SEQUENTIAL COLLECTION: holds a a series of value without reordering them. 
(def v1 [1 2 3])
(def l1 '(1 2 3))
(def jl (java.util.Arrays/asList (into-array [1 2 3])))
(def m1 {'a 1 'b 2 'c 3})
(def s1 #{1 2 3})
;
; SEQUENCE: a sequential collections that represents a series of values 
; that may or may not exist yet.
;
; a 'seq object' is any object that implements the 'seq API' 
; (first coll): returns the first element or nil, 
; (rest coll): returns a sequence other than the first or the empty sequence.
;
; the 'seq function' produces a 'seq object' 

(def hm1 (hash-map :a 1)) ; an hash map
(def shm1 (seq hm1))


