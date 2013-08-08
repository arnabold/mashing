(ns spikes.java-interop-test
  (:use midje.sweet)
  (:use clojure.test)
  (:use spikes.core))

(facts "leveraging java via interop 2.7"
       (facts "accessing static class members 2.7.1"
              (class java.util.Locale/JAPAN) => java.util.Locale
              (Math/sqrt 9) => 3.0
              )
       (facts "creating java class instances 2.7.2"
              (class (new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"})) => java.util.HashMap
              (fact "idiomatic form"
                    (class (java.util.HashMap. {"foo" 42 "bar" 9 "baz" "quux"})) => java.util.HashMap)
              )
       (facts "accessing java instance members with the . operator 2.7.3"
              (.x (java.awt.Point. 10 20)) => 10
              (.divide (java.math.BigDecimal. "42") 2M) => 21M
              )
       (facts "setting java instance properties 2.7.4"
              ; in absence of mutators setXXX
              (let [origin (java.awt.Point. 0 0)]
                (set! (.x origin) 15)
                (str origin)) => "java.awt.Point[x=15,y=0]"
              )
       (facts "the .. macro 2.7.5"
              ; method call chaining
              ; /* java code */ new Date().toString().endsWith("2010")
              (.endsWith (.toString (java.util.Date.)) "2013") => truthy
              ; simper to read...
              (.. (java.util.Date.) toString (endsWith "2013")) => truthy
              )
       (facts "the doto macro 2.7.6"
              ; calling a set of methods on the same obejct
              ; /* HashMap props = new HashMap();
              ; props.put("HOME", "/home/me");
              ; props.put("SRC", "src");
              ; props.put("BIN", "classes");
              (doto (java.util.HashMap.)
                (.put "HOME" "/home/me")
                (.put "SRC" "src")
                (.put "BIN" "classes"))
              )
       )

