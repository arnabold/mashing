package org.arnabold.clojure;

public class Conversation {    
    public static void main(String[] args) {
        // The "ns1" prefix is necessary because ShyGhost belongs to
        // the "ns1" package, and we haven't imported the classes in
        // that package
        org.arnabold.clojure.ns1.ShyGhost shyGhost = new org.arnabold.clojure.ns1.ShyGhost();
        shyGhost.talk();
    }
}


