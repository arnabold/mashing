// The classes defined in this file belong to the "ns1" package.
// Notice that this file is in the "ns1" directory.
package org.arnabold.clojure.ns1;

// This basically means, "I hate typing the namespace prefix all the
// time so please allow me to not do that"
import org.arnabold.clojure.ns2.*;

public class ShyGhost {
    public void talk() {
        // the shy ghost can't speak for himself and has to get
        // someone else to do it for him

        // Notice that even though SuperManlyIguana belongs to the ns2
        // namespace, we don't have to use the ns2 prefix
        SuperManlyIguana smi = new SuperManlyIguana();
        smi.talk();
    }
}


