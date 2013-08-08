package org.arnabold.clojure;

public final class AnImmutableObject {
  public AnImmutableObject(Long l, String s) {
    this.l = l;
    this.s = s;
  }
  // java.lang.String is immutable
  public String getString() { return s; }
  // java.lang.Long is immutable
  public Long getLong() { return l; }
  public boolean equals(Object o) {
    boolean result = false;
    if ((o != null) && 
        (o instanceof AnImmutableObject)) {
      if ((l.equals(((AnImmutableObject) o).l)) && 
          (s.equals(((AnImmutableObject) o).s))) {
        result = true;
      } 
    }
    return result;
  }
  public int hashCode() {
    return l.hashCode() + s.hashCode();
  }
  private final Long l;
  private final String s;
}
