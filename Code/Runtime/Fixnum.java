package org.appliedlanguage.energyfoolsthemagician;

public final class Fixnum {
    private final long value;

    public Fixnum(long value) {
        this.value = value;
    }
    public static final Fixnum ofValue(long value) {
        // ABCL and CPython cache fixnum objects in some small interval,
        // eg (-3, 256).
        return new Fixnum(value);
    }
    public final long value() { return value; }
    
    public String toString() { return String.format("%d", this.value); }
    public boolean equals(Object other) {
        if (other == this) return true;
        if (!(other instanceof Fixnum)) return false;
        return ((Fixnum)other).value == this.value;
    }
}
