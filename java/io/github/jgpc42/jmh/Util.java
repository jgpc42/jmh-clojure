package io.github.jgpc42.jmh;

import clojure.java.api.Clojure;
import clojure.lang.Compiler;
import clojure.lang.IFn;
import clojure.lang.Namespace;
import clojure.lang.Var;

import static clojure.lang.Symbol.intern;

/** Utilities used by jmh proxy classes. */
public final class Util {

    /** Read and evaluate the given string as a fn. */
    public static IFn eval (String form) {
        Object val = Compiler.eval(Clojure.read(form));
        if (val instanceof IFn)
            return (IFn) val;
        throw new RuntimeException("form did not evaluate to fn: " + form);
    }

    /** Read the given string as edn data. */
    public static Object read (String form) {
        Clojure.var("clojure.core", "require").invoke(intern("clojure.edn"));
        return Clojure.var("clojure.edn", "read-string").invoke(form);
    }

    /** Require and resolve as a fn the given var components. */
    public static IFn resolve (String ns, String name) {
        Object val = null;

        Namespace n = Namespace.find(intern(ns));
        if (n != null && (val = Compiler.maybeResolveIn(n, intern(name))) != null)
            val = ((Var) val).deref();

        if (val == null) {
            Clojure.var("clojure.core", "require").invoke(intern(ns));
            val = ((Var) Clojure.var(ns, name)).deref();
        }

        if (!(val instanceof Var.Unbound) && val instanceof IFn)
            return (IFn) val;
        throw new RuntimeException(String.format("%s/%s did not resolve as fn", ns, name));
    }

}
