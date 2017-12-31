package io.github.jgpc42.jmh;

import clojure.lang.DynamicClassLoader;
import clojure.lang.IFn;

/** This class is documented in the `jmh.util` namespace. */
public class DelegateClassLoader extends DynamicClassLoader {

    private final IFn _load;

    public DelegateClassLoader (IFn load) {
        super();
        _load = load;
    }

    @Override protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        Object val = _load.invoke(name);

        if (!(val instanceof byte[]))
            return super.loadClass(name, resolve);

        Class c = defineClass(name, (byte[])val, null);

        if (resolve)
            resolveClass(c);

        int i = name.lastIndexOf(".");
        String p = i > 0 ? name.substring(0, i) : null;
        if (p != null)
            try {
                definePackage(p, null, null, null, null, null, null, null);
            }
            catch (IllegalArgumentException e) {
                /* ignored */
            }

        return c;
    }

}
