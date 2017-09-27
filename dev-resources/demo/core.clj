(ns demo.core)

(defn index ^Character [^String s ^long idx]
  (.charAt s idx))

(def not-a-fn 42)

(def tuple vector)

(def unbound)

(defn vararg [x & xs]
  (list* x xs))
