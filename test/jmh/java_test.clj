(ns jmh.java-test
  (:require [jmh.java :as java]
            [clojure.test :refer :all])
  (:import [io.github.jgpc42.jmh DelegateClassLoader]))

(deftest test-compile
  (let [srcs [["jmh.java_test.Quux"
               (str "package jmh.java_test;"
                    "public class Quux extends Bar {"
                    "public int foo () { return super.foo() + 1; } }")]

              ["jmh.java_test.Bar"
               (str "package jmh.java_test;"
                    "public class Bar implements Foo {"
                    "public int foo () { return 42; } }")]

              ["jmh.java_test.Foo"
               (str "package jmh.java_test;"
                    "public interface Foo { public int foo (); }")]]

        results (java/compile srcs)
        cmap (reduce #(assoc % (:name %2) (:bytes %2)) {} results)
        loads (atom [])
        cl (DelegateClassLoader.
            (fn [cname]
              (swap! loads conj (re-find #"\.[^.]+$" cname))
              (get cmap cname)))]

    (doseq [res results]
      (Class/forName (:name res) true cl))

    (is (= 43 (eval `(.foo (jmh.java_test.Quux.)))))
    (is (.getPackage (Class/forName "jmh.java_test.Quux" true cl)))
    (is (= [".Quux" ".Bar" ".Foo" ".Object"] @loads))))

(deftest test-compile-error
  (let [srcs {"foo.quux" "packag foo; impor bar;"}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"/foo/quux\.java: line 1: .+\n\.\.\. and 1 more$"
                          (java/compile srcs)))))
