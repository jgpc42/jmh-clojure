(ns jmh.benchmark-test
  (:require [clojure.test :refer :all]
            [demo.core :as demo]
            [jmh.benchmark :as bench])
  (:import [clojure.lang IFn IFn$OLO]
           [io.github.jgpc42.jmh Util]))

(deftest test-normalize
  (testing "var"
    (is (= {:fn 'clojure.core/map}
           (bench/normalize #'map))))

  (testing "main"
    (is (= {:fn 'foo/-main}
           (bench/normalize {:fn 'foo}))))

  (testing "expr"
    (is (= {:fn `#(rand-int 42), :name :expr}
           (bench/normalize (list `fn* [] (list `rand-int 42)))))))

(deftest test-flatten
  (is (= '[{:ns foo, :fn foo/bar, :apply true}
           {:ns foo, :fn foo/quux, :apply true}]
         (bench/flatten {:ns 'foo, :fn '[bar quux]
                         :apply true}))))

(deftest test-fn-field-info
  (testing "no arglist"
    (is (= {:desc [Object Object Object], :return Object, :type IFn}
           (bench/fn-field-info {:fn `demo/tuple
                                 :args [:x :jmh/blackhole]}))))

  (testing "prim"
    (is (= {:desc [Object :long Object], :return Object, :type IFn$OLO}
           (bench/fn-field-info {:fn `demo/index
                                 :args [:s :i]}))))

  (testing "variadic"
    (is (= {:desc [Object Object], :return Object, :type IFn}
           (bench/fn-field-info {:fn `demo/vararg
                                 :args [:seq]
                                 :apply true})))))

(deftest test-class-fields
  (is (= 2 (count (bench/class-fields
                   {:fn 'f, :apply true, :type Object})))))

(deftest test-method-emits
  (let [b {:fn `demo/index, :args [:seq], :apply true}
        info (bench/fn-field-info b)
        emits (bench/method-emits (merge b info))
        {:keys [invoke prelude]} emits]
    (is (= 2 (count prelude)))
    (is (= [:invokeinterface IFn "invoke" [Object Object Object]]
           invoke))))

(deftest test-method-desc
  (let [b {:fn `demo/index, :args [:state/foo :state/i], :return Object
           :jmh/resolver {:state/i "i", :state/foo 'pkg.Foo}}]
    (is (= ['pkg.Foo "i" Object]
           (bench/method-desc b)))))
