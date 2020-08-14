(ns jmh.benchmark-test
  (:require [clojure.test :refer :all]
            [demo.core :as demo]
            [jmh.benchmark :as bench]
            [clojure.pprint :refer [pprint] :rename {pprint pp}])
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

(deftest test-class-type-of
  (let [benchmarks [{:fn 'f, :apply true, :type Object, :index 0, :method "x" :return :long}
                    {:fn 'g, :type Object, :index 1, :method "y" :return `Object}]
        {:keys [fields methods] :as t} (bench/class-type-of "foo" benchmarks)]
    #_(pp methods)
    (is (= "foo" (:name t)))
    (is (= 3 (count fields)))
    (is (= 3 (count methods)))))
