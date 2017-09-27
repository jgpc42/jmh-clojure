(ns jmh.state-test
  (:require [clojure.test :refer :all]
            [jmh.state :as state]
            [demo.core :as demo])
  (:import [org.openjdk.jmh.infra Blackhole]))

(deftest test-fixture-seq
  (let [lc #(apply hash-map :level %, :phase %2, :fn %3 %&)
        ts (lc :trial :setup 'f)
        td (lc :trial :teardown 'g)
        tsa (assoc ts :args [:x])
        tda (assoc td :args [:x])
        rs (assoc ts :level :iteration)
        rd (assoc td :level :iteration)
        rsa (assoc rs :args [:x])]

    (are [xs m] (= (set xs) (set (state/fixture-seq 'm)))
      []        {}
      [ts]      f
      [ts]      {:fn f}
      [tsa]     {:fn f, :args [:x]}
      [ts]      {:setup f}
      [ts]      {:setup {:fn f}}
      [tsa]     {:setup {:fn f, :args [:x]}}
      [tsa td]  {:setup f, :args [:x], :teardown g}
      [ts tda]  {:setup f, :teardown {:fn g, :args [:x]}}
      [tsa td]  {:setup {:fn f, :args [:x]}, :teardown {:fn g}}
      [rs]      {:iteration f}
      [rsa]     {:iteration {:fn f, :args [:x]}}
      [rsa rd]  {:iteration {:fn f, :setup {:args [:x]}, :teardown g}})

    (is (= #{(lc :trial :setup 'a, :args [:x], :void true)
             (lc :trial :teardown 'b)
             (lc :iteration :setup 'c, :args [:y])
             (lc :iteration :teardown 'd)
             (lc :invocation :setup 'e)}
           (set (state/fixture-seq '{:fn a, :args [:x]
                                     :setup {:void true}
                                     :trial {:teardown b}
                                     :iteration {:fn c, :args [:y], :teardown d}
                                     :invocation e}))))))

(deftest test-fixtures
  (let [s {:fn 'map, :args [:x]
           :teardown `rand, :prim false}]
    (is (= #{{:state s, :level :trial, :phase :setup, :fn `map, :args [:param/x]}
             {:state s, :level :trial, :phase :teardown, :fn `rand}}
           (set (state/fixtures s))))))

(deftest test-fn-field-desc
  (is (= [Object]
         (state/fn-field-desc {:fn `demo/tuple, :level :trial, :phase :setup})))
  (is (= [Object Object Object]
         (state/fn-field-desc {:fn `demo/tuple, :args [:param/p]}))))

(deftest test-method-desc
  (let [s {:jmh/resolver {:state/x "x", :state/y 'pkg.Foo}}]
    (is (= ["x" Blackhole 'pkg.Foo :void]
           (state/method-desc {:args [:param/p, :state/x, :jmh/blackhole, :state/y]
                               :state s})))))

(deftest test-load-indexes
  (are [xs ys] (= xs (state/load-indexes {:args ys}))
    [1 2]       [:a :b]
    [1 1 2]     [:param/a :b :c]
    [1 2 2 3 3] [:a :param/b :c :param/d :e]))
