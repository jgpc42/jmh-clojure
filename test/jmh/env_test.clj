(ns jmh.env-test
  (:require [jmh.env :as env]
            [demo.core :as demo]
            [clojure.test :refer :all]))

(deftest test-merge-options
  (let [xs [{:a 42} :x]
        selectors {:x {:b 2}, :jmh/default {:a 2, :c 3}}
        opts {:a 1, :d 4}]
    (is (= {:a 1, :b 2, :c 3, :d 4}
           (env/merge-options xs selectors opts)))))

(deftest test-filter-by-selectors
  (let [selectors {:a #(#{1 2} %)
                   :b #(zero? (mod % 3))
                   :c #(zero? (mod % 5))}]
    (are [s xs] (= xs (env/filter-by-selectors
                       selectors s (range 11)))
      [] (range 11)
      :a [1 2]
      :b [0 3 6 9]
      [:a :b] [0 1 2 3 6 9]
      [:a :b :c] [0 1 2 3 5 6 9 10])))

(deftest test-select-benchmarks
  (let [named #(comp (partial = %) :name)
        benchmarks `[{:name :a, :fn vector}
                     {:name :b, :fn vector}
                     {:name :c, :fn vector}
                     {:name :d, :fn vector}
                     {:name :e, :fn vector}]
        selectors {:a (named :a)
                   :b (named :b)
                   :c (named :c)
                   :d (named :d)
                   :e (named :e)}
        opts {:select [:a :c :e]
              :warmups {:select [:d :e]}}]
    (is (= 5 (count (env/select-benchmarks selectors {} benchmarks))))
    (is (= [{:name :a}
            {:name :c}
            {:name :e}
            {:name :d :warmup true}]
           (->> (env/select-benchmarks selectors opts benchmarks)
                (map #(select-keys % [:name :index :warmup])))))))

(deftest test-used-states
  (let [states '{:a a
                 :b {:fn b, :args [:state/a :p]}
                 :c {:fn c, :args [:state/b :state/d]}
                 :d d
                 :e {:fn e, :args [:state/c :state/f]}
                 :f {:fn f, :args [:state/e]}}]
    (is (= #{:state/a}
           (env/used-states [{:args [:state/a]}] states)))
    (is (= #{:state/a, :state/b, :state/d}
           (env/used-states [{:args [:state/b]},
                             {:args [:state/d]}] states)))
    (is (= #{:state/a :state/b :state/c :state/d}
           (env/used-states [{:args [:state/c]}] states)))
    (is (thrown-with-msg? RuntimeException #"state cycle detected"
                          (env/used-states [{:args [:state/e]}] states)))))

(deftest test-primitive-states
  (is (= #{}
         (env/primitive-states [{:fn `vector, :args [:state/a :state/b]}]))
      (= #{:state/b :state/c}
         (env/primitive-states [{:fn `demo/index, :args [:state/a :state/b]}
                                {:fn `demo/index, :args [:state/b :state/c]}]))))
