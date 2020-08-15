(ns jmh.env-test
  (:require [jmh.env :as env]
            [demo.core :as demo]
            [clojure.test :refer :all]))

(deftest test-merge-options
  (let [xs [{:a 42} :x]
        selectors {:x {:b 2}, :jmh/default {:a 2, :c 3}}
        opts {:a 1, :d 4}]
    (is (= {:fail-on-error true, :a 1, :b 2, :c 3, :d 4}
           (env/merge-options xs selectors opts)))
    (is (= {:fail-on-error true, :group :g, :fork {:count 1},
            :group-threads 3, :mode :average}
           (env/merge-options
            [:g {:threads 3}]
            {:g {:group :g :threads 1}, :jmh/default {:fork 2 :mode :average}}
            {:fork 1})))))

(deftest test-filter-by-selectors
  (let [selectors {:a #(#{1 2} %)
                   :b #(zero? (mod % 3))
                   :c #(zero? (mod % 5))
                   :d (constantly false)}]
    (are [s xs] (= xs (seq (env/filter-by-selectors
                            selectors s (range 11))))
      [] (range 11)
      [:a] [1 2]
      [:b] [0 3 6 9]
      [:a :b] [0 1 2 3 6 9]
      [:a :b :c] [0 1 2 3 5 6 9 10]
      [:d] nil
      [:a :d] [1 2]
      [:e :f] nil)))

(deftest test-select-benchmarks
  (let [benchmarks `[{:name :a, :fn vector}
                     {:name :b, :fn vector}
                     {:name :c, :fn vector}
                     {:name :d, :fn vector}
                     {:name :e, :fn vector}]
        selectors {:b (comp #{:a} :name)
                   :f (constantly false)}
        opts {:select [:b :c :e :x]
              :warmups {:select [:d :e]}}]
    (are [n m] (= n (count (env/select-benchmarks selectors m benchmarks)))
      5 {}
      0 {:select :f}
      1 {:select :f, :warmups {:select :a}}
      1 {:select :a, :warmups {:select :f}})
    (is (= [{:name :a}
            {:name :c}
            {:name :d :warmup true}
            {:name :e :warmup true}]
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
