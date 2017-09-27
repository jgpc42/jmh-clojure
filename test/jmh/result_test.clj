(ns jmh.result-test
  (:require [jmh.result :as result]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest test-merge-secondary
  (let [result '[{:index 0}
                 {:index 0, :params {:p "foo"}}
                 {:method x}]
        profilers '[{:index 0, :profiler :foo, :score 42}
                    {:method x, :profiler :foo, :score 100}
                    {:index 0, :profiler :bar, :score 17}
                    {:index 0, :profiler :foo, :score 3.14
                     :params {:p "foo"}}]]
    (is (= '[{:index 0
              :profilers {:foo {:score 42}, :bar {:score 17}}}
             {:index 0, :params {:p "foo"}
              :profilers {:foo {:score 3.14}}}
             {:method x
              :profilers {:foo {:score 100}}}]
           (result/merge-secondary :profiler profilers result)))))

(deftest test-benchmark-seq
  (let [output (slurp (io/resource "test-output.csv"))
        [result & more] (result/benchmark-seq "foo" output)]

    (is (nil? more))

    (is (= [:sample 156679 [0.001283 "s/op"] 3.6E-5 1]
           ((juxt :mode :samples :score :score-error :threads)
            result)))

    (is (= {0.0 {:samples 1, :score [0.001005 "s/op"]}
            0.5 {:samples 1, :score [0.001071 "s/op"]}}
           (:percentiles result)))

    (is (= {"class.load" {:samples 200, :score [0.0 "classes/sec"]}}
           (:profilers result)))))
