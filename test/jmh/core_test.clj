(ns jmh.core-test
  (:require [jmh.core :as core]
            [jmh.sample :as sample]
            [jmh.test-util :as test]
            [clojure.set :as set]
            [clojure.test :refer :all]))

(deftest test-profilers
  (is (set/select (comp #{"gc"} :name)
                  (core/profilers))))

(deftest ^:integration test-run-expr
  (is (-> (core/run-expr (vector 42 17)
                         (assoc test/options :fork 1))
          :score)))

(deftest ^:integration test-sample
  (let [extern "io.github.jgpc42.jmh.sample.Benchmarks"
        opts (assoc test/options
                    :arguments ["-p" "amount=100"]
                    :externs [extern])
        result (core/run test/sample-env opts)]
    (is (= `[[sample/spin 0]
             [sample/sum 0] [sample/sum 0]
             [sample/add 0] [sample/add 0]
             [sample/hasheq 1] [sample/hashcode 1]
             [sample/consume 2] [sample/consume 2]
             [~(symbol extern "spin") 1]]
           (for [r result]
             [(:fn r (:method r))
              (count (:params r))])))))
