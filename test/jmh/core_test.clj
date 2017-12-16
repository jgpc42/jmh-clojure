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
                    :params {:jmh/externs {:amount 100}}
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

(deftest ^:integration test-run
  (let [env `{:benchmarks [rand]
              :selectors {:x (constantly true)
                          :y (constantly false)}}]

    (testing "all"
      (is (seq (core/run env test/options))))

    (testing "empty"
      (is (thrown-with-msg?
           Exception #"^no benchmarks"
           (core/run {}))))

    (testing "none selected"
      (are [m] (thrown-with-msg?
                Exception #"^no benchmarks"
                (core/run env (merge test/options m)))
        {:select :y}
        {:select :y, :warmups {:select :x}}))))
