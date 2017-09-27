(ns jmh.instrument-test
  (:require [jmh.instrument :as inst]
            [jmh.core :as core]
            [jmh.test-util :as test]
            [demo.core :as demo]
            [clojure.test :refer :all]))

(deftest test-intern-fn
  (let [env (inst/env)
        _ (inst/with-instrumentation env
            (let [v1 (inst/intern-fn (fn [a b] (+ a b)) :foo)
                  v2 (inst/intern-fn (fn [x] (assert false "bar")) :bar)
                  v3 (inst/intern-fn #'demo/index :quux)]
              (is (= 59 (v1 42 17)))
              (is (thrown-with-msg? AssertionError #"bar\nfalse$" (v2 :x)))
              (is (= \q (v3 "quux" 0)))
              (is (= \q (.invokePrim @v3 "quux" 0)))))
        log (inst/log env)]

    (is (= [{:args [42 17], :ret 59}]
           (:foo log)))
    (is (-> log :bar first :thrown))
    (is (= [{:args ["quux" 0], :ret \q}
            {:arglist '[s idx], :args ["quux" 0], :ret \q}]
           (:quux log)))
    (is (= '[String long]
           (->> log :quux second :arglist (map (comp :tag meta)))))))

(deftest ^:integration test-sample
  (let [opts (assoc test/options :instrument true)
        result (core/run test/sample-env opts)
        log (-> result meta :jmh/log)]

    (testing "result"
      (is (= 9 (count result))))

    (testing "spin"
      (is (= 2 (count (:jmh.benchmark.0/jmh.sample.spin log)))))

    (testing "sum"
      (is (= 3 (count (:jmh.state.random-num.trial.setup/expr log))))
      (is (= 2 (count (:jmh.benchmark.1/jmh.sample.sum log)))))

    (testing "sum-seq"
      (is (= 1 (count (:jmh.state.random-nums.trial.setup/jmh.sample.random-nums log))))
      (is (= 2 (count (:jmh.benchmark.2/jmh.sample.sum-seq log)))))

    (testing "add"
      (is (= 2 (count (:jmh.benchmark.3/jmh.sample.add log)))))

    (testing "inc"
      (is (= 2 (count (:jmh.benchmark.4/jmh.sample.inc log))))
      (is (= 'long
             (-> log :jmh.benchmark.4/jmh.sample.inc first :arglist meta :tag))))

    (testing "hashing"
      (is (= 2 (count (:jmh.state.int-array.trial.setup/expr log))))
      (is (= 2 (count (:jmh.benchmark.5/jmh.sample.hasheq log))))
      (is (= 2 (count (:jmh.benchmark.6/jmh.sample.hashcode log)))))

    (testing "consume"
      (is (= 2 (count (:jmh.state.service.trial.setup/jmh.sample.->MockApi log))))
      (is (= 4 (count (:jmh.state.service.iteration.setup/jmh.sample.start! log))))
      (is (= 4 (count (:jmh.state.service.iteration.teardown/jmh.sample.stop! log))))
      (is (= 2 (count (:jmh.state.service.trial.teardown/jmh.sample.dispose! log))))
      (is (= 2 (count (:jmh.state.temp-file.trial.setup/jmh.sample.temp-file log))))
      (is (= 2 (count (:jmh.state.temp-file.trial.teardown/clojure.java.io.delete-file log))))
      (is (= 2 (count (:jmh.state.random-bytes.trial.setup/jmh.sample.random-bytes log))))
      (is (= 2 (count (:jmh.state.composite.trial.setup/clojure.core.vector log))))
      (is (= (+ 2 2) (count (:jmh.benchmark.7/jmh.sample.consume log)))))))
