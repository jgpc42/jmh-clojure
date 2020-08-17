(ns jmh.core-test
  (:require [jmh.core :as core]
            [jmh.sample :as sample]
            [jmh.test-util :as test]
            [clojure.pprint :refer [pprint] :rename {pprint pp}]
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
        method #(symbol extern %)
        opts (assoc test/options
                    :params {:jmh/externs {:amount 100}}
                    :externs [extern])
        result (core/run test/sample-env opts)]
    (is (= `[[sample/spin 0]
             [sample/sum 0] [sample/sum 0]
             [sample/add 0] [sample/add 0]
             [sample/hasheq 1] [sample/hashcode 1]
             [sample/consume 2] [sample/consume 2]]
           (for [r (take 9 result)]
             [(:fn r) (count (:params r))])))

    (is (= `#{[~(method "spin") 1]
              [~(method "nothing") 1]}
           (set (for [r (drop 9 result)]
                  [(:method r) (count (:params r))]))))

    (is (= `[jmh.sample/spin
             jmh.sample/sum jmh.sample/sum
             jmh.sample/add jmh.sample/add
             jmh.sample/hasheq jmh.sample/hashcode
             ~(method "spin")]
           (for [r (core/run test/sample-env
                     (assoc opts
                            :select :non-void
                            :externs [{:class extern :select #"spin"}]))]
             (:fn r (:method r)))))

    (is (= `[jmh.sample/spin
             jmh.sample/add
             jmh.sample/hasheq jmh.sample/hashcode
             jmh.sample/consume jmh.sample/consume
             ~(method "spin")]
           (for [r (core/run test/sample-env
                     (assoc opts
                            ;; :status true, :verbose true, :mode :sample
                            :warmups {:select [:sum :inc]}
                            :externs [{:class extern :select #"spin"}
                                      {:class extern :select #"nothing" :warmup true}]))]
             (:fn r (:method r)))))

    ;; disabled for CI
    #_
    (let [result (core/run
                   test/sample-env
                   (-> (assoc opts
                              :fork {:count 1 :warmups 0}
                              :status true, :verbose true)
                       (dissoc :mode)))]
      ;; (binding [*print-meta* true] (prn result))
      (is (= 11 (count result)))
      (is (= 9 (count (filter :index result)))))))

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

(deftest ^:integration test-grouped
  (let [put '(fn [q] (.offer q :ok 10 java.util.concurrent.TimeUnit/MILLISECONDS))
        take '(fn [q] (.poll q 10 java.util.concurrent.TimeUnit/MILLISECONDS))
        make '(fn [n] (java.util.concurrent.ArrayBlockingQueue. n))
        benchmarks `[{:name :put, :args [:q], :fn ~put, :options {:threads 3}}
                     {:name :take, :args [:q] :fn ~take, :options {:threads 1}}]
        states `{:q {:fn ~make, :args [:n], :scope :group}}
        opts {:group :g
              :fork {:count 1 :warmups 0}
              :measurement 2
              :mode :single-shot
              :params {:n 1}}
        env `{:benchmarks ~benchmarks, :states ~states}
        [res & more] (core/run env opts)]
    #_(pp res)
    (is (nil? more))
    (is (= {:put 3, :take 1} (:thread-groups res)))))
