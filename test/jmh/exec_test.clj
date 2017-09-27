(ns jmh.exec-test
  (:require [jmh.exec :as exec]
            [clojure.test :refer :all])
  (:import [java.io PrintStream]))

(deftest test-benchmark-arguments
  (let [benchmarks '[{:class "_a_"}
                     {:class "_b_", :warmup true}]
        externs '[p.C
                  {:class p.D, :select #"\$bench$"}
                  {:class p.E, :warmup true}]
        wmb (str "(?:" "^\\Q_b_\\E\\..+" "|"
                       "^\\Qp.E\\E\\..+" ")")]
    (is (= ["-e" wmb
            "-wmb" wmb
            (str "(?:" "^\\Q_a_\\E\\..+"        "|"
                       "^\\Qp.C\\E\\..+"        "|"
                       "^\\Qp.D\\E\\.\\$bench$" ")")]
           (exec/benchmark-arguments benchmarks externs)))))

(deftest test-progress-stream
  (let [events (atom [])
        add (partial swap! events conj)
        ps (exec/progress-print-stream add)]
    (.println ps "foo")
    (.flush ps)
    (is (= [] @events))

    (.print ps "# Run progress: 60.00% complete, ETA 00:01:35\r")
    (.flush ps)
    (is (= [] @events))

    (.print ps "\n")
    (.flush ps)
    (is (= [{:eta (+ 60 35)
             :percent 0.6}]
           @events))

    (.println ps "# Run progress: 71.1% complete, ETA 10:00:17")
    (.flush ps)
    (is (= [{:eta (+ (* 10 60 60) 17)
             :percent 0.711}]
           (next @events)))

    (.println ps "# Run complete. Total time: 00:05:59")
    (.flush ps)
    (is (= [{:eta 0, :percent 1.0, :complete true
             :duration (+ (* 5 60) 59)}]
           (nthnext @events 2)))))
