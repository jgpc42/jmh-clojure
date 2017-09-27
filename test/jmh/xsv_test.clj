(ns jmh.xsv-test
  (:require [jmh.xsv :as xsv]
            [clojure.test :refer :all]))

(deftest test-parse
  (let [in (str "foo,bar,\"baz\"\r\n"
                "42,,ok\r\n"
                "\"17\",\"\"\"hmm\"\".\",\" \r\n,\"\r\n")]
    (is (= [{"foo" "42", "bar" "", "baz" "ok"}
            {"foo" "17", "bar" "\"hmm\".", "baz" " \r\n,"}]
           (xsv/parse in)))))
