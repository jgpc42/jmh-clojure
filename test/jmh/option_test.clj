(ns jmh.option-test
  (:require [jmh.option :as opt]
            [clojure.test :refer :all]))

(deftest test-aliases
  (is (thrown-with-msg?
       Exception #"invalid type: :foo"
       (opt/without-type-alias {:type :foo, :x 42 :y 17})))
  (is (= {:x 42, :y 17, :z 1000}
         (opt/without-type-alias {:type :foo :x 42 :y 17}
                                 {:foo {:x 100, :z 1000}}))))
