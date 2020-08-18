(ns jmh.option-test
  (:require [jmh.option :as opt]
            [clojure.test :refer :all]))

(deftest test-aliases
  (is (thrown-with-msg?
       Exception #"invalid type: :foo"
       (opt/without-type-alias {:type :foo, :x 42 :y 17})))
  (is (= {:x 42, :y 17, :z 1000}
         (opt/without-type-alias {:type :foo :x 42 :y 17}
                                 {:foo {:x 100, :z 1000}})))
  (is (= {:x 42, :y 200, :z 2000}
         (opt/without-type-alias {:type [:foo :bar] :x 42}
                                 {:foo {:x 100, :z 1000}
                                  :bar {:y 200, :z 2000}})))
  (is (= {:x 100, :y 200, :z 1000, :quux "ok"}
         (opt/without-type-alias {:type [:bar :foo] :quux "ok"}
                                 {:foo {:x 100, :z 1000}
                                  :bar {:y 200, :z 2000}}))))
