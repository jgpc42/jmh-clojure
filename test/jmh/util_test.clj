(ns jmh.util-test
  (:require [clojure.test :refer :all]
            [clojure.edn :as edn]
            [jmh.util :as util])
  (:import [io.github.jgpc42.jmh Util]))

(deftest test-readable-str
  (is (= 43 (-> (list `inc 42) util/readable-str
                read-string eval))))

(deftest test-matching-arity
  (testing "uniadic"
    (is (= nil    (util/matching-arity '([x] [x x]) 0)))
    (is (= '[x]   (util/matching-arity '([x] [x x]) 1)))
    (is (= '[x x] (util/matching-arity '([x] [x x]) 2)))
    (is (= nil    (util/matching-arity '([x] [x x]) 3))))

  (testing "variadic"
    (is (= '[x]      (util/matching-arity '([x] [x & xs]) 1)))
    (is (= '[x & xs] (util/matching-arity '([x] [x & xs]) 3)))
    (is (= nil       (util/matching-arity '([x & xs]) 0)))))

(deftest test-resolve
  (let [tmp (clojure.lang.Var/intern *ns* (gensym "+") +)]

    (testing "fn"
      (is (= [42 17] ((Util/resolve "demo.core" "tuple")
                      42 17)))
      (is (= 59
             ((Util/resolve (-> *ns* ns-name name)
                            (-> tmp meta :name name))
              42 17))))

    (testing "non-fn"
      (is (thrown-with-msg? RuntimeException #"demo.core/not-a-fn did not resolve as fn"
                            (Util/resolve "demo.core" "not-a-fn"))))

    (testing "unbound"
      (is (thrown-with-msg? RuntimeException #"demo.core/unbound did not resolve as fn"
                            (Util/resolve "demo.core" "unbound"))))))

(deftest test-eval
  (testing "fn"
    (is (= 59 ((Util/eval "(fn [a b] (+ a b))")
               42 17))))

  (testing "non-fn"
    (is (thrown-with-msg? RuntimeException #"did not evaluate to fn: 42"
                          (Util/eval "42")))))

(deftest test-read
  (is (= {:foo 42} (Util/read "{:foo 42}"))))

(deftest test-var-symbol
  (is (= 'clojure.core/map (util/var-symbol #'map))))
