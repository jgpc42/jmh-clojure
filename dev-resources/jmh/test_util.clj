(ns jmh.test-util
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def options
  {:clear-locals true
   :fail-on-error true
   :fork 0
   :measurement {:count 1, :iterations 2}
   :mode :single-shot
   :progress false
   :threads 1
   :warnings false})

(defn env-file [fname]
  (-> fname io/resource slurp edn/read-string))

(def sample-env (env-file "sample.jmh.edn"))
