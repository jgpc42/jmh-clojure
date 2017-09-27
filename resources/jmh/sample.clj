(ns jmh.sample
  "Example fns used by the 'sample.jmh.edn' file.

  Benchmark and state fns are annotated with metadata in this file for
  clarity. This is not required for functionality."
  (:require [clojure.edn :as edn])
  (:import [java.io File]
           [java.util Arrays Date]
           [io.github.jgpc42.jmh.sample Counters]
           [org.openjdk.jmh.infra BenchmarkParams Blackhole]))

(defprotocol Api
  "An HTTP endpoint."
  (fetch [api payload]))

(defprotocol Service
  "A mutable lifecycle."
  (dispose! [service])
  (start! [service])
  (stop! [service]))

;;;

(defn ^:bench add
  "Simple arithmetic."
  (^long [^long n]
   (unchecked-add 1 n))
  ([a b]
   (+ a b)))

(defn ^:bench hashcode
  "Java hashing."
  [^ints arr]
  (Arrays/hashCode arr))

(defn ^:bench hasheq
  "Clojure hashing."
  [^ints arr]
  (hash arr))

(defn ^:bench spin
  "Sleep for a short period."
  []
  (Thread/sleep 1))

(defn ^:bench sum
  "Sum a non-empty sequence of numbers."
  [& xs]
  {:pre [(seq xs)]}
  (apply + xs))

(defn ^:bench consume
  "Discard arguments via a blackhole."
  [^Blackhole hole
   cnt
   api
   [^BenchmarkParams params, edn, ^File temp-file, rand-bytes :as composite]
   ^Counters counters]
  (let [discard (fn [expr]
                  {:pre [expr]}
                  (set! (. counters metric) (inc (.metric counters)))
                  (.consume hole expr))]
    (discard (.contains (.getBenchmark params) "consume"))
    (discard (some #{(str cnt)} (map #(.getParam params %)
                                     (.getParamsKeys params))))
    (discard (= 200 (:status (fetch api nil))))
    (discard (or (instance? Date edn) (:magic edn)))
    (discard (.exists temp-file))
    (discard (pos? (alength rand-bytes)))))

;;;

(defn ^:state random-bytes
  "Generate a random byte-array."
  ^bytes [size]
  (let [sample (vec (range -128 128))]
    (byte-array (take size (repeatedly #(rand-nth sample))))))

(defn ^:state random-nums
  "Generate a random number sequence."
  []
  (take 100 (repeatedly #(rand-int Integer/MAX_VALUE))))

(defn ^:state temp-file
  "Return a new temporary file."
  []
  (File/createTempFile "sample" ".tmp"))

;;;

(defrecord ^:state MockApi []
  Api
  (fetch [api payload]
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body "Hello, world."})
  Service
  (dispose! [api])
  (start! [api])
  (stop! [api]))
