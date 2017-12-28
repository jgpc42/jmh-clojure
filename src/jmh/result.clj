(ns ^:internal ^:no-doc jmh.result
  "Transform jmh benchmark results into Clojure data."
  (:require [jmh.util :as util]
            [jmh.option :as option]
            [jmh.state :as state]
            [clojure.edn :as edn])
  (:import [clojure.lang MapEntry]
           [java.util Map$Entry]
           [org.openjdk.jmh.infra BenchmarkParams]
           [org.openjdk.jmh.results Result RunResult]
           [org.openjdk.jmh.util ScoreFormatter Statistics]))

(defprotocol Edn
  (edn [x]
    "Return a normalized representation of an object as simple data."))

(def ^:private assoc-sorted (fnil assoc (sorted-map)))

;;;

(defn transform
  "Return a sequence of benchmark result maps from the collection of
  result objects produced by the Runner."
  [env]
  (let [benchmarks (vec (:jmh/benchmarks env))
        pkg (:jmh/pkg env)
        presolver (:jmh/param-resolver env)
        sort (partial sort-by (juxt :method :index))]

    (when (option/debug? (:jmh/options env))
      (util/debug "Transforming result"))

    (sort
     (for [^RunResult r (:jmh/result env)]
       (let [p (.getParams r)
             bname (.getBenchmark p)
             init (if (.startsWith bname pkg)
                    (let [idx (-> (.split bname "_") next
                                  ^String (first) Long/valueOf)
                          b (nth benchmarks idx)
                          args (map #(get presolver % %) (:args b))]
                      (util/some-assoc
                       (with-meta {} {:jmh/options (:options b)})
                       :args args
                       :fn (:fn b)
                       :index idx
                       :name (:name b)))
                    (let [idx (.lastIndexOf bname ".")
                          method (symbol (subs bname 0 idx)
                                         (subs bname (inc idx)))]
                      {:method method}))
             secs (reduce-kv
                   (fn [m ^String k v]
                     (if-let [[_ pct] (re-find #"\u00b7p([\d.]+)$" k)]
                       (let [pct (Double/valueOf ^String pct)]
                         (update m :percentiles assoc-sorted pct (edn v)))
                       (let [prof (if (.startsWith k "\u00b7")
                                    (subs k 1)
                                    k)]
                         (update m :profilers assoc-sorted prof (edn v)))))
                   {} (into {} (.getSecondaryResults r)))]
         (merge init
                (edn p)
                (edn (.getPrimaryResult r))
                secs))))))

;;;

(extend-protocol Edn
  BenchmarkParams
  (edn [p]
    (let [mode (util/mode-name? (.getMode p))
          threads (.getThreads p)

          nprefix (count state/param-field-prefix)
          params (reduce
                  (fn [m ^String k]
                    (let [v (.getParam p k)]
                      (if (.startsWith k state/param-field-prefix)
                        (assoc-sorted m (keyword (subs k nprefix))
                                      (edn/read-string v))
                        (assoc-sorted m k v))))
                  nil (.getParamsKeys p))]
      (util/some-assoc
       {}
       :mode mode
       :params params
       :threads threads)))

  Result
  (edn [r]
    (let [conf (let [i (.getScoreConfidence r)]
                 (when-not (Double/isNaN (aget i 0))
                   (vec i)))
          error (let [v (.getScoreError r)]
                  (when-not (Double/isNaN v)
                    v))
          score [(.getScore r) (.getScoreUnit r)]
          samples (.getSampleCount r)

          stats (.getStatistics r)
          stats (when-not (or (<= (.getN stats) 2)
                              (ScoreFormatter/isApproximate (.getScore r)))
                  (edn stats))]
      (util/some-assoc
       {}
       :samples samples
       :score score
       :score-confidence conf
       :score-error error
       :statistics stats)))

  Statistics
  (edn [s]
    (let [pcts (reduce #(assoc-sorted % %2 (.getPercentile s %2))
                       nil [0.00, 50.0, 90, 95, 99, 99.9
                            99.99, 99.999, 99.9999, 100])
          raw (map #(MapEntry. (.getKey ^Map$Entry %)
                               (.getValue ^Map$Entry %))
                   (iterator-seq (.getRawData s)))]
      (with-meta
        {:max (.getMax s)
         :mean (.getMean s)
         :min (.getMin s)
         :n (.getN s)
         :percentiles pcts
         :stdev (.getStandardDeviation s)
         :sum (.getSum s)
         :variance (.getVariance s)}
        {:jmh/raw-data raw}))))
