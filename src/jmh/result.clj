(ns ^:internal ^:no-doc jmh.result
  "Transform jmh benchmark results into Clojure data."
  (:require [jmh.util :as util]
            [jmh.option :as option]
            [jmh.state :as state]
            [clojure.edn :as edn])
  (:import [clojure.lang Compiler MapEntry]
           [java.util Map$Entry]
           [java.util.concurrent TimeUnit]
           [org.openjdk.jmh.infra BenchmarkParams IterationParams]
           [org.openjdk.jmh.results Result RunResult SampleTimeResult SingleShotResult]
           [org.openjdk.jmh.runner IterationType]
           [org.openjdk.jmh.runner.options TimeValue]
           [org.openjdk.jmh.util ScoreFormatter Statistics]))

(defprotocol Edn
  (edn [x]
    "Return a normalized representation of an object as simple data."))

(def ^:private assoc-sorted (fnil assoc (sorted-map)))

(def time-keyword
  {TimeUnit/NANOSECONDS :ns
   TimeUnit/MICROSECONDS :us
   TimeUnit/MILLISECONDS :ms
   TimeUnit/SECONDS :sec
   TimeUnit/MINUTES :min
   TimeUnit/HOURS :hr})

(defn demunge [^String s]
  (Compiler/demunge s))

(defn time-tuple [^TimeValue tv]
  [(.getTime tv) (time-keyword (.getTimeUnit tv))])

;;;

(defn transform
  "Return a sequence of benchmark result maps from the collection of
  result objects produced by the Runner."
  [env]
  (let [benchmarks (vec (:jmh/benchmarks env))
        class (:jmh/benchmark-class env)
        pat (re-pattern (format "^(?:\\Q%s\\E\\.)?_(\\d+)_" class))
        presolver (:jmh/param-resolver env)
        sort (partial sort-by (juxt :method :index))

        benchmark-data
        (memoize
         (fn [s]
           (when-let [[_ m] (re-find pat s)]
             (let [idx (Long/valueOf ^String m)
                   b (nth benchmarks idx)
                   args (map #(get presolver % %) (:args b))]
               (util/some-assoc
                (with-meta {} {:jmh/options (:options b)})
                :args args
                :fn (:fn b)
                :index idx
                :name (:name b))))))]

    (when (option/debug? (:jmh/options env))
      (util/debug "Transforming result"))

    (sort
     (for [^RunResult r (:jmh/result env)]
       (let [p (.getParams r)
             bname (.getBenchmark p)

             init (if (.startsWith bname class)
                    (or (benchmark-data bname)
                        (let [idx (inc (.lastIndexOf bname "."))
                              gname (demunge (subs bname idx))]
                          {:fn (keyword "group" gname) :name (keyword gname)}))
                    (let [idx (.lastIndexOf bname ".")
                          method (symbol (subs bname 0 idx)
                                         (subs bname (inc idx)))]
                      {:method method}))

             secondary (reduce-kv
                        (fn [m ^String k v]
                          (if-let [[_ pct] (re-find #"\u00b7p([\d.]+)$" k)]
                            (let [pct (* 100 (double (Double/valueOf ^String pct)))]
                              (update m :percentiles assoc-sorted pct (edn v)))
                            (if-let [b (benchmark-data k)]
                              (update m :secondary assoc-sorted (:name b (:fn b))
                                      (merge b (edn v)))
                              (let [kind (if (.startsWith k "\u00b7")
                                           (subs k 1)
                                           k)]
                                (update m :secondary assoc-sorted kind (edn v))))))
                        {} (into {} (.getSecondaryResults r)))

             nprefix (count state/param-field-prefix)
             params (reduce
                     (fn [m ^String k]
                       (let [v (.getParam p k)]
                         (if (.startsWith k state/param-field-prefix)
                           (assoc-sorted m (keyword (demunge (subs k nprefix)))
                                         (edn/read-string v))
                           (assoc-sorted m k v))))
                     nil (.getParamsKeys p))]
         (merge init
                (util/some-assoc
                 {}
                 :fork {:count (.getForks p)
                        :warmups (.getWarmupForks p)
                        :jvm {:java (.getJvm p)
                              :args (vec (.getJvmArgs p))
                              :jdk (.getJdkVersion p)
                              :jmh (.getJmhVersion p)}}
                 :iterations {:synchronize (.shouldSynchIterations p)}
                 :measurement (edn (.getMeasurement p))
                 :mode (util/mode-name? (.getMode p))
                 :ops-per-invocation (.getOpsPerInvocation p)
                 :output-time-unit (time-keyword (.getTimeUnit p))
                 :params params
                 :timeout (time-tuple (.getTimeout p))
                 :threads (.getThreads p)
                 :thread-groups (zipmap (for [s (.getThreadGroupLabels p)
                                              :let [b (benchmark-data s)]]
                                          (or (:name b (:fn b)) s))
                                        (.getThreadGroups p))
                 :warmup (edn (.getWarmup p)))
                (edn (.getPrimaryResult r))
                secondary))))))

;;;

(def ^{:dynamic true, :tag 'long}
  *min-histogram-bins*
  (Integer/getInteger "jmh.histogramBins" 10))

(extend-protocol Edn
  IterationParams
  (edn [p]
    (let [tv (.getTime p)
          tn (.getTime tv)]
      (util/some-assoc
       {}
       :count (.getBatchSize p)
       :iterations (.getCount p)
       :time (when (pos? tn) (time-tuple (.getTime p))))))

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
          stats (when (and (> (.getN stats) 2)
                           (or (instance? SampleTimeResult r)
                               (instance? SingleShotResult r)
                               (not (ScoreFormatter/isApproximate (.getScore r)))))
                  (edn stats))]
      (with-meta
        (util/some-assoc
         {}
         :samples samples
         :score score
         :score-confidence conf
         :score-error error
         :statistics stats)
        {:jmh/extended-info (.extendedInfo r)})))

  Statistics
  (edn [s]
    (let [pcts (reduce #(assoc-sorted % %2 (.getPercentile s %2))
                       nil [0.00, 50.0, 90, 95, 99, 99.9
                            99.99, 99.999, 99.9999, 100])
          raw (map #(MapEntry. (.getKey ^Map$Entry %)
                               (.getValue ^Map$Entry %))
                   (iterator-seq (.getRawData s)))

          ;; taken from org.openjdk.jmh.results.Result/printHisto
          nmin (.getMin s)
          nmax (.getMax s)
          bin (Math/pow 10 (Math/floor (Math/log10 (- nmax nmin))))
          bmin (* bin (Math/floor (/ nmin bin)))
          bmax (* bin (Math/ceil (/ nmax bin)))
          nrange (- bmax bmin)
          levels (if (pos? nrange)
                   (loop [bin bin]
                     (if (< (/ nrange bin) *min-histogram-bins*)
                       (recur (/ bin 2))
                       (let [nbin (max 2 (int (Math/ceil (/ nrange bin))))]
                         (mapv #(+ bmin (* (long %) bin)) (range nbin)))))
                   [(- nmin (Math/ulp nmin))
                    (+ nmax (Math/ulp nmax))])
          histogram (map vector
                         (map #(vector (nth levels %) (nth levels (inc (long %))))
                              (range (dec (count levels))))
                         (.getHistogram s (double-array levels)))]
      (with-meta
        {:histogram histogram
         :max nmax
         :mean (.getMean s)
         :min nmin
         :n (.getN s)
         :percentiles pcts
         :stdev (.getStandardDeviation s)
         :sum (.getSum s)
         :variance (.getVariance s)}
        {:jmh/raw-data raw}))))
