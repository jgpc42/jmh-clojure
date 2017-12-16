(ns ^:internal ^:no-doc jmh.exec
  "Build command arguments and run benchmarks."
  (:require [jmh.util :as util]
            [jmh.option :as option]
            [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream File OutputStream PrintStream]
           [java.lang.management ManagementFactory]
           [org.openjdk.jmh.annotations Mode]
           [org.openjdk.jmh.results.format ResultFormatType]
           [org.openjdk.jmh.runner BenchmarkException NoBenchmarksException Runner RunnerException]
           [org.openjdk.jmh.runner.options OptionsBuilder TimeValue]))

(defmulti ^:private build
  "Update the options builder using the given map entry."
  (fn [b entry] (first entry)) :default ::default)

(def ^{:doc "A registry map of aliases to profilers."}
  profiler-aliases (atom {}))

;;;

(defn re-escape
  "Return a pattern that will match the literal string."
  [s]
  (str "\\Q" s "\\E"))

(defn re-class
  "Return a pattern that will match the class, or if the second argument
  is provided, members of the class."
  ([cname] (re-class cname ".+"))
  ([cname members-pattern]
   (str "^" (re-escape cname) "\\." members-pattern)))

(defn forked-with-arguments?
  "Return true if the benchmark will run forked with new JVM arguments."
  [{opts :options}]
  (let [forks (or (some-> opts :fork :count)
                  (some-> opts :fork)
                  0)]
    (and (pos? (long forks))
         (-> opts :jvm :args))))

(defn- tiered-stop-argument? [arg]
  (re-find #"^-XX:TieredStopAtLevel=[123]$" arg))

(defn- check-jvm-arguments
  "Based on code from https://github.com/hugoduncan/criterium."
  [benchmarks]
  (let [compiler (.getName (ManagementFactory/getCompilationMXBean))
        args (.getInputArguments (ManagementFactory/getRuntimeMXBean))
        stop (some tiered-stop-argument? args)
        problem?
        (fn [b]
          (when-not (forked-with-arguments? b)
            (util/warn "At least one benchmark will run"
                       "with problematic JVM argument:" stop)
            true))]
    (and (.contains compiler "Tiered")
         stop (some problem? benchmarks))))

;;;

(defn- get-cause
  "Get at the reason of the benchmark failure."
  [^Throwable t]
  (let [t (.getCause t)]
    (when (instance? BenchmarkException t)
      (when-let [[^Throwable e & more] (.getSuppressed t)]
        (doseq [s more]
          (.addSuppressed e s))
        e))))

(defn include-patterns
  "Returns a include regex pattern strings."
  [benchmarks externs]
  (concat (->> (remove :warmup benchmarks)
               (map (comp re-class :class)))
          (for [x externs
                :when (not (:warmup x))]
            (re-class (:class x x) (:select x ".+")))))

(defn warmup-patterns
  "Returns a warmup include regex pattern strings."
  [benchmarks externs]
  (concat (->> (filter :warmup benchmarks)
               (map (comp re-class :class)))
          (for [x externs
                :when (:warmup x)]
            (re-class (:class x) (:select x ".+")))))

(defn- run*
  "Run the given command-line arguments and return the output."
  [benchmarks opts]
  (let [status (:jmh/status opts)
        log (cond
              (string? status)
              (io/file status)
              (not status)
              (doto (File/createTempFile "jmh" ".txt") .deleteOnExit))
        out (doto (File/createTempFile "jmh" ".xsv") .deleteOnExit)

        builder (OptionsBuilder.)]

    (doseq [entry opts]
      (build builder entry))

    (doseq [re (warmup-patterns benchmarks (:externs opts))]
      (.exclude builder re)
      (.includeWarmup builder re))
    (doseq [re (include-patterns benchmarks (:externs opts))]
      (.include builder re))

    (when log
      (.output builder (str log)))

    (doto builder
      (.resultFormat ResultFormatType/CSV)
      (.result (str out)))

    (try
      (.run (Runner. (.build builder)))
      (slurp out)
      (catch NoBenchmarksException e
        (throw (RunnerException. "no benchmarks defined/selected.")))
      (catch RunnerException e
        (if-let [cause (get-cause e)]
          (throw cause)
          (if (.contains (or (.getMessage e) "") option/ignore-lock)
            (let [msg (str "could not acquire jmh lock file "
                           "(is another benchmark in progress?): "
                           "use :ignore-lock to bypass.")]
              (throw (RunnerException. msg)))
            (throw e))))
      (finally
        (when-not status
          (.delete log))
        (.delete out)))))

(defn line-output-stream
  "Return a stream that will invoke the supplied callback fn for each
  line read."
  ^OutputStream [callback]
  (let [buf (ByteArrayOutputStream. 128)]
    (proxy [OutputStream] []
      (write
        ([x]
         (if (number? x)
           (.write ^OutputStream this (byte-array [x]))
           (.write ^OutputStream this x 0 (alength ^bytes x))))
        ([^bytes arr ^long off ^long len]
         (let [end (+ off len)]
           (loop [off off]
             (if (< off end)
               (let [b (aget arr off)]
                 (if (== b 10)
                   (do (callback (.toString buf))
                       (.reset buf))
                   (.write buf (int b)))
                 (recur (inc off)))))))))))

(defn- long-value
  "Parse the given string and return its primitive long value."
  ^long [^String s]
  (.longValue (Long/valueOf s)))

(defn progress-print-stream
  "Return a PrintStream that will parse output into callback data."
  ^PrintStream [callback]
  (let [progress-re #"^# Run progress: ([\d.]+)% complete, ETA (\d+):(\d+):(\d+)$"
        complete-re #"^# Run complete. Total time: (\d+):(\d+):(\d+)$"

        seconds #(+ (* (long-value %) 60 60)
                    (* (long-value %2) 60)
                    (long-value %3))

        on-line
        (fn [line]
          (if-let [[_ pct hr min sec] (re-find progress-re line)]
            (let [pct (* (.doubleValue (Double/valueOf ^String pct)) 0.01)
                  eta (seconds hr min sec)]
              (when-not (>= pct 1.0)
                (let [event {:eta eta, :percent pct}
                      event (if (== pct 0.0)
                              (assoc event :start true)
                              event)]
                  (callback event))))
            (when-let [[_ hr min sec] (re-find complete-re line)]
              (let [event {:eta 0, :percent 1.0, :complete true
                           :duration (seconds hr min sec)}]
                (callback event)))))]

    (PrintStream. (line-output-stream on-line))))

(defn run
  "Run and update the benchmark environment with the jmh output."
  [{benchmarks :jmh/benchmarks, opts :jmh/options :as env}]
  (let [ignore-orig (System/getProperty option/ignore-lock)

        out-orig System/out
        out (if-let [f (and (not (:status opts))
                            (:progress opts))]
              (progress-print-stream f)
              out-orig)

        status (or (:status opts)
                   (boolean (:progress opts)))
        opts (assoc opts :jmh/status status)]

    (when (:warnings opts true)
      (check-jvm-arguments benchmarks))

    (when (option/debug? opts)
      (util/debug "Running jmh"))

    (try
      (when (not= out out-orig)
        (System/setOut out))
      (when (:ignore-lock opts)
        (System/setProperty option/ignore-lock "true"))
      (assoc env :jmh/output (run* benchmarks opts))
      (finally
        (when (not= out out-orig)
          (.flush ^PrintStream out)
          (System/setOut out-orig))
        (when ignore-orig
          (System/setProperty option/ignore-lock ignore-orig))))))

;;;

(defn- str-array [x]
  (into-array String (map str (if (coll? x) x [x]))))

(defn- time-unit
  "Convert a time unit keyword to a TimeUnit."
  [x]
  (util/check-valid "time-unit" util/time-unit? x))

(defn- time-value
  "Convert a time tuple to TimeValue."
  [[n u]]
  (TimeValue. n (time-unit u)))

(defmethod build :fail-on-error [^OptionsBuilder b [_ v]]
  (.shouldFailOnError b (boolean v)))

(defmethod build :fork [^OptionsBuilder b [_ v]]
  (.forks b (int (:count v (:forks option/defaults))))
  (when-let [x (:warmups v)]
    (.warmupForks b (int x)))
  (when-let [x (get-in v [:jvm :java])]
    (.jvm b (str x)))
  (when-let [x (get-in v [:jvm :args])]
    (.jvmArgs b (str-array x)))
  (when-let [x (get-in v [:jvm :prepend-args])]
    (.jvmArgsPrepend b (str-array x)))
  (when-let [x (get-in v [:jvm :append-args])]
    (.jvmArgsAppend b (str-array x))))

(defmethod build :iterations [^OptionsBuilder b [_ v]]
  (.shouldDoGC b (boolean (:gc v (:gc option/defaults))))
  (.syncIterations b (boolean (:synchronize v (:sync option/defaults)))))

(defmethod build :measurement [^OptionsBuilder b [_ v]]
  (when-let [x (:iterations v)]
    (.measurementIterations b (int x)))
  (when-let [x (:count v)]
    (.measurementBatchSize b (int x)))
  (when-let [x (:time v)]
    (.measurementTime b (time-value x))))

(defmethod build :mode [^OptionsBuilder b [_ v]]
  (doseq [k (util/keyword-seq v)]
    (.mode b (util/check-valid "mode" util/mode? k))))

(defmethod build :ops-per-invocation [^OptionsBuilder b [_ v]]
  (.operationsPerInvocation b (int v)))

(defmethod build :output-time-unit [^OptionsBuilder b [_ v]]
  (.timeUnit b (time-unit v)))

(defmethod build :params [^OptionsBuilder b [_ v]]
  (doseq [[k x] (:jmh/externs v)]
    (.param b (name k) (str-array x))))

(defmethod build :profilers [^OptionsBuilder b [_ v]]
  (util/check (coll? v) "expected seq of profilers")
  (doseq [x v]
    (let [[prof ^String init] (if (coll? x) x [x ""])
          prof (get @profiler-aliases prof prof)
          prof (if (symbol? prof)
                 (Class/forName (name prof))
                 prof)]
      (if (string? prof)
        (.addProfiler b ^String prof init)
        (.addProfiler b ^Class prof init)))))

(defmethod build :thread-groups [^OptionsBuilder b [_ v]]
  (.threadGroups b (int-array (if (number? v) [v] v))))

(defmethod build :threads [^OptionsBuilder b [_ v]]
  (.threads b (int v)))

(defmethod build :timeout [^OptionsBuilder b [_ v]]
  (.timeout b (time-value v)))

(defmethod build :verbose [^OptionsBuilder b [_ v]]
  (.verbosity b (util/check-valid "verbose mode" util/verbose-mode? v)))

(defmethod build :warmup [^OptionsBuilder b [_ v]]
  (when-let [x (:iterations v)]
    (.warmupIterations b (int x)))
  (when-let [x (:count v)]
    (.warmupBatchSize b (int x)))
  (when-let [x (:time v)]
    (.warmupTime b (time-value x))))

(defmethod build :warmups [^OptionsBuilder b [_ v]]
  (when-let [x (:mode v)]
    (.warmupMode b (util/check-valid "warmup mode" util/warmup-mode? x))))

(defmethod build ::default [_ _])
