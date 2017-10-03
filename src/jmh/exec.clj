(ns ^:internal ^:no-doc jmh.exec
  "Build command arguments and run benchmarks."
  (:require [jmh.util :as util]
            [jmh.option :as option]
            [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream File OutputStream PrintStream]
           [java.lang.management ManagementFactory]
           [org.openjdk.jmh.annotations Mode]
           [org.openjdk.jmh.runner BenchmarkException Runner RunnerException]
           [org.openjdk.jmh.runner.options CommandLineOptions TimeValue]))

(defmulti ^:private arg-seq
  "Return a jmh command-line argument seq from the given map entry."
  first :default ::default)

;;;

(defn re-escape
  "Return a pattern that will match the literal string."
  [s]
  (str "\\Q" s "\\E"))

(defn re-alt
  "Return a pattern that will try each alternative pattern."
  [patterns]
  (str "(?:" (apply str (interpose \| patterns)) ")"))

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

(defn- run*
  "Run the given command-line arguments and return the output."
  [args status]
  (let [log (cond
              (string? status)
              (io/file status)
              (not status)
              (doto (File/createTempFile "jmh" ".txt") .deleteOnExit))
        out (doto (File/createTempFile "jmh" ".xsv") .deleteOnExit)

        args (if log (concat args ["-o" (str log)]) args)
        args (concat args ["-rf" "CSV", "-rff" (str out)])

        cmd (CommandLineOptions. (into-array String args))]

    (try
      (.run (Runner. cmd))
      (slurp out)
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

(defn benchmark-arguments
  "Return the benchmark include, exclude, and warmup arguments."
  [benchmarks externs]
  (let [warmups (concat (->> (filter :warmup benchmarks)
                             (map (comp re-class :class)))
                        (for [x externs
                              :when (:warmup x)]
                          (re-class (:class x) (:select x ".+"))))
        args (when (seq warmups)
               (let [re (re-alt warmups)]
                 ["-e" re, "-wmb" re]))

        includes (concat (->> (remove :warmup benchmarks)
                              (map (comp re-class :class)))
                         (for [x externs
                               :when (not (:warmup x))]
                           (re-class (:class x x) (:select x ".+"))))
        include (re-alt includes)]
    (concat args [include])))

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
  (let [args (concat (mapcat arg-seq opts)
                     (benchmark-arguments benchmarks (:externs opts)))
        ignore-orig (System/getProperty option/ignore-lock)

        out-orig System/out
        out (if-let [f (and (not (:status opts))
                            (:progress opts))]
              (progress-print-stream f)
              out-orig)

        status (or (:status opts)
                   (boolean (:progress opts)))]

    (when (:warnings opts true)
      (check-jvm-arguments benchmarks))

    (when (option/debug? opts)
      (util/debug "Running jmh"))

    (try
      (when (not= out out-orig)
        (System/setOut out))
      (when (:ignore-lock opts)
        (System/setProperty option/ignore-lock "true"))
      (assoc env :jmh/output (run* args status))
      (finally
        (when (not= out out-orig)
          (.flush ^PrintStream out)
          (System/setOut out-orig))
        (when ignore-orig
          (System/setProperty option/ignore-lock ignore-orig))))))

;;;

(defn- cat-arg
  "If the map contains a value at `path`, concat `arg` and it to `args`.
  Otherwise return the args unmodified."
  ([args m path arg]
   (cat-arg args m path arg str))
  ([args m path arg to-str]
   (let [path (if (keyword? path) [path] path)]
     (if-let [x (get-in m path)]
       (if (sequential? x)
         (concat args (cons arg (map to-str x)))
         (concat args [arg (to-str x)]))
       args))))

(defn- time-unit
  "Convert a time unit to a string."
  [x]
  (TimeValue/tuToString
   (util/check-valid "time-unit" util/time-unit? x)))

(defn- time-str
  "Convert a time tuple to an option string."
  [[n u]]
  (str n (time-unit u)))

(defmethod arg-seq :arguments [[_ v]]
  v)

(defmethod arg-seq :fail-on-error [[_ v]]
  ["-foe" (str (boolean v))])

(defmethod arg-seq :fork [[_ v]]
  (-> ["-f" (str (:count v))]
      (cat-arg v :warmups "-wf")
      (cat-arg v :java "-jvm")
      (cat-arg v [:jvm :args] "-jvmArgs")
      (cat-arg v [:jvm :prepend-args] "-jvmArgsPrepend")
      (cat-arg v [:jvm :append-args] "-jvmArgsAppend")))

(defmethod arg-seq :iterations [[_ v]]
  ["-gc" (str (boolean (:gc v)))
   "-si" (str (boolean (:synchronize v)))])

(defmethod arg-seq :mode [[_ v]]
  (let [mode (fn [x]
               (let [mode (util/check-valid "mode" util/mode? x)]
                 (.shortLabel ^Mode mode)))
        xs (if (keyword? v) [v] v)]
    ["-bm" (apply str (interpose \, (map mode xs)))]))

(defmethod arg-seq :ops-per-invocation [[_ v]]
  ["-opi" (str v)])

(defmethod arg-seq :output-time-unit [[_ v]]
  ["-tu" (time-unit v)])

(defmethod arg-seq :profilers [[_ v]]
  (mapcat (partial vector "-prof")
          (if (string? v) [v] v)))

(defmethod arg-seq :thread-groups [[_ v]]
  ["-tg" (apply str (interpose \, v))])

(defmethod arg-seq :threads [[_ v]]
  ["-t" (str v)])

(defmethod arg-seq :timeout [[_ v]]
  ["-to" (time-str v)])

(defmethod arg-seq :verbose [[_ v]]
  ["-v" (str (util/check-valid "verbose mode" util/verbose-mode? v))])

(defmethod arg-seq :warmups [[_ v]]
  (when-let [m (:mode v)]
    ["-wm" (str (util/check-valid "warmup mode" util/warmup-mode? m))]))

(let [method
      (fn [[i-arg bs-arg t-arg] [_ v]]
        (-> (cat-arg [] v :iterations i-arg)
            (cat-arg v :count bs-arg)
            (cat-arg v :time t-arg time-str)))]
  (doto ^clojure.lang.MultiFn arg-seq
    (.addMethod :measurement (partial method ["-i" "-bs" "-r"]))
    (.addMethod :warmup (partial method ["-wi" "-wbs" "-w"]))))

(defmethod arg-seq ::default [_])
