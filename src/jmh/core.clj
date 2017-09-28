(ns jmh.core
  "Clojure benchmarking with JMH."
  (:require [jmh.env :as env]
            [jmh.exec :as exec]
            [jmh.generate :as gen]
            [jmh.option :as option]
            [jmh.result :as result]
            [jmh.util :as util]
            [jmh.instrument :as instrument])
  (:import [clojure.lang Namespace]
           [java.io ByteArrayOutputStream PrintStream]
           [org.openjdk.jmh.profile ProfilerFactory]))

(defn run
  "Run the :benchmarks given in env. Returns the benchmark results as a
  sequence of maps. Options include:

    :compile-path   directory which to write .class files to. Must be on
                    the classpath. The default is to use *compile-path*
                    and should not generally need to be changed if using
                    a tool such as Leiningen or similar.

    :fail-on-error  halt execution on any benchmark exception.

    :ignore-lock    do not fail if the jmh lock file cannot be obtained.

    :instrument     debugging switch. If true, capture call information
                    for each state and benchmark fn during the run. Note
                    that this option disables jmh forking. The data is
                    provided as :jmh/log metadata on the returned seq.

    :iterations     map. If key :gc is true, a garbage collection is
                    forced between each iteration. If key :synchronize
                    is false, worker threads don't wait until each is
                    ready to run an iteration before proceeding.

    :params         map. Parameters to add or override from env.

    :profilers      a string or seq of strings. Profilers to enable
                    while benchmarking. See `profilers`.

    :progress       callback fn that will receive periodic progress
                    events. Overridden by :status.

    :select         keyword or seq of selector keywords for filtering
                    benchmarks to run.

    :status         by default, jmh output is suppressed. If true,
                    display the log output while running. If a string,
                    write it to the given file. Overrides :progress.

    :type           keyword. An alias for a common option set. For
                    example, :quick. See the `jmh.option` namespace.

    :verbose        if true or :extra, generate additional :status
                    output during the run. If :silent or false, don't
                    collect any. Provide :normal for the default.

    :warmups        map. The :mode key is one of :indi, :bulk, or
                    :bulk-indi. See the jmh WarmupMode javadoc for more.
                    The :select key may also be given to run additional
                    benchmark fns only during warmup.

    :warnings       if false, do not check for JVM configuration issues.

  Along with the above, benchmark options can be specified as well to
  add to or override those given in the env.

  Additional advanced options are described below.

  External benchmark classes may be run along side Clojure ones. The
  :externs key gives a sequence of class names to process. Each item may
  be a class name, or a map, the latter gives a :class name along with
  an optional :select method regex and a boolean :warmup flag.

  Raw argument strings may be passed directly to jmh via the :arguments
  key as a sequence of strings. This should generally only be used for
  specifying parameters for external benchmarks."
  ([env] (run env {}))
  ([env opts]
   (let [go (comp result/parse exec/run gen/write env/setup)]
     (if (:instrument opts)
       (let [inst (instrument/env)]
         (instrument/with-instrumentation inst
           (vary-meta (go env opts)
                      assoc :jmh/log (instrument/log inst))))
       (go env opts)))))

(defn run-fn
  "Benchmark a thunk (a fn of zero arguments). See `run` for details on
  the available options. Returns a single result map.

  Note that benchmarking this way precludes forking."
  ([f] (run-fn f {}))
  ([f opts]
   (let [ns (Namespace/findOrCreate (gensym "jmh.run"))
         v (intern ns 'fn f)
         env {:benchmarks [{:fn (util/var-symbol v)}]}]
     (first (run env (option/without-forking opts))))))

(defmacro run-expr
  "Benchmark an expression. Equivalent to: (run-fn (fn [] expr) opts)"
  ([expr] `(run-expr ~expr {}))
  ([expr opts]
   `(run-fn (fn [] ~expr) ~opts)))

(defn profilers
  "Return a set of profiler maps. If :supported, the profiler :name is
  available for use with `run`."
  []
  (let [bas (ByteArrayOutputStream. 4096)
        _ (doto (PrintStream. bas)
            ProfilerFactory/listProfilers)
        out (.toString bas "UTF-8")
        [ok no] (.split out "(?m)^Unsupported profilers:$")
        make (fn [s ok?]
               (for [[_ k desc] (re-seq #"(?m)^\s+([^\s]+):\s+(.+?)\s*$" s)]
                 {:name k, :desc desc, :supported ok?}))]
    (set (concat (make ok true) (make no false)))))
