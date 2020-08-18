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

    :externs        advanced option. See below.

    :fail-on-error  halt execution on any benchmark exception. Defaults
                    to true. Note that if this is false, benchmarks that
                    fail will be missing from the returned sequence.

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
                    Parameters for externs (see below) are provided via
                    a special :jmh/externs key which gives a nested map.

    :profilers      seq. Profilers to enable while benchmarking. See
                    `profilers`. External profilers may be specified by
                    giving the class as package-prefixed symbol, Class
                    object, or tuple of [class initialization-string].

    :progress       callback fn that will receive periodic progress
                    events. Overridden by :status.

    :select         keyword or seq of selector keywords for filtering
                    benchmarks to run.

    :status         by default, jmh output is suppressed. If true,
                    display the log output while running. If a string,
                    write it to the given file. Overrides :progress.

    :type           keyword or keyword seq. Aliases for common option
                    sets. For example, :quick. Option groups described
                    in env are automatically merged for convenience. See
                    the `jmh.option` namespace for more.

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
  an optional :select method regex and a boolean :warmup flag. Note that
  the :select pattern is anchored to the beginning of the method name.
  For example, to match methods ending with 'foo' use #\".*foo$\"."
  ([env] (run env {}))
  ([env opts]
   (let [go (comp result/transform exec/run gen/write env/setup)]
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
  "Benchmark an expression. Equivalent to: (run-fn (fn [] expr) opts)."
  ([expr] `(run-expr ~expr {}))
  ([expr opts]
   `(run-fn (fn [] ~expr) ~opts)))

;;;

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

(defn register-profiler-alias!
  "Accepts a key `k` and a value `x` suitable for the `run` :profilers
  option. Registers the given profiler value under the alias k. This
  name may subsequently be used in place of x. Returns nil."
  [k x]
  (swap! exec/profiler-aliases assoc k x)
  nil)
