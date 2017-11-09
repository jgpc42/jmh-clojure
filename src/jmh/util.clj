(ns ^:no-doc jmh.util
  "JMH and general utilities."
  (:require [clojure.set :as set])
  (:import [io.github.jgpc42.jmh DelegateClassLoader]
           [java.util.concurrent TimeUnit]
           [org.openjdk.jmh.annotations Level Mode Scope]
           [org.openjdk.jmh.infra BenchmarkParams Blackhole Control IterationParams ThreadParams]
           [org.openjdk.jmh.runner.options VerboseMode WarmupMode]))

(defn- dotted?
  "Returns true if the name of the given value contains dot(s)."
  [x]
  (.contains (name x) "."))

(defn bare-keyword?
  "Returns true if the value is a keyword with no namespace."
  [x]
  (and (keyword? x)
       (nil? (namespace x))))

(defn bare-symbol?
  "Returns true if the value is a symbol with no namespace."
  [x]
  (and (symbol? x)
       (nil? (namespace x))))

(defn class-symbol?
  "Returns true if the value is a symbol that denotes a
  packaged-prefixed class."
  [x]
  (and (bare-symbol? x)
       (dotted? x)))

(defn intern-keyword?
  "Returns true if the value is a keyword with no namespace or dots."
  [x]
  (and (bare-keyword? x)
       (not (dotted? x))))

(defn intern-symbol?
  "Returns true if the value is a symbol with no namespace or dots."
  [x]
  (and (bare-symbol? x)
       (not (dotted? x))))

(defn prefixed-keyword?
  "Returns true if `x` is a keyword whose name does not contain dots and
  has a namespace of `prefix`."
  [prefix x]
  (and (keyword? x)
       (= prefix (namespace x))
       (not (dotted? x))))

(defn var-symbol?
  "Returns true if the value is a symbol whose name does not contain
  dots. The namespace may contain dots."
  [x]
  (and (symbol? x)
       (not (dotted? x))))

(def option-keyword? (partial prefixed-keyword? "option"))
(def param-keyword? (partial prefixed-keyword? "param"))
(def state-keyword? (partial prefixed-keyword? "state"))

;;;

(defmacro check
  "If not `expr`, throw error with message `msg`, else return it."
  [expr msg]
  `(or ~expr (throw (RuntimeException. ~msg))))

(defmacro check-valid
  "Get the value at `k` in map `m` or throw exception."
  [msg m k]
  `(let [m# ~m, k# ~k]
     (or (get m# k#)
         (throw (ex-info (str "invalid " ~msg ": " (pr-str k#))
                         {:key k# :map m#})))))

(defn debug
  "Print the messages and a newline, prefixed by 'DEBUG: '."
  [& msg]
  (apply println "DEBUG:" msg))

(defn ^:internal ^ClassLoader delegate-classloader
  "Returns a ClassLoader that will invoke the passed fn when a needed
  class is not found. The fn is provided a packaged-prefixed class name
  and should return a bytecode byte array for the given class or nil if
  not available."
  [f]
  (DelegateClassLoader. f))

(defn eval-fn
  "Evaluate the given form. If not a fn, or the evaluation fails, throw
  an exception."
  [form]
  (let [f (try
            (eval form)
            (catch Exception e
              (throw (ex-info "error while evaluating fn expression form"
                              {:form form} e))))]
    (check (ifn? f) (str "form did not evaluate to a fn: " (pr-str form)))
    f))

(defn keyword-seq
  "Coerce value to a sequence of keywords."
  [x]
  (seq (if (keyword? x) [x] x)))

(defn matching-arity
  "Returns the first arglist that can be applied with the given arity
  arguments, or nil if not found."
  [arglists ^long arity]
  (let [match? (fn [alist]
                 (if (some #{'&} alist)
                   (>= arity (count (take-while #(not= '& %) alist)))
                   (== arity (count alist))))]
    (->> arglists (filter match?) first)))

(defn ns-keyword
  "Return a normalized namespaced keyword."
  [default-ns-str k]
  (if (intern-keyword? k)
    (keyword default-ns-str (name k))
    k))

(defn readable-str
  "Return the value as a string readable by `read-string`."
  [x]
  (binding [*print-dup* false]
    (with-out-str (pr x))))

(defn require-fn
  "Resolve and return the given var denoted by the symbol, ensuring
  its namespace is loaded if not already. If the var does not exist,
  or its value is not a fn, an error is thrown."
  [svar]
  (check (and (namespace svar)
              (try
                (or (resolve svar)
                    (require (symbol (namespace svar))))
                (let [v (resolve svar)]
                  (and v, (-> v meta :macro nil?), (ifn? @v), v))
                (catch java.io.FileNotFoundException _)))
         (str "value did not resolve to a fn: " (pr-str svar))))

(defn some-assoc
  "Assoc each pair of key and value if the value is not nil."
  [m & {:as xs}]
  (reduce-kv (fn [m k v]
               (if (nil? v) m (assoc m k v)))
             m xs))

(defn var-symbol
  "Return the var's qualified name as a symbol."
  [v]
  (let [m (meta v)]
    (symbol (name (ns-name (:ns m)))
            (name (:name m)))))

(defn warn
  "Print the messages and a newline to stderr, prefixed by 'WARNING: '."
  [& msg]
  (binding [*out* *err*]
    (apply println "WARNING:" msg)))

;;;

(def jmh-keyword?
  {:jmh/benchmark-params BenchmarkParams
   :jmh/blackhole Blackhole
   :jmh/control Control
   :jmh/iteration-params IterationParams
   :jmh/thread-params ThreadParams})

(def level?
  {:trial Level/Trial
   :iteration Level/Iteration
   :invocation Level/Invocation})

(def mode?
  {:all Mode/All
   :average Mode/AverageTime
   :sample Mode/SampleTime
   :single-shot Mode/SingleShotTime
   :throughput Mode/Throughput})

(def mode-name?
  (set/map-invert mode?))

(def scope?
  {:benchmark Scope/Benchmark
   :group Scope/Group
   :thread Scope/Thread})

(def time-unit?
  {:ns TimeUnit/NANOSECONDS, :nanoseconds TimeUnit/NANOSECONDS
   :us TimeUnit/MICROSECONDS, :microseconds TimeUnit/MICROSECONDS
   :ms TimeUnit/MILLISECONDS, :milliseconds TimeUnit/MILLISECONDS
   :s TimeUnit/SECONDS, :sec TimeUnit/SECONDS, :seconds TimeUnit/SECONDS
   :m TimeUnit/MINUTES, :min TimeUnit/MINUTES, :minutes TimeUnit/MINUTES
   :hr TimeUnit/HOURS, :hours TimeUnit/HOURS})

(def verbose-mode?
  {:silent VerboseMode/SILENT, false VerboseMode/SILENT
   :normal VerboseMode/NORMAL
   :extra VerboseMode/EXTRA, true VerboseMode/EXTRA})

(def warmup-mode?
  {:indi WarmupMode/INDI
   :bulk WarmupMode/BULK
   :bulk-indi WarmupMode/BULK_INDI})
