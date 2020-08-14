(ns ^:internal ^:no-doc jmh.instrument
  "Generate instrumented vars for testing and debugging."
  (:require [jmh.util :as util]
            [jmh.emit :as emit]
            [insn.core :as insn])
  (:import [clojure.lang Compiler$FnMethod Fn IFn ISeq Namespace RestFn RT Symbol Var]))

(defprotocol Intern
  (intern-fn [x metric]
   "Given a value that can be coerced to a fn and an arbitrary value
   object, return a var in the instrumentation namespace. When invoked,
   the var will delegate to the derived fn, recording information under
   `metric` about the passed arguments and return value (or error)."))

(def ^{:dynamic true
       :doc "A non-nil value indicates instrumentation is enabled."}
  *env* nil)

;;;

(defn env
  "Return an instrumentation environment suitable for binding to `*env*`."
  []
  (let [ns (Namespace/findOrCreate (gensym "inst"))]
    {::cache (atom {})
     ::log (atom {})
     ::ns ns
     ::thread (Thread/currentThread)}))

(defn load-fn
  "Instructions to load the fn symbol or form value. If instrumentation
  is enabled then instrument the fn as `metric`."
  [x metric]
  (cond
    *env*
    (if (= (Thread/currentThread) (::thread *env*))
      (or (get (::cache *env*) metric)
          (let [v (intern-fn x metric)
                ops (emit/load-resolve (util/var-symbol v))]
            (update *env* ::cache swap! assoc metric ops)
            ops))
      (throw (IllegalStateException.
              "invalid instrumentation environment")))
    (symbol? x)
    (emit/load-resolve x)
    :else
    (emit/load-eval x)))

(defn log
  "Return the data collected from the environment during all calls of
  the instrumented fns."
  [inst]
  (some-> inst ::log deref))

(defmacro with-instrumentation
  "Evaluate the body expressions with instrumentation enabled. The first
  form must evaluate to a value of that as returned by `env`."
  [inst & body]
  `(let [inst# ~inst]
     (util/check (::ns inst#)
                 (str "invalid instrumentation environment: " (pr-str inst#)))
     (binding [*env* inst#]
       ~@body)))

;;;

(def field-name
  {:fn "_fn"
   :map "_map"})

(def class-fields
  [{:flags [:private :final], :name (:fn field-name), :type IFn}
   {:flags [:private :final], :name (:map field-name), :type Object}])

(def ^{:doc "The proxy constructor."}
  init-method
  {:name :init, :desc [IFn Object :void]
   :emit [[:aload 0]
          [:invokespecial :super :init [:void]]
          (map-indexed
           (fn [^long i f]
             [[:aload 0]
              [:aload (inc i)]
              [:putfield :this (:name f) (:type f)]])
           class-fields)
          [:return]]})

(def ^{:doc "The proxy RestFn invoke method data."}
  do-invoke-method
  {:name "doInvoke", :desc [Object Object]
   :emit [(emit/load-resolve `apply)
          [:aload 0]
          [:getfield :this (:fn field-name) IFn]
          [:ldc nil]
          [:aload 1]
          [:invokeinterface IFn "invoke" (repeat 4 Object)]
          [:areturn]]})

(def ^{:doc "The proxy RestFn arity method data."}
  arity-method
  {:name "getRequiredArity", :desc [:int]
   :emit [[:ldc 0]
          [:ireturn]]})

;;;

(defn primitive-interface
  "Return the name of the Clojure primitive interface for the given
  arglist, or nil if not primitive."
  ^String [arglist]
  (Compiler$FnMethod/primInterface arglist))

(defn primitive-desc
  "Return the method descriptor data for the given primitive arglist."
  [arglist]
  (let [iface (util/check
               (primitive-interface arglist)
               (str "given arglist is not primitive: " (pr-str arglist)))]
    (->> (.split ^String iface "\\$") second
         (map {\L :long, \D :double, \O Object}))))

(defn primitive-body
  "Returns the primitive method instruction op sequence. Takes an
  numeric index that keys into to the map instance field to retrieve the
  arglist for reporting."
  [index arglist]
  (let [desc (primitive-desc arglist)

        prelude [[:aload 0]
                 [:getfield :this (:fn field-name) IFn]
                 [:aload 0]
                 [:getfield :this (:map field-name) Object]
                 [:ldc index]
                 [:i2l]
                 [:invokestatic Long "valueOf" [:long Long]]
                 [:invokestatic RT "get" [Object Object Object]]]

        load-locals (fn [[loads i] t]
                      (let [inext (+ (long i) (long (if (#{:long :double} t) 2 1)))
                            load (condp = t
                                   :long [[:lload i]
                                          [:invokestatic Long "valueOf" [:long Long]]]
                                   :double [[:dload i]
                                            [:invokestatic Double "valueOf" [:double Double]]]
                                   [:aload i])]
                        [(conj loads load) inext]))

        return (condp = (last desc)
                 :long [[:invokestatic RT "longCast" [Object :long]]
                        [:lreturn]]
                 :double [[:invokestatic RT "doubleCast" [Object :double]]
                          [:dreturn]]
                 [:areturn])]
    [prelude
     (first (reduce load-locals [[], 1] (butlast desc)))
     [:invokeinterface IFn "invoke" (repeat (inc (count desc)) Object)]
     return]))

(defn- log-fn
  "Return the delegation fn."
  [f metric]
  (let [log (::log *env*)
        vconj (fnil conj [])]
    (fn [arglist & args]
      (let [call (util/some-assoc {} :arglist arglist, :args (seq args))]
        (try
          (let [ret (apply f args)]
            (swap! log update metric vconj (assoc call :ret ret))
            ret)
          (catch Throwable e
            (swap! log update metric vconj (assoc call :thrown e))
            (throw e)))))))

(defn- proxy-fn
  "Return a proxy that will record and delegate all invocations to the
  given fn. Generates a primitive method for each primitive arglist and
  one catch-all variadic method for non-primitive calls."
  [f metric arglists]
  (let [ifaces (keep primitive-interface arglists)
        prim-arglists (filter primitive-interface arglists)

        prims (map-indexed
               (fn [i arglist]
                 {:name "invokePrim"
                  :desc (primitive-desc arglist)
                  :emit (primitive-body i arglist)})
               prim-arglists)
        methods (concat [init-method
                         do-invoke-method
                         arity-method]
                        prims)

        data {:name (gensym "inst-proxy")
              :super RestFn
              :interfaces ifaces
              :fields class-fields
              :methods methods}

        log (log-fn f metric)
        margs (zipmap (range) prim-arglists)]

    (insn/new-instance data log margs)))

;;;

(extend-protocol Intern
  Fn
  (intern-fn [f x]
    (let [f (proxy-fn f x nil)]
      (intern (::ns *env*) (gensym 'eval-fn) f)))
  ISeq
  (intern-fn [form x]
    (intern-fn (util/eval-fn form) x))
  Symbol
  (intern-fn [s x]
    (let [v (util/check (or (resolve s) (util/require-fn s))
                        (str "symbol did not resolve: " s))]
      (intern-fn v x)))
  Var
  (intern-fn [v x]
    (let [prefix (-> v meta :name name)
          f (proxy-fn @v x (-> v meta :arglists))]
      (intern (::ns *env*) (gensym prefix) f))))
