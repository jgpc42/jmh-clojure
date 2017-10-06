(ns ^:internal ^:no-doc jmh.benchmark
  "Fns for assembling jmh benchmark proxy classes."
  (:refer-clojure :exclude [flatten])
  (:require [jmh.util :as util]
            [jmh.instrument :as instrument]
            [jmh.emit :as emit]
            [jmh.state :as state])
  (:import [clojure.lang Compiler$FnMethod IFn]
           [org.openjdk.jmh.annotations
            Benchmark BenchmarkMode Fork Group GroupThreads Measurement
            OutputTimeUnit OperationsPerInvocation Timeout Threads Warmup]))

(def apply-field
  {:flags [:private :static :final]
   :name "_apply", :type IFn})

(def fn-field-name "_fn")

(defmulti ^:private ann-tuple
  "Return an annotation tuple from the given map entry."
  first :default ::default)

;;;

(defn fn-field-info
  "Return type data describing the fn field."
  [{f :fn :as b}]
  (let [fvar (when (symbol? f)
               (util/require-fn f))
        nargs (count (:args b))

        alists (some-> fvar meta :arglists sort seq)
        sig (util/matching-arity alists nargs)

        _ (util/check (or (nil? alists)
                          sig
                          (and (:apply b) (seq (:args b))))
                      (format "%s does not support arity %d" f nargs))

        vararg? (some #{'&} sig)
        objs? (or (not sig) (:apply b) vararg?)

        sig (if objs?
              (vec (repeat nargs 'x))
              sig)

        tag #(-> % meta :tag ('{long :long, double :double} Object))
        ret (tag sig)
        desc (conj (mapv tag sig) ret)

        iface (and (not objs?)
                   (some-> (Compiler$FnMethod/primInterface sig)
                           symbol resolve))
        ftype (or iface IFn)]

    (util/check (<= nargs 20) "max benchmark arity allowed is 20")
    {:desc desc, :return ret, :type ftype}))

(defn flatten
  "Flatten :fn vector forms. Returns a seq of maps."
  [b]
  (if (vector? (:fn b))
    (let [ns (when-let [ns (:ns b)]
               (if (symbol? ns) (name ns) (ns-name ns)))
          bare (fn [s]
                 (util/check
                  (util/bare-symbol? s)
                  (str ":fn vector must all be simple symbols when using :ns"))
                 (name s))]
      (for [g (:fn b)
            :let [g (if ns (symbol ns (bare g)) g)]]
        (assoc b :fn g)))
    [b]))

(defn normalize
  "Ensure consistent benchmark format."
  [x]
  (let [b (if (map? x) x {:fn x})
        f (util/check-valid "benchmark" b :fn)

        b (if (seq? f)
            (do (util/eval-fn f)
                (assoc b :name (:name b :expr)))
            b)

        arg-keyword (partial util/ns-keyword "state")
        args (map arg-keyword (:args b))
        b (util/some-assoc b :args (seq args))

        f (cond
            (var? f) (util/var-symbol f)
            (util/bare-symbol? f) (symbol (name f) "-main")
            :else f)]

    (assoc b :fn f)))

(defn normalized-seq
  "Normalize and adorn with type info. Returns a seq of maps."
  [x]
  (let [with-info #(merge % (fn-field-info %))]
    (->> x normalize flatten (map with-info))))

;;;

(defn class-fields
  "Return a seq of static field data."
  [{ftype :type :as b}]
  (let [fields [{:flags [:private :static :final]
                  :name fn-field-name, :type ftype}]]
    (if (:apply b)
      (conj fields apply-field)
      fields)))

(defn method-emits
  "Return a map of bytecode snippets for later assembly."
  [{apply? :apply, f :fn, ftype :type, :keys [desc return] :as b}]
  (let [desc (if apply? (cons Object desc) desc)

        invoke (if (= IFn ftype)
                 [:invokeinterface IFn "invoke" desc]
                 [:invokeinterface ftype "invokePrim" desc])

        return (if (:void b)
                 [[:pop] [:return]]
                 (condp = return
                   :long [:lreturn]
                   :double [:dreturn]
                   [:areturn]))

        prelude [(when apply?
                   [:getstatic :this (:name apply-field) IFn])
                 [:getstatic :this fn-field-name ftype]]]

    {:invoke invoke, :prelude prelude, :return return}))

;;;

(defn method-desc
  "Returns the benchmark method descriptor."
  [{ret :return, resolver :jmh/resolver :as b}]
  (let [desc (for [k (:args b)]
               (if (util/state-keyword? k)
                 (util/check-valid "state" resolver k)
                 (util/check-valid "jmh keyword" util/jmh-keyword? k)))
        ret (if (:void b) :void ret)]
    (conj (vec desc) ret)))

(defn method-body
  "Returns the instruction op sequence for the benchmark method."
  [{fdesc :desc, resolver :jmh/resolver :as b}]
  (let [clear-locals? (-> b :options :clear-locals)
        emits (method-emits b)

        load-local (fn [i k t]
                     [[:aload i]
                      (when (util/state-keyword? k)
                        (let [x (util/check-valid "state" resolver k)]
                          (when-not (util/class-symbol? x)
                            [:getfield x (state/value-field-name t) t])))
                      (when clear-locals?
                        [[:ldc nil] [:astore i]])])]
    [(:prelude emits)
     (mapcat load-local (next (range)) (:args b) fdesc)
     (:invoke emits)
     (:return emits)]))

(defn fn-load
  "Return an op sequence that will load and set the static :fn field."
  [{f :fn, ftype :type :as b}]
  (let [metric (keyword (str "jmh.benchmark." (:index b))
                        (if (seq? f)
                          "expr"
                          (str (namespace f) "." (name (:name b f)))))]
    [(when (:apply b)
       [(emit/load-resolve `apply)
        [:putstatic :this (:name apply-field) IFn]])
     (instrument/load-fn f metric)
     (when (not= IFn ftype)
       [:checkcast ftype])
     [:putstatic :this fn-field-name ftype]]))

(defn class-type
  "Yield the proxy class data of the given benchmark."
  [b]
  (let [anns (cons [Benchmark true]
                   (keep ann-tuple (:options b)))

        clinit {:name :clinit
                :emit [(fn-load b)
                       [:return]]}

        desc (method-desc b)

        run {:flags [:public :final], :name "run"
             :desc desc, :annotations anns
             :emit (method-body b)}]
    {:flags [:public]
     :name (:class b)
     :fields (class-fields b)
     :methods [clinit run]}))

;;;

(defn- time-unit [x]
  (util/check-valid "time unit" util/time-unit? x))

(defmethod ann-tuple :fork [[_ v]]
  [Fork (util/some-assoc
         {:value (int (:count v 1))}
         :warmups (some-> v :warmups int)
         :jvm (:java v)
         :jvmArgs (get-in v [:jvm :args])
         :jvmArgsPrepend (get-in v [:jvm :prepend-args])
         :jvmArgsAppend (get-in v [:jvm :append-args]))])

(defmethod ann-tuple :group [[_ v]]
  [Group v])

(defmethod ann-tuple :group-threads [[_ v]]
  [GroupThreads (int v)])

(defmethod ann-tuple :mode [[_ v]]
  (let [mode #(util/check-valid "mode" util/mode? %)]
    [BenchmarkMode (if (keyword? v) [(mode v)] (mapv mode v))]))

(defmethod ann-tuple :ops-per-invocation [[_ v]]
  [OperationsPerInvocation (int v)])

(defmethod ann-tuple :output-time-unit [[_ v]]
  [OutputTimeUnit (time-unit v)])

(defmethod ann-tuple :timeout [[_ v]]
  [Timeout {:time (int (first v))
            :timeUnit (time-unit (second v))}])

(defmethod ann-tuple :threads [[_ v]]
  [Threads (int v)])

(let [method
      (fn [atype [_ v]]
        [atype (util/some-assoc
                {} :batchSize (some-> v :count int)
                :iterations (some-> v :iterations int)
                :time (some-> v :time first int)
                :timeUnit (some-> v :time second time-unit))])]
  (doto ^clojure.lang.MultiFn ann-tuple
    (.addMethod :measurement (partial method Measurement))
    (.addMethod :warmup (partial method Warmup))))

(defmethod ann-tuple ::default [_])
