(ns ^:internal ^:no-doc jmh.env
  "Coordinate and build the benchmark and state type data."
  (:require [jmh.util :as util]
            [jmh.option :as option]
            [jmh.benchmark :as benchmark]
            [jmh.state :as state]))

(defn merge-options
  "Return the fully merged options. The newer `opts` will override the
  default and selector options, in that order."
  [xs option-selectors opts]
  (let [defaults (:jmh/default option-selectors)
        xs (if (map? xs) [xs] xs)
        maps (for [x xs]
               (if (keyword? x)
                 (get option-selectors (keyword (name x)))
                 x))
        maps (->> (concat (cons defaults maps) [opts])
                  (remove nil?)
                  (map option/normalize))
        merge-maps #(if (map? %) (merge % %2) %2)]
    (apply merge-with merge-maps maps)))

(defn filter-by-selectors
  "Given the keyword `sels` and predicate map `selectors`, filter by any
  predicate value of selectors that returns true."
  [selectors sels xs]
  (if (seq sels)
    (let [fns (seq (keep selectors sels))]
      (for [x xs
            :when (some #(% x) fns)]
        x))
    xs))

;;;

(defn used-states
  "Return a set of all states used by benchmarks."
  [benchmarks state-map]
  (let [args (reduce-kv
              (fn [m k x]
                (assoc m (util/ns-keyword "state" k)
                       (->> (state/fixture-seq x)
                            (mapcat :args)
                            (filter util/state-keyword?)
                            set)))
              {} state-map)
        walk (fn walk [cycle k]
               (util/check (not (cycle k)) "state cycle detected")
               (cons k (mapcat walk (repeat (conj cycle k)) (k args))))]
    (->> (mapcat :args benchmarks)
         (filter util/state-keyword?)
         (mapcat (partial walk #{}))
         set)))

(defn primitive-states
  "Return a set of all states that require primitive numeric support."
  [benchmarks]
  (let [prims
        (fn [b]
          (for [[k t] (map vector (:args b) (:desc b))
                :when (#{:long :double} t)]
            (util/check
             (and (util/state-keyword? k) k)
             (format "%s must be a state to support primitive for %s"
                     (pr-str k) (or (:name b) (:fn b))))))]
    (set (mapcat prims benchmarks))))

;;;

(defn finalize-parameters
  "Return the normalized parameter map."
  [env]
  (let [params (merge (:params env)
                      (:params (:jmh/options env)))]
    (reduce-kv
     (fn [m k x]
       (let [k (util/ns-keyword "param" k)
             pname (state/param-name k)
             xs (if (sequential? x) x [x])
             pvals (mapv util/readable-str xs)]
         (assoc m k {:key k, :name pname, :values pvals})))
     {} params)))

(defn finalize-states
  "Return a seq of normalized state maps."
  [{benchmarks :jmh/benchmarks
    states :states,
    resolver :jmh/resolver
    :as env}]
  (let [prim? (primitive-states benchmarks)
        param #(util/check-valid "param" (:jmh/params env) %)

        with-fixture-keys
        (fn [{k :key :as s}]
          (assoc s :prim (prim? k), :jmh/resolver resolver))
        with-state-keys
        (fn [{k :key :as s}]
          (let [cname (resolver k)
                params (map param (:params s))]
            (assoc s :class cname, :jmh/params params)))
        finalize (comp with-state-keys
                       state/normalize
                       with-fixture-keys)

        states (:states env)
        used? (comp (used-states benchmarks states) :key)
        extern? (comp util/class-symbol? second)]

    (->> (remove extern? states)
         (map (partial apply state/normalize))
         (filter used?)
         (map finalize))))

(defn external-classes
  "Return a seq of external class names."
  [env]
  (concat (for [x (vals (:env/states env))
                :when (util/class-symbol? x)]
            x)
          (for [x (-> env :jmh/options :externs)]
            (if (map? x)
              (name (:class x))
              (name x)))))

(defn- class-name
  "Return a package-prefixed class name."
  [pkg prefix ^String s]
  (util/check (neg? (.indexOf s "$"))
              (str "jmh class names cannot contain the '$' character: " s))
  (str pkg "." prefix "_" (munge s)))

(defn state-resolver
  "Returns a map with state-prefixed keywords to states."
  [env]
  (reduce-kv
   (fn [m k x]
     (let [cname (if (util/class-symbol? x)
                   x
                   (class-name (:jmh/pkg env) "state" (name k)))]
       (assoc m (util/ns-keyword "state" k) cname)))
   {} (:states env)))

(defn select-benchmarks
  "Filter based on the benchmark selectors."
  [selectors opts benchmarks]
  (let [selectors (merge {:jmh/default (constantly true)}
                         selectors)
        selectors (zipmap (keys selectors)
                          (for [x (vals selectors)]
                            (if (ifn? x) x (eval x))))

        selects (util/keyword-seq (:select opts :jmh/default))
        includes (util/keyword-seq (get-in opts [:warmups :select]))

        selectors (merge (reduce (fn [m k]
                                   (assoc m k (comp #{k} :name)))
                                 {} (concat selects includes))
                         selectors)

        warmups (when includes
                  (filter-by-selectors selectors includes benchmarks))
        benchmarks (filter-by-selectors selectors selects benchmarks)

        warmups (->> (remove (set benchmarks) warmups)
                     (map #(assoc % :warmup true)))]
    (concat benchmarks warmups)))

(defn finalize-benchmarks
  "Return the final benchmarks for running."
  [{resolver :jmh/resolver :as env}]
  (let [opt-selectors (:jmh/option-selectors env)
        opts (:jmh/options env)

        finalize
        (fn [idx b]
          (let [prefix (str "bench_" idx)
                cname (name (:name b (:fn b)))
                cname (class-name (:jmh/pkg env) prefix cname)
                merged (merge-options (:options b) opt-selectors opts)]
            (assoc b :class cname, :index idx, :options merged
                   :jmh/resolver resolver)))]

    (->> (:benchmarks env)
         (select-benchmarks (:selectors env) opts)
         (map-indexed finalize))))

(defn normalize
  "Flatten benchmarks and expand benchmark parameters to states."
  [env]
  (let [benchmarks (->> (:benchmarks env)
                        (mapcat benchmark/normalized-seq))

        pkeys #(filter util/param-keyword? (:args %))
        params (-> (mapcat pkeys benchmarks) set seq)
        gens (for [k params]
               (->> k name gensym name (keyword "state")))

        state-val #(hash-map :fn 'identity, :args [%])
        pstates (zipmap (map (comp keyword name) gens)
                        (map state-val params))

        substitute (partial replace (zipmap params gens))
        benchmarks (for [b benchmarks]
                     (update b :args substitute))
        states (merge (:states env) pstates)]

    (assoc env :benchmarks benchmarks, :states states)))

(defn setup
  "Return the updated environment, suitable for generation."
  [env opts]
  (let [opts (option/normalize opts)
        opts (if (:instrument opts)
               (option/without-forking opts)
               opts)
        opts (if (:ignore-lock opts)
               (option/without-locking opts)
               opts)

        pkg (str "jmh" (System/currentTimeMillis))
        path (or (:compile-path opts) *compile-path* "classes")
        env (assoc env :jmh/pkg pkg, :jmh/path path
                   :jmh/options opts
                   :jmh/option-selectors (:options env))

        env (as-> (normalize env) env
              (assoc env :jmh/params (finalize-parameters env))
              (assoc env :jmh/resolver (state-resolver env))
              (assoc env :jmh/benchmarks (finalize-benchmarks env))
              (assoc env :jmh/externs (external-classes env))
              (assoc env :jmh/states (finalize-states env)))

        types (concat (map benchmark/class-type (:jmh/benchmarks env))
                      (map state/class-type (:jmh/states env)))]

    (when (option/debug? opts)
      (util/debug "Generating type data"))

    (assoc env :jmh/types (vec types))))
