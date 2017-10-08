(ns ^:internal ^:no-doc jmh.state
  "Fns for assembling jmh state proxy classes."
  (:require [jmh.util :as util]
            [jmh.instrument :as instrument])
  (:import [io.github.jgpc42.jmh Util]
           [clojure.lang IFn RT]
           [org.openjdk.jmh.annotations Param Setup State TearDown]))

(def ^{:private true
       :doc "The fixture levels, in the order invoked."}
  level-keys [:trial :iteration :invocation])

(def ^{:private true
       :doc "The fixture methods, in the order invoked."}
  phase-keys [:setup :teardown])

(def ^{:private true
       :doc "The keys supported of fixture fns."}
  fn-keys [:fn :args :void])

(def ^{:doc "The parameter instance field prefix. Disambiguate from
            possible external class parameter fields."}
  param-field-prefix (str (gensym "_param_") "_"))

(def ^{:doc "Mapping of a type to the respective class field name."}
  value-field-name
  {:long "_long_value"
   :double "_double_value"
   Object "_value"})

;;;

(defn- fn-map
  "If `x` is a map, return it. If x is non-nil, wrap it in a :fn map.
  Otherwise, return nil."
  [x]
  (cond
    (map? x) x
    (some? x) {:fn x}))

(defn- phases
  "Normalize each phase of the given level, found in the `levels` map."
  [level levels]
  (when-let [pm (level levels)]
    (let [pm (update (fn-map pm) :setup fn-map)
          pm (update pm :setup merge (select-keys pm fn-keys))]
      (for [pk phase-keys
            :let [m (fn-map (pk pm))]
            :when (:fn m)]
        (assoc (select-keys m fn-keys)
               :level level, :phase pk)))))

(defn fixture-seq
  "Return a seq of flattened fixture maps representing each of the
  given state value fns and at which point they are to be called."
  [x]
  (let [m (update (fn-map x) :trial fn-map)
        m (update m :trial merge
                  (select-keys m phase-keys)
                  (select-keys m fn-keys))]
    (mapcat phases level-keys (repeat m))))

(defn fixtures
  "Return a seq of flattened fixture maps from the state."
  [s]
  (let [arg-keyword (partial util/ns-keyword "param")]
    (for [g (fixture-seq s)]
      (let [f (util/check-valid "state fixture" g :fn)
            f (cond
                (var? f)
                (util/var-symbol f)
                (util/intern-symbol? f)
                (symbol "clojure.core" (name f))
                :else
                f)
            args (map arg-keyword (:args g))]

        (if (seq? f)
          (util/eval-fn f)
          (util/check (symbol? f)
                      (str "invalid state :fn value: expected symbol "
                           "or fn expression sequence: " (pr-str f))))

        (util/some-assoc g :state s, :fn f, :args (seq args))))))

(defn normalize
  "Return a normalized map from the raw state map entry on the first
  pass. Adorn with additional data on the second pass."
  (^:pass-1 [k x]
   (let [k (util/ns-keyword "state" k)
         s (if (map? x) x {:fn x})]
     (assoc s :key k)))
  (^:pass-2 [s]
   (let [fxs (fixtures s)
         pkeys #(filter util/param-keyword? (:args %))
         params (set (mapcat pkeys fxs))]
     (util/check (seq fxs)
                 (str "no fns found for state: " (:key s)))
     (assoc s :fixtures fxs, :params params))))

;;;

(defn- root-level?
  "Returns true if the fixture map is the root fixture (i.e., the setup
  phase fn at the trial level)."
  [g]
  (and (= (:level g) :trial)
       (= (:phase g) :setup)))

(defn fn-field-desc
  "Return the fixture field fn descriptor for the fixture map."
  [{f :fn :as g}]
  (let [fvar (when (symbol? f)
               (util/require-fn f))

        nargs (count (:args g))
        alists (some-> fvar meta :arglists sort seq)
        nargs (if (root-level? g) (long nargs) (inc nargs))

        _ (util/check (or (nil? alists)
                          (util/matching-arity alists nargs))
                      (format "%s does not support arity %d" f nargs))

        desc (repeat (inc nargs) Object)]

    (util/check (<= nargs 20) "max state arity allowed is 20")
    desc))

(defn method-name
  "Return the string name of the method for fixture."
  [g]
  (str (name (:level g)) "_" (name (:phase g))))

(defn fn-field-name
  "Return the string name of the fn field for the provided fixture."
  [g]
  (str "_" (method-name g) "_fn"))

(defn param-name
  "Return the field name of the parameter key."
  [k]
  (str param-field-prefix (munge (name k))))

(defn fn-load
  "Return the instructions to load the given fixture."
  [{f :fn :as g}]
  (let [path (map name [(-> g :state :key) (:level g) (:phase g)])
        suffix (interpose \. path)
        metric (keyword (apply str "jmh.state." suffix)
                        (if (seq? f)
                          "expr"
                          (str (namespace f) "." (name (:name g f)))))]
    [(instrument/load-fn f metric)
     [:putstatic :this (fn-field-name g) IFn]]))

(defn method-emits
  "Return a map of bytecode snippets for later assembly. Takes a
  fixture map and whether or not this fixture supports primitives."
  [g]
  (let [prim? (-> g :state :prim)
        fname (fn-field-name g)
        vname (value-field-name Object)
        prelude [:getstatic :this fname IFn]

        prelude (if (root-level? g)
                  prelude
                  [prelude
                   [:aload 0]
                   [:getfield :this vname Object]])

        load-swap [[:aload 0] [:swap]]
        put (if (:void g)
              [:pop]
              [(when prim? [:dup])
               load-swap
               [:putfield :this vname Object]
               (when prim?
                 [[:dup]
                  load-swap
                  [:invokestatic RT "longCast" [Object :long]]
                  [:putfield :this (value-field-name :long) :long]
                  load-swap
                  [:invokestatic RT "doubleCast" [Object :double]]
                  [:putfield :this (value-field-name :double) :double]])])]

    {:prelude prelude, :set-field put}))

(defn load-indexes
  "Return the method local argument indexes, in :args order, for the
  given fixture. Parameters are ignored."
  [g]
  (-> (reduce (fn [[^long n :as s] k]
                (if (util/param-keyword? k)
                  (conj s n)
                  (conj s (inc n))))
              '(1) (:args g))
      next reverse))

(defn method-desc
  "Return the fixture method descriptor for the fixture."
  [g]
  (let [resolver (-> g :state :jmh/resolver)
        desc (for [k (:args g)
                   :when (not (util/param-keyword? k))]
               (if (util/state-keyword? k)
                 (util/check-valid "state" resolver k)
                 (util/check-valid "jmh keyword" util/jmh-keyword? k)))]
    (conj (vec desc) :void)))

(defn method-body
  "Return the fixture method instructions for the fixture."
  [g]
  (let [resolver (-> g :state :jmh/resolver)
        emits (method-emits g)
        indexes (load-indexes g)

        load-local (fn [i k]
                     (if (util/param-keyword? k)
                       [[:aload 0]
                        [:getfield :this (param-name k) String]
                        [:invokestatic Util "read" [String Object]]]
                       [[:aload i]
                        (when (util/state-keyword? k)
                          (let [x (util/check-valid "state" resolver k)]
                            (when-not (util/class-symbol? x)
                              [:getfield x (value-field-name Object) Object])))
                        [:ldc nil]
                        [:astore i]]))]
    [(:prelude emits)
     (mapcat load-local indexes (:args g))
     [:invokeinterface IFn "invoke" (fn-field-desc g)]
     (:set-field emits)
     [:return]]))

(defn value-fields
  "Return the mutable instance fields, for updating by fixture methods.
  If `prim?`, primitive numeric fields are added."
  [prim?]
  (let [fields [{:flags [:public]
                 :name (value-field-name Object)
                 :type Object}]]
    (if prim?
      (conj fields
            {:flags [:public]
             :name (value-field-name :long)
             :type :long}
            {:flags [:public]
             :name (value-field-name :double)
             :type :double})
      fields)))

(defn class-fields
  "Return a seq of all static and instance field data for the state."
  [s]
  (concat
   (value-fields (:prim s))
   (for [p (:jmh/params s)]
     {:flags [:private], :name (:name p)
      :type String, :annotations {Param (:values p)}})
   (for [g (:fixtures s)]
     {:flags [:private :final :static]
      :name (fn-field-name g), :type IFn})))

(defn class-type
  "Yield the proxy class data of the provided state value."
  [s]
  (let [clinit {:name :clinit
                :emit [(mapcat fn-load (:fixtures s))
                       [:return]]}

        lifecycle (for [g (:fixtures s)]
                    (let [phase (if (= (:phase g) :setup) Setup TearDown)
                          level (util/check-valid "level" util/level? (:level g))]
                      {:flags [:public :final]
                       :name (method-name g)
                       :desc (method-desc g)
                       :annotations {phase level}
                       :emit (method-body g)}))

        methods (conj (vec lifecycle) clinit)
        scope (util/check-valid "scope" util/scope? (:scope s :benchmark))]

    {:flags [:public], :name (:class s)
     :fields (class-fields s)
     :methods methods
     :annotations {State scope}}))
