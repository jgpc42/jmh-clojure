(ns ^:internal ^:no-doc jmh.result
  "Transform jmh benchmark output into Clojure data."
  (:require [jmh.util :as util]
            [jmh.option :as option]
            [jmh.state :as state]
            [jmh.xsv :as xsv]
            [clojure.edn :as edn])
  (:import [org.openjdk.jmh.annotations Mode]))

(defmulti ^:private add-field
  "Update the map with the result of parsing the xsv field tuple."
  (fn [m entry] (first entry)))

(def ^:private assoc-sorted (fnil assoc (sorted-map)))

;;;

(defn merge-secondary
  "Update the each benchmark results with its secondary results under
  the given pluralized key."
  [secondary-key secondaries results]
  (let [row-key (juxt :name :index :method :mode :params)
        add-row (fn [m r]
                  (let [k (row-key r)
                        label (get r secondary-key)
                        r (dissoc r secondary-key :index :method
                                  :mode :params :threads)]
                    (update m k assoc-sorted label r)))
        row-map (->> secondaries
                     (reduce add-row {}))
        result-key (-> secondary-key name (str "s") keyword)]
    (for [r results]
      (util/some-assoc r result-key (row-map (row-key r))))))

(defn benchmark-seq
  "Parse jmh csv output into a seq of maps."
  [pkg output]
  (let [init (with-meta {} {:jmh/pkg pkg})
        results (->> (xsv/parse output)
                     (map (partial reduce add-field init)))
        percentiles (filter :percentile results)
        profilers (filter :profiler results)]
    (->> results
         (remove #(or (:percentile %) (:profiler %)))
         (merge-secondary :percentile percentiles)
         (merge-secondary :profiler profilers)
         (sort-by (juxt :class :method :index)))))

(defn parse
  "Parse the jmh output and return a sequence of benchmark result maps."
  [env]
  (let [benchmarks (vec (:jmh/benchmarks env))]

    (when (option/debug? (:jmh/options env))
      (util/debug "Parsing output"))

    (for [m (benchmark-seq (:jmh/pkg env) (:jmh/output env))]
      (if (:method m)
        m
        (let [b (-> (nth benchmarks (:index m))
                    (select-keys [:fn :name :args])
                    (with-meta {:jmh/options (:options m)}))]
          (apply util/some-assoc m (mapcat identity b)))))))

;;;

(defmethod add-field "Benchmark" [m [_ v]]
  (let [[method prof] (.split ^String v ":\u00b7?")
        [m ^String v]
        (if prof
          (if-let [[_ pct] (re-find #"\u00b7p([\d.]+)$" prof)]
            (let [pct (Double/valueOf ^String pct)]
              [(assoc m :percentile pct) method])
            [(assoc m :profiler prof) method])
          [m v])]
    (if (.startsWith v ^String (-> m meta :jmh/pkg))
      (let [idx (-> (.split v "_") next ^String (first) Long/valueOf)]
        (assoc m :index idx))
      (let [idx (.lastIndexOf v ".")]
        (assoc m :method (symbol (subs v 0 idx), (subs v (inc idx))))))))

(defmethod add-field "Mode" [m [_ v]]
  (assoc m :mode (util/mode-name? (Mode/deepValueOf v))))

(defmethod add-field "Samples" [m [_ ^String v]]
  (assoc m :samples (Long/valueOf v)))

(defmethod add-field "Score" [m [_ ^String v]]
  (assoc m :score [(Double/valueOf v)]))

(defmethod add-field "Score Error (99.9%)" [m [_ ^String v]]
  (let [d (Double/valueOf v)]
    (if (or (Double/isNaN d) (= d 0.0))
      m
      (assoc m :score-error d))))

(defmethod add-field "Threads" [m [_ ^String v]]
  (assoc m :threads (Long/valueOf v)))

(defmethod add-field "Unit" [m [_ v]]
  (update m :score conj v))

(defmethod add-field :default [m [k v]]
  (if (= v "")
    m
    (let [[_ ^String p] (re-find #"^Param: (.+)" k)]
      (util/check p (str "could not parse xsv parameter: " k))
      (if (.startsWith p state/param-field-prefix)
        (let [n (count state/param-field-prefix)
              k (keyword (subs (.toLowerCase p) n))]
          (update m :params assoc-sorted k (edn/read-string v)))
        (update m :params assoc-sorted p v)))))
