(ns jmh.option
  "Helpers for transforming jmh option maps."
  (:require [jmh.util :as util])
  (:import [org.openjdk.jmh.runner Defaults]))

(def ^{:private true, :no-doc true
       :doc "Property definition to enable debug output."}
  debug "jmh-clojure.debug")

(def ^{:internal true, :no-doc true
       :doc "Default values for some required option map values that may
            be left unspecified."}
  defaults
  {:gc Defaults/DO_GC
   :forks Defaults/MEASUREMENT_FORKS
   :sync Defaults/SYNC_ITERATIONS})

(def ^{:internal true, :no-doc true
       :doc "Property definition to ignore lock file errors."}
  ignore-lock "jmh.ignoreLock")

(def ^:dynamic *type-aliases*
  "The option sets available for the :type shortcut. By
  default, the following types are available:

    :quick  1 fork, 5 warmup/measurement iterations.
    :test   no forking, single-shot, 1 thread."
  {:quick {:fail-on-error true
           :fork 1
           :measurement 5
           :warmup 5}
   :test {:fail-on-error true
          :fork 0
          :mode :single-shot
          :threads 1}})

;;;

(defn ^:internal ^:no-doc debug?
  "Returns true if debug output is enabled."
  [opts]
  (or (:jmh/debug opts)
      (#{"" "true" "1"} (System/getProperty debug))))

(defn ^:internal ^:no-doc normalize
  "Return the fully expanded options map."
  [m]
  (let [m (if-let [t (:type m)]
            (->> (dissoc m :type)
                 (merge (util/check-valid "type" *type-aliases* t)))
            m)

        expand (fn [m root val-key]
                 (let [x (get m root)]
                   (if (or (nil? x) (map? x))
                     m
                     (assoc m root {val-key x}))))
        m (-> (expand m :fork :count)
              (expand :measurement :iterations)
              (expand :warmup :iterations))

        m (reduce (fn [m k]
                    (if-let [n (get-in m [k :count])]
                      (if (pos? (long n))
                        m
                        (update m k dissoc :count))
                      m))
                  m [:measurement :warmup])]

    (if-let [n (and (:group m) (:threads m))]
      (-> (assoc m :group-threads n) (dissoc :threads))
      m)))

(defn without-forking
  "Return the given options with process forking disabled."
  [opts]
  (update (normalize opts) :fork assoc :count 0))

(defn without-locking
  "Return the given options with the jmh file lock disabled."
  [opts]
  (let [arg (str "-D" ignore-lock "=true")
        path [:fork :jvm :prepend-args]]
    (update-in (normalize opts) path conj arg)))
