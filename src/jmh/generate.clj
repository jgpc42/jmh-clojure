(ns ^:internal ^:no-doc jmh.generate
  "Write class and resource files to the classpath."
  (:require [jmh.java :as java]
            [jmh.util :as util]
            [jmh.option :as option]
            [insn.core :as insn]
            [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream File FileOutputStream IOException PrintWriter StringWriter]
           [org.openjdk.jmh.generators.reflection RFGeneratorSource]
           [org.openjdk.jmh.generators.core BenchmarkGenerator GeneratorDestination]))

(defn- class-file
  "Write the type map containing a classes' :name and :bytes to the
  location specified by `*compile-path*`. Returns the destination File."
  [t]
  (let [root *compile-path*
        paths (.split ^String (:name t) "\\.")
        ^File dir (apply io/file root (butlast paths))
        file (io/file dir (str (last paths) ".class"))]
    (.mkdirs dir)
    (doto (FileOutputStream. file)
      (.write ^bytes (:bytes t))
      .close)
    file))

(defn- resource-file
  "Write the resource map containing relative :path and byte :stream
  to `*compile-path*`. Returns the destination File."
  [r]
  (let [root *compile-path*
        file (io/file root (:path r))
        text (.toString ^ByteArrayOutputStream (:stream r) "UTF-8")]
    (.mkdirs (.getParentFile file))
    (doto file (spit text))))

(defn- generation-error
  "Return an exception describing the generation errors."
  [[err & more :as errs]]
  (let [msg (str "jmh generation error: " err)
        msg (if more
              (str msg "\n... and " (count more) " more")
              msg)]
    (ex-info msg {:errors errs})))

(defn- process
  "Returns a sequence of the resultant resource and class files
  written from processing the provided classes with jmh."
  [classes]
  (let [src (doto (RFGeneratorSource.)
              (.processClasses ^java.util.Collection (vec classes)))

        [errs resx] (repeatedly #(volatile! []))
        srcs (volatile! {})
        dest (proxy [GeneratorDestination] []
               (getResource [_]
                 (throw (IOException.)))
               (newClass [cname]
                 (let [w (StringWriter.)]
                   (vswap! srcs assoc cname w)
                   (PrintWriter. w true)))
               (newResource [rname]
                 (let [s (ByteArrayOutputStream.)]
                   (vswap! resx conj {:path rname, :stream s})
                   s))
               (printError [& args]
                 (vswap! errs conj args)))]

    (doto (BenchmarkGenerator.)
      (.generate src dest)
      (.complete src dest))

    (when-let [errs (seq @errs)]
      (throw (generation-error errs)))

    (concat (map resource-file @resx)
            (map class-file (java/compile @srcs)))))

(defn write
  "Emit all classes, and jmh resource files to the classpath. Returns
  an updated environment."
  [env]
  (let [codes (map insn/visit (:jmh/types env))

        loader (reduce #(assoc % (:name %2) (:bytes %2))
                       {} codes)
        cl (util/delegate-classloader loader)
        classes (concat (map #(Class/forName (:name %) true cl) codes)
                        (map #(Class/forName (name %)) (:jmh/externs env)))

        files (binding [*compile-path* (:jmh/path env)]
                (doall (concat (mapv class-file codes)
                               (process classes))))]

    (when (option/debug? (:jmh/options env))
      (util/debug "Wrote" (count files) "files to" (:jmh/path env)))

    (assoc env :jmh/files files)))
