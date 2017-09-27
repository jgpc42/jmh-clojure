(ns ^:no-doc jmh.java
  "Compile java source code."
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream]
           [java.net URI]
           [javax.tools Diagnostic Diagnostic$Kind DiagnosticCollector ForwardingJavaFileManager
            JavaFileObject$Kind SimpleJavaFileObject StandardLocation ToolProvider]))

(def ^{:dynamic true
       :doc "The javac compiler options to use."}
  *compile-args* ["-proc:none"])

(defn- compile-error
  "Return an exception describing the compilation errors."
  [^DiagnosticCollector diags]
  (let [errs (for [^Diagnostic d (.getDiagnostics diags)
                   :when (= Diagnostic$Kind/ERROR (.getKind d))]
               {:line (.getLineNumber d)
                :message (.getMessage d nil)
                :source (.getName ^SimpleJavaFileObject (.getSource d))})
        err (first errs)
        msg (format "java compilation error: %s: line %d: %s"
                    (:source err) (:line err) (:message err))
        msg (if-let [more (next errs)]
              (str msg "\n... and " (count more) " more")
              msg)]
    (ex-info msg {:errors errs})))

(defn- source-file
  "Return a proper java source file object for the given class name and
  source code."
  [cls code]
  (let [code (str code)
        uri (URI. (str "string:///" (.replace (str cls) \. \/) ".java"))]
    (proxy [SimpleJavaFileObject] [uri JavaFileObject$Kind/SOURCE]
      (getCharContent [_] code))))

(defn compile
  "Compile a map of {class-name source-code}. Returns a seq of maps for
  each class with the compiled classes' :name and :bytes.

  Adds the location specified by `*compile-path*` to the compilation
  class path."
  [srcs]
  (when (seq srcs)
    (let [units (vec (for [[cname src] srcs]
                       (source-file cname src)))

          javac (ToolProvider/getSystemJavaCompiler)
          mgr (.getStandardFileManager javac nil nil nil)
          entries (.getLocation mgr StandardLocation/CLASS_PATH)
          dir (io/file (or *compile-path* "classes"))
          _ (when-not (some #{dir} entries)
              (.mkdirs dir)
              (.setLocation mgr StandardLocation/CLASS_PATH (cons dir entries)))

          codes (volatile! {})
          mgr (proxy [ForwardingJavaFileManager] [mgr]
                (getJavaFileForOutput [_ cname _ _]
                  (let [bas (ByteArrayOutputStream.)]
                    (vswap! codes assoc cname bas)
                    (proxy [SimpleJavaFileObject] [(URI. cname) JavaFileObject$Kind/CLASS]
                      (openOutputStream [] bas)))))

          diags (DiagnosticCollector.)
          task (.getTask javac nil mgr diags (vec *compile-args*) nil units)]

      (when-not (.call task)
        (throw (compile-error diags)))

      (doall
       (for [[cname ^ByteArrayOutputStream bas] @codes]
         {:name cname, :bytes (.toByteArray bas)})))))
