(ns ^:internal ^:no-doc jmh.emit
  "Bytecode instruction helpers."
  (:require [jmh.util :as util])
  (:import [io.github.jgpc42.jmh Util]
           [clojure.lang IFn]))

(defn load-resolve
  "Instructions to resolve a namespaced fn symbol."
  [s]
  [[:ldc (namespace s)]
   [:ldc (name s)]
   [:invokestatic Util "resolve" [String String IFn]]])

(defn load-eval
  "Instructions to evaluate a fn form."
  [form]
  [[:ldc (util/readable-str form)]
   [:invokestatic Util "eval" [String IFn]]])
