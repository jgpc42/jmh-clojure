(def dependencies
  (->> "deps.edn" slurp read-string
       :deps (mapv #(vector (% 0) (:mvn/version (% 1))))
       (into '[[org.clojure/clojure "1.8.0"]])))

(def javac-options
  (let [spec (-> (System/getProperty "java.specification.version")
                 Double/valueOf)]
    (if (= spec 1.8)
      []
      `["-target" "1.8"
        "-source" "1.8"
        ~@(when-let [root (System/getenv "JAVA_8_HOME")]
            [(str "-Xbootclasspath:"
                   (-> (java.io.File. root "jre/lib/rt.jar") .getCanonicalPath))])])))

(defproject jmh-clojure "0.4.1-SNAPSHOT"
  :description "Benchmarking with JMH, the Java Microbenchmark Harness, from Clojure."
  :url "https://github.com/jgpc42/jmh-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies ~dependencies

  :min-lein-version "2.0.0"
  :jar-exclusions [#".+\.java$"]
  :java-source-paths ["java"]
  :javac-options ~javac-options
  :test-selectors {:unit (complement :integration)}

  :aliases {"test-all" ["do" "javac," "test,"
                        "with-profile" "+1.10" "test,"
                        "with-profile" "+1.9" "test,"
                        "with-profile" "+1.7" "test"]}

  :profiles
  {:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
   :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
   :1.10 {:dependencies [[org.clojure/clojure "1.10.0"]]}
   :repl {:source-paths ["dev"]}})
