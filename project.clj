(def version
  {:jmh "1.19"})

(def boot-classpath
  (str "-Xbootclasspath:"
       (System/getProperty "sun.boot.class.path")))

(defproject jmh-clojure "0.1.5-SNAPSHOT"
  :description "Benchmarking with JMH, the Java Microbenchmark Harness, from Clojure."
  :url "https://github.com/jgpc42/jmh-clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[insn "0.1.4"]
                 [org.clojure/clojure "1.8.0"]
                 [org.openjdk.jmh/jmh-core ~(:jmh version)]
                 [org.openjdk.jmh/jmh-generator-reflection ~(:jmh version)]]

  :min-lein-version "2.0.0"
  :jar-exclusions [#".+\.java$"]
  :java-source-paths ["java"]
  :javac-options ["-target" "1.6", "-source" "1.6" ~boot-classpath]
  :test-selectors {:unit (complement :integration)}

  :aliases {"test-all" ["do" "javac," "test,"
                        "with-profile" "+1.9" "test,"
                        "with-profile" "+1.7" "test"]}

  :profiles
  {:1.7 {:dependencies [[org.clojure/clojure "1.7.0"]]}
   :1.9 {:dependencies [[org.clojure/clojure "1.9.0-beta2"]]}
   :repl {:source-paths ["dev"]}})
