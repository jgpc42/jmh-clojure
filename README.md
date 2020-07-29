[![Clojars Project](https://img.shields.io/clojars/v/jmh-clojure.svg)](https://clojars.org/jmh-clojure)
[![](https://github.com/jgpc42/jmh-clojure/workflows/Test%20runner/badge.svg)][ci]

### Dependency and version information
<details>
  <summary>Click to show</summary>

[Leiningen][lein]

``` clojure
[jmh-clojure "0.3.1"]
```

[tools.deps][deps]

```clojure
{jmh-clojure {:mvn/version "0.3.1"}}
```

[Maven][maven]

``` xml
<dependency>
  <groupId>jmh-clojure</groupId>
  <artifactId>jmh-clojure</artifactId>
  <version>0.3.1</version>
</dependency>
```

JDK versions 8 to 14 and Clojure versions 1.7 to 1.10 are currently [supported][ci].
</details>

### What is it?

This library provides a data-oriented API to [JMH][jmh], the Java Microbenchmark Harness. It can be used directly or via Leiningen with [lein-jmh][lein-jmh].

JMH is developed by OpenJDK JVM experts and goes to great lengths to ensure accurate benchmarks. Benchmarking on the JVM is a complex beast and, by extension, JMH takes a bit of effort to learn and use properly. That being said, JMH is very robust and configurable. If you are new to JMH, I would recommend browsing the [sample][samples] code and javadocs before using this library.

If you a need simpler, less strenuous tool, I would suggest looking at the popular [criterium][criterium] library.

### Quick start

As a simple example, let's say we want to benchmark our fn that gets the value at an arbitrary indexed type. Of course, the built in `nth` already does this, but we can't extend `nth` to existing types like `java.nio.ByteBuffer`, etc.

```clojure
(ns demo.core)

(defprotocol ValueAt
  (value-at [x idx]))

(extend-protocol ValueAt
  clojure.lang.Indexed
  (value-at [i idx]
    (.nth i idx))
  CharSequence
  (value-at [s idx]
    (.charAt s idx))
  #_...)
```

Benchmarks are [usually](#alternate-ways-to-run) described in data and are fully separated from definitions. The reason for this is twofold. First, decoupling is generally good design practice. And second, it allows us to easily take advantage of JMH process isolation (forking) for reliability and accurracy. More on this later.

For repeatability, we'll place the following data in a `benchmarks.edn` resource file in our project. (Note that using a file is not a requirement, we could also specify the same data in Clojure. The `:fn` key values would need to be quoted in that case, however.)

```clojure
{:benchmarks
 [{:name :str, :fn demo.core/value-at, :args [:state/string, :state/index]}
  {:name :vec, :fn demo.core/value-at, :args [:state/vector, :state/index]}]

 :states
 {:index {:fn (partial * 0.5), :args [:param/count]} ;; mid-point
  :string {:fn demo.utils/make-str, :args [:param/count]}
  :vector {:fn demo.utils/make-vec, :args [:param/count]}}

 :params {:count 10}}
```

I have omitted showing the `demo.utils` namespace for brevity, it is defined [here][utils] if interested.

The above data should be fairly easy to understand. It is also a limited view of what can be specified. The [sample file][sample] provides a complete reference and explanation.

Now to run the benchmarks. We'll start a REPL in our project and evaluate the following. Note that we could instead use [lein-jmh][lein-jmh] to automate this entire process.

```clojure
(require '[jmh.core :as jmh]
         '[clojure.java.io :as io]
         '[clojure.edn :as edn])

(def bench-env
  (-> "benchmarks.edn" io/resource slurp edn/read-string))

(def bench-opts
  {:type :quick
   :params {:count [31 100000]}
   :profilers ["gc"]})

(jmh/run bench-env bench-opts)
;; => ({:name :str, :params {:count 31},     :score [1.44959801438209E8 "ops/s"], #_...}
;;     {:name :str, :params {:count 100000}, :score [1.45485370497829E8 "ops/s"]}
;;     {:name :vec, :params {:count 31},     :score [1.45550038851249E8 "ops/s"]}
;;     {:name :vec, :params {:count 100000}, :score [8.5783753539823E7 "ops/s"]})
```

The `run` fn takes a benchmark environment and an optional map. We select the `:quick` type: an [alias][alias-doc] for some common options. We override our default `:count` parameter sequence to measure our fn against small and large inputs. We also enable the gc profiler.

Notice how we have four results: one for each combination of parameter and benchmark fn. For this example, we have omitted lots of additional result map data, including the [profiler][profilers] information.

Note that the above results were taken from multiple [runs][result], which is always a good practice when benchmarking.

#### Alternate ways to run

Benchmarking expressions or fns manually without the data specification is also supported. For example, the `run-expr` macro provides an interface similar to criterium, and allows benchmarking of code that only resides in memory (that you are updating in a REPL, for example), rather than on disk (loadable via `require`). However, this forgoes JMH process isolation. For more on why benchmarking this way on the JVM can be sub-optimal, see [here][extended].

### More information

As previously mentioned, please see the [sample file][sample] for the complete benchmark environment reference. For `run` options, see the [docs][run-doc]. Also, see the [wiki][wiki] for additional examples and topics.

The materials for a talk I gave at a [London Clojurians][london] online meetup are also available [here][talk]. A video capture of the event can also be viewed on [YouTube][video].

### Running the tests

```bash
lein test
```

Or, `lein test-all` for all supported Clojure versions.

### License

Copyright © 2020 Justin Conklin

Distributed under the Eclipse Public License, the same as Clojure.



[alias-doc]:  https://jgpc42.github.io/jmh-clojure/doc/jmh.option.html#var-*type-aliases*
[ci]:         https://github.com/jgpc42/jmh-clojure/blob/master/.github/workflows/test.yml
[criterium]:  https://github.com/hugoduncan/criterium
[deps]:       https://github.com/clojure/tools.deps.alpha
[extended]:   https://github.com/jgpc42/jmh-clojure/wiki/Extended
[jmh]:        http://openjdk.java.net/projects/code-tools/jmh/
[lein]:       http://github.com/technomancy/leiningen
[lein-jmh]:   https://github.com/jgpc42/lein-jmh
[london]:     https://www.meetup.com/London-Clojurians/
[maven]:      http://maven.apache.org
[profilers]:  https://github.com/jgpc42/jmh-clojure/wiki/JMH-Profilers
[result]:     https://gist.github.com/jgpc42/4d8a828f8d0739748afa71035f2b2c9c#file-results-edn
[run-doc]:    https://jgpc42.github.io/jmh-clojure/doc/jmh.core.html#var-run
[sample]:     https://github.com/jgpc42/jmh-clojure/blob/master/resources/sample.jmh.edn
[samples]:    http://hg.openjdk.java.net/code-tools/jmh/file/1ddf31f810a3/jmh-samples/src/main/java/org/openjdk/jmh/samples/
[talk]:       https://github.com/jgpc42/london-clojurians-jmh-talk-2020
[utils]:      https://gist.github.com/jgpc42/4d8a828f8d0739748afa71035f2b2c9c#file-utils-clj
[video]:      https://www.youtube.com/watch?v=_6qVfFkBdWI
[wiki]:       https://github.com/jgpc42/jmh-clojure/wiki
