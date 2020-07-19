## 0.3.1 (2020-07-19)

* BUGFIX: compile Java code for advertized baseline JDK 8 compatibility

## 0.3.0 (2020-07-14)

* Upgrade JMH to latest version `1.23`
* Add a `deps.edn` file

## 0.2.1 (2017-12-29)

* Include new benchmark result key `:score-confidence`
* Include extra `:statistics` info for benchmark and profiler results when available
* Show original parameter names for benchmark result `:args`, not the gensyms

## 0.2.0 (2017-12-17)

* Support external profiler classes and profiler registry
* BUGFIX: `:java` should be subkey of `:jvm` and not at top-level of `:fork` option

## 0.1.7 (2017-11-09)

* Default `:fail-on-error` option to `true`
* Improve error messages for `:fn` expressions

## 0.1.6 (2017-10-20)

* BUGFIX: sometimes score units would be missing in result maps
* Better benchmark and state `:fn` validation

## 0.1.5 (2017-10-07)

* Undefined `:select` keywords to `jmh.core/run` use implicit selectors ([#2][issue2])
* Treat string `:params` as normal strings

## 0.1.4 (2017-10-06)

* BUGFIX: `:synchronize` should default to `true` when option `:iterations` is passed to `jmh.core/run`

## 0.1.3 (2017-10-02)

* BUGFIX: exception when passing global time options (e.g., `:timeout`) to `jmh.core/run`
* Use `:expr` not `"expr"` as the default name for benchmark `:fn` expressions

## 0.1.2 (2017-10-02)

* BUGFIX: [#1][issue1] NPE when no benchmarks are defined/selected

## 0.1.1 (2017-09-27)

* Compile java files with `-target 1.6` for older JVM compatibility



[issue1]:  https://github.com/jgpc42/jmh-clojure/issues/1
[issue2]:  https://github.com/jgpc42/jmh-clojure/issues/2
