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

* BUGFIX: [#1](https://github.com/jgpc42/jmh-clojure/issues/1) NPE when no benchmarks are defined/selected

## 0.1.1 (2017-09-27)

* Compile java files with `-target 1.6` for older JVM compatibility



[issue2]:  https://github.com/jgpc42/jmh-clojure/issues/2
