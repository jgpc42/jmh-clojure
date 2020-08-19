(comment

  (set! *warn-on-reflection* true)
  (set! *unchecked-math* :warn-on-boxed)

  (apply run-tests (filter #(re-find #"^jmh\..+-test$" (str (ns-name %))) (all-ns)))

  )
