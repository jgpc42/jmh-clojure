(comment

  (with-warnings (refresh))

  (apply run-tests (filter #(re-find #"^jmh\..+-test$" (str (ns-name %))) (all-ns)))

  )
