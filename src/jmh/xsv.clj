(ns ^:no-doc jmh.xsv
  "Parse x-separated value strings.")

(defmacro ^:private expect [expr msg]
  `(when-not ~expr
     (throw (AssertionError. (str ~msg " when parsing xsv")))))

(defn ^:internal parse-line
  "Read a xsv line according to https://tools.ietf.org/html/rfc4180."
  ([s] (parse-line \, s))
  ([^Character sep ^String s]
   (let [end (.length s)]
     (loop [pos 0, q nil, sb (StringBuilder.), fs []]
       (cond
         (== pos end)
         (do (expect (not q) "unclosed quote")
             [(conj fs (.toString sb)), ""])

         (and (not q) (< (inc pos) end)
              (= \return (.charAt s pos))
              (= \newline (.charAt s (inc pos))))
         [(conj fs (.toString sb)), (.substring s (+ 2 pos))]

         (and (= sep (.charAt s pos)) (not q))
         (recur (inc pos) nil (StringBuilder.)
                (conj fs (.toString sb)))

         (and (= \" (.charAt s pos)) (not (false? q)))
         (if q
           (if (and (< (inc pos) end)
                    (= \" (.charAt s (inc pos))))
             (recur (+ 2 pos) q (.append sb \") fs)
             (recur (inc pos) false sb fs)) ;; false = now expect sep or crlf
           (do (expect (zero? (.length sb)) "invalid quote")
               (recur (inc pos) true sb fs)))

         :else
         (do (expect (not (false? q)) "trailing after closing quote")
             (recur (inc pos) q (.append sb (.charAt s pos)) fs)))))))

(defn parse
  "Parse a csv string (with header line) into a seq of maps."
  [csv]
  (let [[headers csv] (parse-line csv)]
    (loop [csv csv, v []]
      (if (or (= "" csv) (= "\r\n" csv))
        v
        (let [[fields csv] (parse-line csv)]
          (expect (== (count fields) (count headers))
                  "header count mismatch")
          (recur csv (conj v (zipmap headers fields))))))))
