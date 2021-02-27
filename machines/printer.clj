
[toggle x end-x z end-z y]

(fn [part-name]
  (while (on? toggle)
    (cond
      (and (on? end-z) (on? end-x)) (activate y)
      (on? end-x) (activate z)
      :else (activate x))
    (sleep 100)))
