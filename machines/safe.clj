
[probe track]

(fn [part-name]
  (if (on? probe)
    (set-thing! [:parts track :max-angle] 0.003)
    (set-thing! [:parts track :max-angle] nil)))
