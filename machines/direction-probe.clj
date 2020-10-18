
[button probe lamp]

(fn [part-name]
  (when (on? button)
    (set-value lamp (if (nil? (get-part probe 0.25)) 0 1))))
