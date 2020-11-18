
[button probe lamp]

(fn [part-name]
  (when (on? button)
    (let [max-distance 0.25]
      (set-value lamp (if (nil? (get-part probe max-distance)) 0 1)))))
