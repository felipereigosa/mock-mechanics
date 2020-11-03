
[button probe0 probe1]

(fn [part-name]
  (when (on? button)
    (let [max-distance 0.25]
      (set-value probe0 (if (nil? (get-part probe0 max-distance)) 0 1)))))
