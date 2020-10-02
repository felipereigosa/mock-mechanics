
[button chip]

(fn [part-name]
  (when (= part-name button)
    (while (= (get-value button) 1)
      (activate chip)
      (wait #(chip-active? chip)))))
