
[button chip lamp]

(fn [part-name]
  (set-value lamp (get-value button))
  (when (= part-name button)
    (while (= (get-value button) 1)
      (activate chip)
      (wait #(chip-active? chip)))))
