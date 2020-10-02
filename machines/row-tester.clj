
[button pointer step-chip reset-chip]

(fn [part-name]
  (when (and (= part-name button)
           (= (get-value button) 1))
    (while (get-part pointer)
      (activate step-chip)
      (wait #(chip-active? step-chip)))
    (activate reset-chip)))
