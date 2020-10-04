
[button probe open-chip close-chip]


(fn [part-name]
  (when (and (= part-name button)
             (on? button))

    (activate close-chip)
    (activate open-chip)

    ))
