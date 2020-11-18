
[button probe open-chip close-chip]

(fn [part-name]
  (if (on? button)
    (if (on? probe)
      (activate open-chip)
      (activate close-chip))))




