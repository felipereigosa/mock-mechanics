
[collision-probe button lamp down-chip up-chip reset-chip copy-button]

(fn [part-name]
  (set-value lamp (get-value button))

  (when (= part-name button)
    (while (on? button)
      (activate down-chip)
      (wait #(chip-active? down-chip))

      (when (on? collision-probe)
        (press-button copy-button)
        ;; (wait #(on? copy-button))
        (sleep 200) ;;##########################b

        (activate up-chip)
        (wait #(chip-active? up-chip))
        
        (dotimes [i (rand-int 5)]
          (activate reset-chip)
          (wait #(chip-active? reset-chip)))
        ))))
