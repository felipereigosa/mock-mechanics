
[button open close]


(fn [part-name]
  ;; (when (and (= part-name button)
  ;;            (on? button))

  ;;   (activate close-chip)
  ;;   (activate open-chip)

  ;;   )

  (when (on? button)
    (activate open)
    (sleep 1000)
    (activate close))    
  )
