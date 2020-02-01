
(script
 [button]
 [chip]

 (fn [part-name]
   (if (and
        (= part-name button)
        (= (get-value button) 1))
     (dotimes [i 12]
       ;; (println! ">" (get-value stop-button))
       (activate chip)
       (wait #(chip-active? chip))
       ))
   ))

