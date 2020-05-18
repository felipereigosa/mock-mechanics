
(script
 [probe]
 [chip]

 (fn [part-name]
   (when (and (= part-name probe)
              (= (get-value probe) 1))
     (while (= (get-value probe) 1)
       (activate chip)
       (wait #(chip-active? chip))
       ))))
