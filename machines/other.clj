
(script
 [button]
 [chip]

 (fn [part-name]
   (when (and (= part-name button)
              (= (get-value button) 1))
     (activate chip)
     (wait #(chip-active? chip))
     (activate chip))))
     
