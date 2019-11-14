
(script
 [button probe]
 [up-chip down-chip]

 (fn [part-name]
   (when (and (= part-name button)
              (= (get-value button) 1))
     (if (= (get-value probe) 1)
       (run-chip down-chip)
       (run-chip up-chip))     
   )))
