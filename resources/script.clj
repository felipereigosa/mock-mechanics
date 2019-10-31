
(script
 [button probe]
 [out-chip in-chip]

 (fn [part-name]
   (when (and (= part-name button)
              (= (get-value button) 1))
     (if (= (get-value probe) 1)
       (run-chip out-chip)
       (run-chip in-chip))
     )))
