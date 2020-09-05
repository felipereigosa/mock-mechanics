
(script
 [button probe]
 [close-chip open-chip]

 (fn [part-name]
   (when (and (= part-name button)
              (= (get-value button) 1))
     (if (= (get-value probe) 1)
       (activate open-chip)
       (activate close-chip)))))
