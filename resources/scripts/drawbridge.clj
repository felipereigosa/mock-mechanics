
(script
 [button probe]
 [up-chip down-chip track]

 (fn [part-name]
   (when (and (= part-name button)
              (= (get-value button) 1))

     (if (= (get-value probe) 0)
       (activate down-chip)
       (activate up-chip))
     )))
