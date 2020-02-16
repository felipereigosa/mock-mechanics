
(script
 [button p1 p2 p3]
 [open-bolt]

 (fn [part-name]
   (if (and
        (= part-name button)
        (= (get-value button) 0)
        (= (get-value p1) 1)
        (= (get-value p2) 1)
        (= (get-value p3) 1))
     (activate open-bolt))))
     
