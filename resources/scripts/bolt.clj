
(script
 [bolt-probe button p1 p2 p3]
 [open-bolt close-bolt]

 (fn [part-name]
   (if (and
        (= part-name button)
        (= (get-value button) 1))
     (if (= (get-value bolt-probe) 1)
       (activate close-bolt)
       (if (and
            (= (get-value p1) 1)
            (= (get-value p2) 1)
            (= (get-value p3) 1))
         (activate open-bolt))))))   
