
(script
 [b1 b2]
 [l1 l2]

 (fn [part-name]
   (cond
     (= part-name b1) (set-value l1 (get-value b1))
     (= part-name b2) (set-value l2 (get-value b2)))
   ))
