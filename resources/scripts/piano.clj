
(script
 [block]
 [down up]

 (fn [part-name]
   (if (and (= part-name block)
            (= (get-value block) 1))
     (activate down)
     (activate up))
   ))
