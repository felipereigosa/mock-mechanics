
(script
 [block]
 []

 (fn [part-name]
   (if (and (= part-name block)
            (= (get-value block) 1))
     (println! "block pressed"))))
