
(script
 [p0 p1 p2 p3 p4]
 [track]

 (fn [part-name]
   (if (= (get-value part-name) 0)
     (set-value track (snap-value (get-value track) 0.25))
     )))
