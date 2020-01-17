
(script
 [block pad]
 [down up]

 (fn [part-name]
   (if (= (get-value part-name) 1)
     (activate down)
     (activate up))))
