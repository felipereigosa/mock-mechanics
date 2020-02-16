
(script
 [probe]
 [green red]

 (fn [part-name]
   (if (= (get-value probe) 1)
     (do
       (set-value green 1)
       (set-value red 0))
     (do
       (set-value green 0)
       (set-value red 1)))))
       

     
