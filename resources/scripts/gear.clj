
(script
 [button]
 [chip]

 (fn [part-name]
   (if (and
        (= part-name button)
        (= (get-value button) 1))
     (activate chip))))
