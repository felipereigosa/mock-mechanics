
(script
 [button probe]
 [out-chip in-chip]

 (fn [part-name]
   ;; (when (and (= part-name button)
   ;;            (= (get-value button) 1))
   ;;   (if (= (get-value probe) 1)
   ;;     (activate out-chip)
   ;;     (activate in-chip))
   ;;   )
   (println! part-name)
   ))
