
(script
 []
 [far near]

 (fn [part-name]
   ;; (if (= part-name button)
     (let [;; n (case
           ;;       (= (get-value probe-0) 0) 0
           ;;       (= (get-value probe-1) 1) 1
           ;;       (= (get-value probe-2) 2) 2
           ;;       (= (get-value probe-3) 3) 3)
           n 1
           ]

       ;; (dotimes [i n]
       ;;   (run step-chip))

       ;; (when (= (get-value test-probe) 0)
       ;;   (run bolt-chip))
       
       ;; (run reset-chip)         

       ;; (when (= (get-value unlocked-probe) 0)
       ;;   (run open-door))
       (dotimes [i n]
         (activate far)
         (wait #(chip-active? far))
         )

       (sleep 1000)

       

       (dotimes [i n]
         (activate near)
         (wait #(chip-active? near)))
       )))
