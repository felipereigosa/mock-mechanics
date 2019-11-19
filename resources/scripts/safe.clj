
(script
 [probe button]
 [far near bolt]

 (fn [part-name]
   (if (and (= part-name button)
            (= (get-value button) 1))
     (let [;; n (case
           ;;       (= (get-value probe-0) 1) 0
           ;;       (= (get-value probe-1) 1) 1
           ;;       (= (get-value probe-2) 1) 2
           ;;       (= (get-value probe-3) 1) 3)
           n 1
           ]

       (dotimes [i n]
         (activate far)
         (wait #(chip-active? far))
         )

       (if (= (get-value probe) 1)
         (do
           (println! "wrong value")
           (sleep 100))
         (do
           (activate bolt)
           (wait #(chip-active? bolt))))

       (dotimes [i n]
         (activate near)
         (wait #(chip-active? near)))
       ))))
