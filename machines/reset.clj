
[collision-probe button lamp reset-chip]

(fn [part-name]
  (println! part-name))

;; [probe next-part-chip]

;; (fn [part-name]
;;   (when (and (= part-name probe)
;;              (= (get-value probe) 1))
    
;;     (dotimes [i 3]
;;       (activate next-part-chip)
;;       (wait #(chip-active? next-part-chip)))

;;     (println! "move up")
;;   ))


;; [button chip lamp]

;; (fn [part-name]
;;   (set-value lamp (get-value button))
;;   (when (= part-name button)
;;     (while (= (get-value button) 1)
;;       (activate chip)
;;       (wait #(chip-active? chip)))))
