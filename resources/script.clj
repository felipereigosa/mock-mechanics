
(let [world @temp.core/world
      set-thing! temp.core/set-thing!
      println temp.core/println
      within temp.core/within]
  (fn [pin-name]
    (when (= pin-name :button7998)
      (let [button-value (get-in world [:parts pin-name :value])
            probe-value (get-in world [:parts :probe7995 :value])]
        (when (or (nil? button-value) (= button-value 1))
          (if (= probe-value 0)
            (set-thing! [:parts :chip7997 :time] 0.0)
            (set-thing! [:parts :chip7996 :time] 0.0)
            ))))))

;; (let [probe (input 0)
;;       button (input 1)
;;       hide-chip (output 0)
;;       show-chip (output 1)
;;       ]  
;;   (fn [part]
;;     (println (:color probe))
;;     ;; (when (and (= part button)
;;     ;;            (= (get-value part) 1))
;;     ;;   (if (= (get-value probe) 0)
;;     ;;     (run hide-chip)
;;     ;;     (run show-chip)))
;;     ))

;; (do
;;   ;; (require '[temp.core :as t :refer :all])

;;   ;; (let [probe :probe7995 ;; in0
;;   ;;       chip :chip7997   ;; out0
;;   ;;       ]
    
;;     ;; (letfn [;; (get-part [part-name]
;;     ;;         ;;   (get-in @world [:parts part-name]))
;;     ;;         ;; (run-chip [chip-name]
;;     ;;         ;;   (set-thing! [:parts chip-name :time] 0.0))
;;     ;;         ]
;;       (fn [pin-name]
;;         ;; (println (:color (get-part pin-name)))

;;         ;; (run-chip chip)
;;         (println pin-name)
;;         ;; (set-thing! [:parts :chip7997 :time] 0.0)
;;         ))

;; (fn [pin-name]
;;   ;; (set-thing! [:parts :chip7996 :time] 0.0)
;;   )
