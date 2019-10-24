
(let [world @temp.core/world
      set-thing! temp.core/set-thing!
      println temp.core/println
      within temp.core/within
      track-name :track7405]

  (fn [pin-name]
    (let [value (get-in world [:parts pin-name :value])]
      (when (or (nil? value) (= value 1))
        (dotimes [i 101]
          (set-thing! [:parts track-name :value]
                      (within (/ i 101.0) 0 1))
          (temp.core/sleep 20))
        ))))

;; (let [track ...
;;       ]
;;   (fn [pin-name]
;;     (let [value (:value track)]
;;       (when (or (nil? value) (= value 1))
;;         (dotimes [i 101]
;;           (set-value track (within (/ i 101.0) 0 1))
;;           (sleep 20))))))
      


