
;; (defn one-transition? [function start-time end-time]
;;   (let [pairs (map vector function (rest function))
;;         p1 (find-if (fn [[[t0 & _] [t1 & _]]]
;;                         (<= t0 start-time t1))
;;                     pairs)
;;         p2 (find-if (fn [[[t0 & _] [t1 & _]]]
;;                         (<= t0 end-time t1))
;;                       pairs)
;;         [[_ y0] [_ y1]] p1
;;         [[_ y2] [_ _]] p2]
;;     (or (vector= [y0 y1 end-time] [0 1 1])
;;         (vector= [y0 y1 y2] [0 1 1]))))

;; (if (= (:type part) :chip)
;;   (if (one-transition? function old-time time)
;;     (assoc-in world [:parts part-name :time] 0.0)
;;     world)
;;   world)


