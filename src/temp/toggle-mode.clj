
(ns temp.core)

(defn toggle-mode-pressed [world event]
  (if (< (:x event) 120)
    (let [index (int (/ (- (:y event) 45) 40))]
      (assoc-in world [:selected-property] index))
    (if-let [part-name (get-part-at world (:x event) (:y event))]
      (let [property (nth (get-in world [:properties])
                          (:selected-property world))]
        (println! (get-in world [:parts part-name property]))
        (update-in world [:parts part-name property] not))
      world)))

(defn toggle-mode-draw [world]
  (fill-rect! :black 60 320 120 550)

  (let [properties (map keyword->str (:properties world))]
    (dotimes [i (count properties)]
      (let [y (+ 70 (* i 40))
            color (if (= i (:selected-property world))
                    :blue
                    :white)]
        (draw-rect! color 60 y 100 30)
        (draw-text! color (nth properties i) 20 (+ y 8) 20)))))
