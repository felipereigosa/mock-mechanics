
(ns temp.core)

(defn toggle-mode-entered [world]
  (tree-changed world))

(defn toggle-mode-exited [world]
  (tree-changed world))

(defn toggle-mode-pressed [world {:keys [x y]}]
  (let [box (:toggle-box world)]
    (if (inside-box? box x y)
      (let [start-x (- (:x box) (/ (:w box) 2))
            n (dec (count (:properties world)))
            index (within (int (/ (- x start-x) 100)) 0 n)]
        (assoc-in world [:selected-property] index))
      (if-let [part-name (get-part-at world x y)]
        (let [property (nth (get-in world [:properties])
                            (:selected-property world))]
          (-> world
              (update-in [:parts part-name property] not)
              (tree-changed)))
        world))))

(defn toggle-mode-draw [world]
  (let [layer-box (:toggle-box world)
        {:keys [x y w h]} layer-box]    
    (fill-rect! :black x y w h)
    (let [properties (map keyword->str (:properties world))]
      (dotimes [i (count properties)]
        (let [cx (+ (* i 100) 60 (- x (/ w 2)))
              color (if (= i (:selected-property world))
                      :gray
                      :dark-gray)
              text (nth properties i)
              text (if (= text ".")
                     ""
                     text)]
          (fill-rect! color cx y 98 40)
          (draw-text! :white text (- cx 40) (+ y 5) 20))))))
