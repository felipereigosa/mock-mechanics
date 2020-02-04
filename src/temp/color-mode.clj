
(ns temp.core)

(defn set-object-color [world x y]
  (let [[r g b] (get-in world [:color-palette :regions
                               (:current-color world) :color])
        color (new Color r g b)]
    (if-let [part-name (get-part-at world x y)]
      (let [part (get-in world [:parts part-name])]
        (if (in? (:type part) [:button :lamp])
          (assoc-in world [:parts part-name :other-color]
                    [(/ r 255.0) (/ g 255.0) (/ b 255.0) 1])
          (assoc-in world [:parts part-name :color] color)))
      world)))

(defn color-mode-draw [world]
  (let [{:keys [image x y]} (:color-palette world)]
    (draw-image! image x y))

  (let [color-box (get-in world [:color-palette
                                 :regions (:current-color world)])
        {:keys [x y w h]} color-box
        color (if (< x 230)
                :red
                :black)]
    (dotimes [i 3]
      (draw-rect! color x y (- w i) (- h i 1)))))

(defn color-mode-pressed [world event]
  (let [x (:x event)                    
        y (:y event)]
    (if-let [color-name (get-region-at (:color-palette world) x y)]
      (assoc-in world [:current-color] color-name)
      (set-object-color world x y))))
