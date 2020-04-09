
(ns temp.core)

(defn set-object-color [world x y]
  (let [[r g b] (get-in world [:color-palette :regions
                               (:current-color world) :color])
        color (new Color r g b)]
    (if-let [part-name (get-part-at world x y)]
      (let [part (get-in world [:parts part-name])
            part (assoc-in part [:color] color)
            part (if (= (:type part) :lamp)
                   (assoc-in part [:dark-color] (get-dark-color color))
                   part)]
        (assoc-in world [:parts part-name] part))
      world)))

(defn color-mode-draw [world]
  (let [color-pallete (:color-palette world)]
    (let [{:keys [image x y]} color-pallete]
      (draw-image! image x y))

    (let [color-box (get-in color-pallete
                            [:regions (:current-color world)])
          {:keys [x y w h]} (get-absolute-region color-box color-pallete)
          color (if (< x 230)
                  :red
                  :black)]
      (dotimes [i 3]
        (draw-rect! color x y (- w i) (- h i 1))))))

(defn color-mode-released [world event]
  (let [x (:x event)                    
        y (:y event)]
    (if-let [color-name (get-region-at (:color-palette world) x y)]
      (assoc-in world [:current-color] color-name)
      (set-object-color world x y))))
