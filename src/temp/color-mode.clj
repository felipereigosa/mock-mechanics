
(ns temp.core (:gen-class))


;; (do
;; 1

;; (clear-output!)
;; (let [world @world
;;       display-name :display10686
;;       display (get-in world [:parts display-name])
;;       mesh (:texture display)
;;       image (:image mesh)
;;       ]
;;   (clear image :dark-gray)
;;   (draw-pixel image :red 0 0)
;;   (draw-pixel image :red 1 0)
;;   (draw-pixel image :red 2 0)
;;   (draw-pixel image :red 3 1)
;;   (gl-thread (reset-texture mesh))
;;   ))

(defn set-pixel [display x y color]
  display)

(defn set-display-color [world event]
  (let [line (unproject-point world [(:x event) (:y event)])
        display-name (get-part-at world event)
        display (get-in world [:parts display-name])
        ;; transform (:transform display)
        ;; normal (apply-rotation transform [0 1 0])
        plane (get-track-plane display)
        [a b c] plane

        v1 (vector-subtract b a)
        v2 (vector-subtract c a)
        point (line-plane-intersection line plane)
        vp (vector-subtract point a)
        [x y] (get-affine-coordinates v1 v2 vp)

        px (+ 5 (round (/ x 0.05))) ;;############
        py (+ 2 (round (/ y 0.05)))

        mesh (:texture display)
        image (:image mesh)
        ]
    (draw-pixel image (:current-color world) px py)
    (gl-thread (reset-texture mesh))
    world
    ))

(defn set-object-color [world event]
  (let [[r g b] (get-in world [:color-palette :regions
                               (:current-color world) :color])
        color (new Color r g b)]
    (if-let [collision (get-part-collision world event)]
      (let [part-name (:part-name collision)
            part (get-in world [:parts part-name])]
        (if (= (:type part) :display)
          (set-display-color world event)
          (let [part (assoc-in part [:color] color)
                part (if (= (:type part) :lamp)
                       (assoc-in part [:dark-color] (get-dark-color color))
                       part)]
            (-> world
                (assoc-in [:parts part-name] part)
                (tree-changed)))))
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
      (set-object-color world event))))
