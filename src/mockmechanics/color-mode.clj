(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

(defn get-display-plane [display]
  (let [rotation (:transform display)]
    [(apply-transform rotation [0 0.025 0])
     (apply-transform rotation [1 0.025 0])
     (apply-transform rotation [0 0.025 1])]))

(defn set-pixel! [display x y color]
  (let [mesh (:texture display)
        image (:image mesh)]
    (fill-rect image color (+ 10 (* x 20)) (+ 10 (* y 20)) 20 20)))

(defn get-pixel-coordinates [world spec]
  (let [line (get-spec-line world spec)
        display-name (get-part-at world spec)
        display (get-in world [:parts display-name])
        plane (get-display-plane display)
        [a b c] plane
        v1 (vector/subtract b a)
        v2 (vector/subtract c a)
        point (line-plane-intersection line plane)
        vp (vector/subtract point a)
        [x y] (get-affine-coordinates v1 v2 vp)
        [sx _ sz] (:scale display)
        px (int (/ (+ x (* sx 0.5)) 0.05))
        py (int (/ (+ y (* sz 0.5)) 0.05))]
    [px py]))

(defn set-display-color [world event]
  (let [display-name (get-part-at world event)
        display (get-in world [:parts display-name])
        mesh (:texture display)
        image (:image mesh)
        [px py] (get-pixel-coordinates world event)]
    (if (:shift-pressed world)
      (clear image (:current-color world))
      (set-pixel! display px py (:current-color world)))
    (update-in world [:parts display-name :texture] reset-texture)))

(defn set-object-color [world event]
  (if-let [collision (get-part-collision world event)]
    (let [part-name (:part-name collision)
          part (get-in world [:parts part-name])]
      (cond
        (in? (:type part) [:ground :chip :motherboard :speaker])
        world

        (= (:type part) :display)
        (set-display-color world event)

        :else
        (let [color (get-color (:current-color world))
              part (assoc-in part [:color] color)
              part (if (= (:type part) :lamp)
                     (assoc-in part [:dark-color] (get-dark-color color))
                     part)]
          (-> world
              (assoc-in [:parts part-name] part)
              (tree-changed)))))
    world))

(defn color-mode-draw [world]
  (let [color-pallete (:color-palette world)]
    (let [{:keys [image x y]} color-pallete]
      (draw-image! image x y))

    (let [color-box (get-in color-pallete
                            [:regions (:current-color world)])
          {:keys [x y w h]} (get-absolute-region color-box color-pallete)
          color (if (> (- (:x color-pallete) x) 140)
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
