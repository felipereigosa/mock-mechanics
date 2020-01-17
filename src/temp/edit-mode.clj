
(ns temp.core)

(load "edit/delete")
(load "edit/move")
(load "edit/scale")
(load "edit/translate")
(load "edit/paste")
(load "edit/sink")

(defn set-object-color [world x y]
  (let [[r g b] (get-in world [:color-palette :regions
                               (:current-color world) :color])
        color (new Color r g b)]
    (if-let [part-name (get-part-at world x y)]
      (assoc-in world [:parts part-name :color] color)
      world)))

(defn rotate-part [world event]
  (if-let [part-name (get-part-at world (:x event) (:y event))]
    (let [part (get-in world [:parts part-name])
          transform (:transform part)
          rotation (make-transform [0 0 0] [0 1 0 90])
          transform (combine-transforms rotation transform)
          parent-name (get-parent-part world part-name)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (create-relative-transform part-name parent-name)))
    world))

(defn edit-mode-draw [world]
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

(defn edit-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [color-name (get-region-at (:color-palette world) x y)]
      (assoc-in world [:current-color] color-name)
      (case (:edit-subcommand world)
        :color (set-object-color world x y)
        :move (move-mode-pressed world event)
        :sink (sink-mode-pressed world event)
        :translate (translate-mode-pressed world event)
        :paste (paste-mode-pressed world event)
        :scale (scale-mode-pressed world event)
        :delete (delete-mode-pressed world event)
        :rotate (rotate-part world event)
        world))))

(defn edit-mode-moved [world event]
  (case (:edit-subcommand world)
    :move (move-mode-moved world event)
    :sink (sink-mode-moved world event)
    :translate (translate-mode-moved world event)
    :paste (paste-mode-moved world event)
    :scale (scale-mode-moved world event)
    world))

(defn edit-mode-released [world event]
  (case (:edit-subcommand world)
    :move (move-mode-released world event)
    :sink (sink-mode-released world event)
    :translate (translate-mode-released world event)
    :paste (paste-mode-released world event)
    :scale (scale-mode-released world event)
    world))
