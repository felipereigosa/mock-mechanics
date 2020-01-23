
(ns temp.core)

(load "edit/delete")
(load "edit/move")
(load "edit/scale")
(load "edit/translate")
(load "edit/paste")
(load "edit/sink")

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

(do
1
(defn edit-mode-draw [world]
  (fill-rect! :gray 100 555 100 100)
  (draw-text! :black "edit" 70 560 30)
  )
(redraw!))

(defn edit-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (case (:edit-subcommand world)
      :move (move-mode-pressed world event)
      :sink (sink-mode-pressed world event)
      :translate (translate-mode-pressed world event)
      :paste (paste-mode-pressed world event)
      :scale (scale-mode-pressed world event)
      :delete (delete-mode-pressed world event)
      :rotate (rotate-part world event)
      world)))

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
