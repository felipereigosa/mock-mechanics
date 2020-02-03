
(ns temp.core)

(load "edit/move")
(load "edit/sink")
(load "edit/rotate")
(load "edit/delete")
(load "edit/scale")
(load "edit/copy")
(load "edit/translate")

(defn edit-mode-draw [world]
  (let [{:keys [image x y]} (:edit-menu world)]
    (draw-image! image x y))

  (let [box (get-in world [:edit-menu :regions
                           (:edit-subcommand world)])
        {:keys [x y w h]} box]
    (dotimes [i 3]
      (draw-rect! :black x y (- w i) (- h i)))))

(defn edit-mode-pressed [world event]
  (let [{:keys [x y]} event]
    (if-let [region (get-region-at (:edit-menu world) x y)]
      (-> world
          (assoc-in [:edit-subcommand] region)
          (assoc-in [:region-pressed] true))
      (case (:edit-subcommand world)
        :move (move-mode-pressed world event)
        :sink (sink-mode-pressed world event)
        :rotate (rotate-mode-pressed world event)
        :delete (delete-mode-pressed world event)
        :scale (scale-mode-pressed world event)
        :copy (copy-mode-pressed world event)
        :translate (translate-mode-pressed world event)
        world))))

(defn edit-mode-moved [world event]
  (if (:region-pressed world)
    world
    (case (:edit-subcommand world)
      :move (move-mode-moved world event)
      :rotate (rotate-mode-moved world event)
      :sink (sink-mode-moved world event)
      :scale (scale-mode-moved world event)
      world)))

(defn edit-mode-released [world event]
  (if (:region-pressed world)
    (dissoc-in world [:region-pressed])
    (case (:edit-subcommand world)
      :move (move-mode-released world event)
      :rotate (rotate-mode-released world event)
      :translate (translate-mode-released world event)
      :sink (sink-mode-released world event)
      :copy (copy-mode-released world event)
      :scale (scale-mode-released world event)
      world)))
