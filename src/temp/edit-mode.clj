
(ns temp.core)

(load "edit/move")
(load "edit/sink")
(load "edit/rotate")
(load "edit/delete")
(load "edit/scale")
(load "edit/copy")
(load "edit/translate")

(defn edit-mode-entered [world]
  (dissoc-in world [:selected-part]))

(defn edit-mode-draw [world]
  (let [edit-menu (:edit-menu world)]
    (let [{:keys [image x y w h]} edit-menu]
      (fill-rect! (make-color 70 70 70) x y (+ w 30) (+ h 20))
      (draw-image! image x y))
    
    (let [box (get-in edit-menu [:regions (:edit-subcommand world)])
          {:keys [x y w h]} (get-absolute-region box edit-menu)]
      (dotimes [i 3]
        (draw-rect! :black x y (- w i) (- h i))))))

(defn edit-mode-pressed [world event]
  (let [{:keys [x y]} event]
    (if-let [region (get-region-at (:edit-menu world) x y)]
      (-> world
          (assoc-in [:edit-subcommand] region)
          (show-hint :edit region)
          (assoc-in [:region-pressed] true))
      (let [world (case (:edit-subcommand world)
                    :move (move-mode-pressed world event)
                    :sink (sink-mode-pressed world event)
                    :rotate (rotate-mode-pressed world event)
                    :scale (scale-mode-pressed world event)
                    :copy (copy-mode-pressed world event)
                    :translate (translate-mode-pressed world event)
                    world)]
        (tree-will-change world)))))

(defn edit-children [world]
  (if-let [part-name (:edited-part world)]
    (let [transform (get-in world [:parts part-name :transform])]
      (compute-children-transforms world part-name transform :parts))
    world))

(defn edit-mode-moved [world event]
  (if (:region-pressed world)
    world
    (let [world (case (:edit-subcommand world)
                  :move (move-mode-moved world event)
                  :rotate (rotate-mode-moved world event)
                  :translate (translate-mode-moved world event)
                  :sink (sink-mode-moved world event)
                  :copy (copy-mode-moved world event)
                  :scale (scale-mode-moved world event)
                  world)]
      (edit-children world))))    

(defn edit-mode-released [world event]
  (if (:region-pressed world)
    (dissoc-in world [:region-pressed])
    (let [world (case (:edit-subcommand world)
                  :move (move-mode-released world event)
                  :rotate (rotate-mode-released world event)
                  :translate (translate-mode-released world event)
                  :sink (sink-mode-released world event)
                  :copy (copy-mode-released world event)
                  :scale (scale-mode-released world event)
                  :delete (delete-mode-released world event)              
                  world)]
      (if (and (:control-pressed world)
               (in? (:edit-subcommand world) [:copy :translate]))
        world
        (tree-changed world)))))
