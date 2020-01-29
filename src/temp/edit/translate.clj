
(ns temp.core)

(defn translate-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [part-name (get-part-at world x y)]
      (assoc-in world [:edited-part] part-name)
      world)))

(defn translate-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [x (:x event)
          y (:y event)
          part (get-in world [:parts part-name])
          anchor (get-anchor-point world x y)
          new-parent-name (:part anchor)
          old-parent-name (get-parent-part world part-name)
          offset (get-part-offset part)
          parent (get-in world [:parts new-parent-name])
          transform (anchor->transform offset anchor parent)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (dissoc-in [:parts old-parent-name
                      :children part-name])
          (dissoc-in [:edited-part])
          (create-relative-transform part-name new-parent-name)))
    world))
