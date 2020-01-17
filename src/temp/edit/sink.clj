
(ns temp.core)

(defn sink-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            part-position (get-part-position world part-name)
            offset (vector-subtract part-position point)
            rotation-transform (get-rotation-component (:transform part))
            y-axis (apply-transform rotation-transform [0 1 0])]
        (-> world
            (assoc-in [:edited-part] part-name)
            (create-weld-groups)
            (assoc-in [:sink-line] [point y-axis])
            (assoc-in [:offset] offset)))
      world)))

(defn sink-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point (:sink-line world) mouse-line)
          grain-size 0.05
          d (* grain-size (round (/ d grain-size)))
          snapped-position (vector-add (line-get-point (:sink-line world) d)
                                       (:offset world))
          part (get-in world [:parts part-name])
          rotation (get-transform-rotation (:transform part))
          transform (make-transform snapped-position rotation)]
      (assoc-in world [:parts part-name :transform] transform))
    world))

(defn sink-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)]
      (-> world
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))
