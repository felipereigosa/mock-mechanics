
(ns temp.core)

(defn move-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            v1 (apply-rotation (:transform part) [1 0 0])
            v2 (apply-rotation (:transform part) [0 0 1])
            plane [point (vector-add point v1) (vector-add point v2)]
            part-position (get-part-position world part-name)
            offset (vector-subtract part-position point)]
        (-> world
            (assoc-in [:edited-part] part-name)
            (create-weld-groups)
            (assoc-in [:plane] plane)
            (assoc-in [:offset] offset)
            (assoc-in [:original-position] part-position)
        ))
      world)))

(defn move-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [line (unproject-point world [(:x event) (:y event)])
          touch-point (line-plane-intersection line (:plane world))
          position (vector-add touch-point (:offset world))
          [a b c] (:plane world)
          v1 (vector-subtract b a)
          v2 (vector-subtract c a)
          origin (:original-position world)
          s (point-line-coordinate position [origin v1])
          t (point-line-coordinate position [origin v2])
          grain-size (if (:shift-pressed world)
                       0.2
                       0.05)
          s (* grain-size (round (/ s grain-size)))
          t (* grain-size (round (/ t grain-size)))
          snapped-position (reduce vector-add [origin
                                               (vector-multiply v1 s)
                                               (vector-multiply v2 t)])
          part (get-in world [:parts part-name])
          rotation (get-transform-rotation (:transform part))
          transform (make-transform snapped-position rotation)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (assoc-in [:snapped-position] snapped-position)))
    world))

(defn move-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)
          world (set-value-0-transform world part-name)
          part (get-in world [:parts part-name])
          rotation (get-transform-rotation (:transform part))
          snapped-position (:snapped-position world)
          transform (make-transform snapped-position rotation)
          ]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))
