
(defn rotate-mode-pressed [world event]
  (let [{:keys [x y]} event]
    (if-let [collision (get-part-collision world event)]
      (let [part-name (:part-name collision)
            part (get-in world [:parts part-name])
            rotation (get-rotation-component (:transform part))
            axis (apply-transform rotation [0 1 0])
            center (get-part-position world part-name)
            point (:point collision)
            v1 (vector-subtract point center)
            v2 (vector-normalize (vector-cross-product v1 axis))]
        (-> world
            (assoc-in [:edited-part] part-name)
            (assoc-in [:edit-start-point] [x y])
            (assoc-in [:original-transform] (:transform part))
            (assoc-in [:line] [point v2])))
      world)))

(defn rotate-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [mouse-line (get-spec-line world event)
          d (line-line-closest-point (:line world) mouse-line)
          angle (* d -120)
          grain-size (if (:shift-pressed world) 45 5)
          angle (snap-value angle grain-size)
          rotation (make-transform [0 0 0] [0 1 0 angle])
          original-transform (:original-transform world)          
          transform (combine-transforms rotation original-transform)]
      (user-message! "angle = " (format "%.2f" (float angle)))
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (assoc-in [:rotation-angle] angle)))
    world))

(defn rotate-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)
          angle (:rotation-angle world)
          rotation (make-transform [0 0 0] [0 1 0 angle])
          world (set-value-0-transform world part-name)
          original-transform (get-in world [:parts part-name :transform])
          transform (combine-transforms rotation original-transform)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))
