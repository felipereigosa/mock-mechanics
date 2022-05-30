
(defn avatar-mode-draw-3d [world]
  (let [avatar (:avatar world)
        state (:state avatar)
        position (vector-subtract (:position avatar) [0 0.3 0])
        angle (:angle avatar)
        avatar-position (:position avatar)
        tilt-rotation (if (= state :jumping)
                        (make-transform [0 0 0] [1 0 0 0])
                        (get-rotation-component
                         (get-shadow-transform world avatar-position)))
        angle-rotation (make-transform [0 0 0] [0 1 0 (+ angle 90)])
        rotation-transform (combine-transforms angle-rotation tilt-rotation)
        position-transform (make-transform position [1 0 0 0])
        transform (combine-transforms rotation-transform position-transform)
        mesh (:avatar-mesh world)
        mesh (assoc-in mesh [:transform] transform)]
    (draw-mesh! world mesh)
    (draw-shadow! world)
    (draw-rope! world)))

(defn update-jumping-pose [world]
  (let [avatar (:avatar world)
        v (:vertical-velocity avatar)
        height (second (:position avatar))
        s (:start-jump-height avatar)]
    (assoc-in world [:avatar-mesh :index]
              (within (map-between-ranges height (- s 0.6) (+ s 1.05) 80 109)
                      80 109))))

(defn update-jumping-state [world]
  (let [avatar (:avatar world)
        v (+ (:vertical-velocity avatar) -0.01)
        vertical-velocity (vector-multiply [0 1 0] v)        
        horizontal-velocity (:velocity avatar)
        velocity (vector-add vertical-velocity horizontal-velocity)
        new-position (vector-add (:position avatar) velocity)
        [end-block end-point] (get-end-block world new-position)]
    (cond
      (and (< v 0.0) end-block)
      (-> world
          (set-new-relative-transform end-block end-point)
          (compute-avatar-transform)
          (change-state :running))

      (< (nth new-position 1) -5)
      (reset-avatar world)

      :else
      (-> world
          (assoc-in [:avatar :position] new-position)
          (assoc-in [:avatar :vertical-velocity] v)
          (update-jumping-pose)
          (jump-handle-keys)
          (jump-handle-direction-keys)          
          (handle-collisions)))))
