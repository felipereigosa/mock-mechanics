
(ns temp.core (:gen-class))

(defn enter-jumping-state [world]
  (-> world
      (assoc-in [:avatar :relative-position] nil)
      (assoc-in [:avatar :mesh :frame] 2)))

(defn get-relative-position [world point]
  (find-if identity
           (map (fn [block-name]
                  (let [block (get-solid-block world block-name)
                        block-point (project-down world block point)]
                    (if (and (not-nil? block-point)
                             (< (distance block-point point) 0.5)) ;;#######
                      (let [transform (:transform block)
                            inverse-transform (get-inverse-transform transform)
                            new-offset (apply-transform inverse-transform
                                                        block-point)]
                        {:block block-name
                         :offset new-offset})
                      nil)))
                (:block-names world))))

(defn update-jumping-state [world]
  (let [avatar (:avatar world)
        v (+ (:vertical-velocity avatar) -0.008)
        vertical-velocity (vector-multiply [0 1 0] v)        
        horizontal-velocity (:velocity avatar)
        velocity (vector-add vertical-velocity horizontal-velocity)
        new-position (vector-add (:position avatar) velocity)
        relative-position (get-relative-position world new-position)]
    (cond
      (and (< v 0.0)
           (not-nil? relative-position))
      (-> world
          (assoc-in [:avatar :relative-position] relative-position)
          (set-avatar-position)
          (change-state :running))

      ;; wall collision -----------

      :else
      (-> world
          (assoc-in [:avatar :position] new-position)
          (assoc-in [:avatar :vertical-velocity] v)
          ))))
