
(ns temp.core (:gen-class))

(defn enter-jumping-state [world]
  (let [block-name (get-in world [:avatar :block])]
    (assoc-in world [:parts block-name :value] 0)))

(defn get-end-block [world point]
  (->> (map (fn [block-name]
              (let [block (get-solid-block world block-name)
                    block-point (project-down world block point)]
                (if (and (not-nil? block-point)
                         (< (distance block-point point) 0.5))
                  [block-name block-point]
                  nil)))
            (:block-names world))
       (filter not-nil?)
       (first)))

(defn set-new-relative-transform [world block-name point]
  (let [block (get-solid-block world block-name)
        transform (:transform block)
        inverse-transform (get-inverse-transform transform)
        avatar (:avatar world)
        relative-position (apply-transform inverse-transform point)
        absolute-direction (vector-rotate [0 0 1] [0 1 0] (:angle avatar))
        block-rotation (get-rotation-component (:transform block))
        inverse-rotation (get-inverse-transform block-rotation)
        relative-direction (apply-transform inverse-rotation absolute-direction)]
    (-> world
        (assoc-in [:avatar :block] block-name)
        (assoc-in [:avatar :relative-position] relative-position)
        (assoc-in [:avatar :relative-direction] relative-direction))))

(defn jump-handle-direction-keys [world]
  ;; (let [acceleration (get-acceleration world)]
  ;;   (if (not (vector= acceleration [0 0 0]))
  ;;     (let [avatar (:avatar world)
  ;;           velocity (:velocity avatar)
  ;;           new-velocity (vector-add velocity acceleration)
  ;;           max-speed (:max-speed avatar)
  ;;           new-velocity (if (> (vector-length new-velocity) max-speed)
  ;;                          (vector-multiply
  ;;                           (vector-normalize new-velocity) max-speed)
  ;;                          new-velocity)]
  ;;       ;; (assoc-in world [:avatar :velocity] new-velocity)
  ;;       world
  ;;       )
  ;;     world))
  world
  )

(defn jump-handle-keys [world]
  (cond
    (avatar-key-pressed? world "k")
    (-> world
        (avatar-press-part)
        (assoc-in [:avatar :part-pressed] true))

    (and
     (avatar-key-released? world "k")
     (get-in world [:avatar :part-pressed]))
    (-> world
        (avatar-release-part)
        (assoc-in [:avatar :part-pressed] false))

    (avatar-key-on? world "u") (rotate-cameraman world -2)
    (avatar-key-on? world "o") (rotate-cameraman world 2)
    :else world))

(defn update-jumping-state [world]
  (let [avatar (:avatar world)
        v (+ (:vertical-velocity avatar) -0.016)
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
          (jump-handle-keys)
          (jump-handle-direction-keys)))))
