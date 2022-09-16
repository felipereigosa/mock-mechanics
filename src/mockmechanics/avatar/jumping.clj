(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

(defn enter-jumping-state [world]
  (let [block-name (get-in world [:avatar :block])]
    (-> world
        (assoc-in [:parts block-name :value] 0)
        (assoc-in [:avatar :start-jump-height]
                  (get-in world [:avatar :position 1])))))

(defn set-new-relative-transform [world block-name point]
  (let [block (get-solid-block world block-name)
        transform (:transform block)
        inverse-transform (get-inverse-transform transform)
        avatar (:avatar world)
        relative-position (apply-transform inverse-transform point)
        absolute-direction (vector/rotate [0 0 1] [0 1 0] (:angle avatar))
        block-rotation (get-rotation-component (:transform block))
        inverse-rotation (get-inverse-transform block-rotation)
        relative-direction (apply-transform inverse-rotation absolute-direction)]
    (-> world
        (assoc-in [:avatar :block] block-name)
        (assoc-in [:avatar :relative-position] relative-position)
        (assoc-in [:avatar :relative-direction] relative-direction))))

(defn jump-handle-direction-keys [world]
  (let [acceleration (get-acceleration world)]
    (if (not (vector/equal? acceleration [0 0 0]))
      (let [avatar (:avatar world)
            velocity (:velocity avatar)
            acceleration (vector/multiply acceleration 0.0002)
            new-velocity (vector/add velocity acceleration)
            max-speed (:max-speed avatar)
            new-velocity (if (> (vector/length new-velocity) max-speed)
                           (vector/multiply
                             (vector/normalize new-velocity) max-speed)
                           new-velocity)
            angle (vector/angle [0 0 1] acceleration [0 1 0])]
        (-> world
            (assoc-in [:avatar :velocity] new-velocity)
            (assoc-in [:avatar :angle] angle)))
      world)))

(defn jump-handle-keys [world]
  (cond
    (avatar-key-pressed? world "j")
    (if-let [normal (:wall-normal world)]
      (-> world
          (assoc-in [:avatar :vertical-velocity] 0.2)
          (assoc-in [:avatar :velocity] (vector/multiply normal 0.1)))
      world)

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

    (avatar-key-on? world "u") (rotate-cameraman world -1)
    (avatar-key-on? world "o") (rotate-cameraman world 1)
    :else world))

(defn get-end-block [world point]
  (let [avatar (:avatar world)
        [_ _ block-point block-name] (:down-collision avatar)]
    (if (and (not-nil? block-point)
             (< (vector/distance block-point point) 0.3))
      [block-name block-point]
      nil)))

(defn update-jumping-state [world]
  (let [avatar (:avatar world)
        v (+ (:vertical-velocity avatar) -0.01)
        vertical-velocity (vector/multiply [0 1 0] v)
        horizontal-velocity (:velocity avatar)
        velocity (vector/add vertical-velocity horizontal-velocity)
        new-position (vector/add (:position avatar) velocity)
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
          (jump-handle-direction-keys)
          (handle-collisions)))))
