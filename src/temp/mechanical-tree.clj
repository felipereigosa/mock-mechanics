
(declare compute-subtree-transforms)
  
(defn compute-children-transforms [world part-name transform key]
  (reduce (fn [w [child-name relative-transform]]
            (let [new-transform (combine-transforms
                                 relative-transform transform)]
              (compute-subtree-transforms w child-name new-transform key)))
          world
          (get-in world [key part-name :children])))

(defn compute-translated-loop-fn [loop-fn]
  (let [offset (second (first loop-fn))]
    (map (fn [[t p]]
           [t (vector-subtract p offset)])
         loop-fn)))

(defn block-compute-subtree-transforms [world name transform key]
  (-> world
      (assoc-in [key name :transform] transform)
      (compute-children-transforms name transform key)))

(defn track-compute-subtree-transforms [world name transform key]
  (let [track (get-in world [:parts name])
        angle (map-between-ranges (:value track) 0.0 1.0 0.0 360.0)
        angle-transform (make-transform [0 0 0] [0 1 0 angle])
        transform (combine-transforms angle-transform transform)
        world (assoc-in world [key name :transform] transform)]
    (reduce (fn [w [child-name relative-transform]]
              (let [new-transform (combine-transforms
                                   relative-transform transform)]
                (compute-subtree-transforms w child-name new-transform key)))
            world
            (get-in world [key name :children]))))

(defn wagon-compute-subtree-transforms [world name transform key]
  (let [wagon (get-in world [:parts name])
        rotation (get-transform-rotation transform)
        loop-fn (if (= key :weld-groups)
                  (compute-translated-loop-fn (:loop-fn wagon))
                  (:loop-fn wagon))
        loop-fn (map (fn [[t v]]
                       [t (apply-transform transform v)])
                     loop-fn)
        value (within (:value wagon) 0.0 1.0)
        position (get-function-value loop-fn value vector-interpolate)
        transform (make-transform position rotation)]
    (-> world
        (assoc-in [key name :transform] transform)
        (compute-children-transforms name transform key))))

(defn compute-subtree-transforms [world name transform key]
  (case (get-in world [:parts name :type])
    :track (track-compute-subtree-transforms world name transform key)
    :wagon (wagon-compute-subtree-transforms world name transform key)
    (block-compute-subtree-transforms world name transform key)))

(defn compute-transforms [world key]
  (let [ground (get-in world [:parts :ground-part])
        transform (:transform ground)]
    (compute-subtree-transforms world :ground-part transform key)))
