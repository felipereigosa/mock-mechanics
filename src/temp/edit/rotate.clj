
(ns temp.core)

(defn rotate-mode-pressed [world event]
  (let [{:keys [x y]} event]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])]
        (-> world
            (assoc-in [:edited-part] part-name)
            (assoc-in [:start-point] [x y])
            (assoc-in [:original-transform] (:transform part))
            (create-weld-groups)))
      world)))

(defn rotate-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [{:keys [x y]} event
          d (distance [x y] (:start-point world))
          angle (snap-value (map-between-ranges d 0 250 0 250) 10)
          part (get-in world [:parts part-name])
          rotation (make-transform [0 0 0] [0 1 0 angle])
          original-transform (:original-transform world)          
          transform (combine-transforms rotation original-transform)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)))
    world))

(defn rotate-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)]
      (-> world
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))
