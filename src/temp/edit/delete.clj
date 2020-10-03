
(ns temp.core (:gen-class))

(declare get-sphere-at)
(declare prune-connections)

(defn forget-part [world parent-name part-name]
  (-> world
      (dissoc-in [:parts parent-name :children part-name])
      (dissoc-in [:parts parent-name :pins part-name])
      (update-in [:parts parent-name] prune-connections)
      (dissoc-in [:parts parent-name :functions part-name])))

(defn unselect-part [world part-name]
  (cond
    (= part-name (:selected-chip world))
    (dissoc-in world [:selected-chip])

    (= part-name (:selected-motherboard world))
    (dissoc-in world [:selected-motherboard])

    :else world))

(defn delete-part [world part-name]
  (let [part (get-in world [:parts part-name])
        world (reduce (fn [w child-name]
                        (delete-part w child-name))
                      world
                      (keys (:children part)))
        world (reduce #(forget-part %1 %2 part-name)
                      world
                      (keys (:parts world)))]
    (-> world
        (unselect-part part-name)
        (dissoc-in [:parts part-name]))))

(defn delete-all-parts [world]
  (reduce (fn [w part-name]
            (delete-part w part-name))
          world
          (keys (get-in world [:parts :ground-part :children]))))

(defn delete-mode-released [world event]
  (if-let [part-name (get-part-at world event)]
    (delete-part world part-name)
    world))
