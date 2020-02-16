
(ns temp.core)

(defn translate-mode-pressed [world {:keys [x y]}]
  (if-let [collision (get-collision world x y)]
    (if (:control-pressed world)
      (assoc-in world [:selected-part] (:part-name collision))
      (if-let [selected-part (:selected-part world)]
        (let [old-parent-name (get-parent-part world selected-part)]
          (-> world
              (dissoc-in [:parts old-parent-name :children selected-part])
              (place-part-at selected-part collision)
              (move-part-pressed selected-part nil)))
        world))
    world))

(defn translate-mode-moved [world event]
  (move-part-moved world event))

(defn translate-mode-released [world event]
  (move-part-released world event))
