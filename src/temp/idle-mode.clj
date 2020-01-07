
(ns temp.core)

(do
1

(defn idle-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            world (if (in? (:type part) [:button :block])
                    (-> world
                        (assoc-in [:parts part-name :value] 1)
                        (assoc-in [:pressed-part] part-name))
                    world)]
        (if-let [part-name (get-first-dof world part-name)]
          (let [part (get-in world [:parts part-name])
                transform (:transform part)
                inverse-transform (get-inverse-transform transform)
                local-point (apply-transform inverse-transform point)
                mouse-line (unproject-point world [x y])]
            (assoc-in world [:force] {:part-name part-name
                                      :velocity 0
                                      :line mouse-line
                                      :point local-point}))
          world))
      world)))

(defn idle-mode-moved [world event]
  (if (nil? (:force world))
    world
    (let [x (:x event)
          y (:y event)
          mouse-line (unproject-point world [x y])]
      (assoc-in world [:force :line] mouse-line))))

(defn idle-mode-released [world event]
  (let [world (if (nil? (:pressed-part world))
                world
                (assoc-in world [:parts (:pressed-part world) :value] 0))]
    (-> world
        (dissoc-in [:pressed-part])
        (dissoc-in [:force]))))
)

