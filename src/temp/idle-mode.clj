
(ns temp.core)

(do
1

(defn idle-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])]
        (if (= (:type part) :button)
          (-> world
              (assoc-in [:parts part-name :value] 1)
              (assoc-in [:button] part-name))
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
            world)))
          world)))

(defn idle-mode-moved [world event]
  (if (nil? (:force world))
    world
    (let [x (:x event)
          y (:y event)
          mouse-line (unproject-point world [x y])]
      (assoc-in world [:force :line] mouse-line))))

(defn idle-mode-released [world event]
  (if-let [button-name (:button world)]
    (-> world
        (assoc-in [:parts button-name :value] 0)
        (dissoc-in [:button]))
    (dissoc-in world [:force]))))

