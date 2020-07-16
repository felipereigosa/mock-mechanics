
(ns temp.core (:gen-class))

(defn sink-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [type (get-in world [:parts part-name :type])]
        (if (= type :wagon)
          (do
            (user-message! "can't sink wagon")
            world)
          (let [part (get-in world [:parts part-name])
                part-position (get-part-position world part-name)
                offset (vector-subtract part-position point)
                rotation-transform (get-rotation-component (:transform part))
                y-axis (apply-transform rotation-transform [0 1 0])]
            (-> world
                (assoc-in [:edited-part] part-name)
                (assoc-in [:sink-line] [point y-axis])
                (assoc-in [:offset] offset)))))
      world)))

(defn sink-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point (:sink-line world) mouse-line)
          grain-size 0.05
          d (* grain-size (round (/ d grain-size)))
          snapped-position (vector-add (line-get-point (:sink-line world) d)
                                       (:offset world))]
      (-> world
          (update-in [:parts part-name]
                     #(set-part-position % snapped-position))
          (assoc-in [:snapped-position] snapped-position)))
    world))

(defn sink-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)
          position (:snapped-position world)]
      (-> world
          (set-value-0-transform part-name)
          (update-in [:parts part-name]
                     #(set-part-position % position))
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))
