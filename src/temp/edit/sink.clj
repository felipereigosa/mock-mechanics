
(ns temp.core (:gen-class))

(declare sink-mode-moved)

(defn sink-mode-pressed [world event]
  (if-let [{:keys [part-name point index]} (get-part-collision world event)]
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
              (assoc-in [:start-position] part-position)
              (assoc-in [:edited-part] part-name)
              (assoc-in [:sink-line] [point y-axis])
              (assoc-in [:offset] offset)
              (sink-mode-moved event)))))
    world))

(defn sink-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [mouse-line (get-spec-line world event)
          d (line-line-closest-point (:sink-line world) mouse-line)
          grain-size (if (:shift-pressed world)
                       0.25
                       0.05)
          d (* grain-size (round (/ d grain-size)))
          snapped-position (vector-add (line-get-point (:sink-line world) d)
                                       (:offset world))
          part (get-in world [:parts part-name])
          parent-name (get-parent-part world part-name)
          parent (get-in world [:parts parent-name])
          vy (apply-rotation (:transform part) [0 1 0])
          plane (get-block-plane parent vy)
          h (- (point-plane-distance snapped-position plane)
               (get-part-offset part))]
      (user-message! "height = " (format "%.2f" h))
      (-> world
          (update-in [:parts part-name]
                     #(set-part-position % snapped-position))
          (assoc-in [:snapped-position] snapped-position)))
    world))

(defn move-other-gear [world part-name]
  (if (= (get-in world [:parts part-name :type]) :gear)
    (let [[a b] (find-if #(in? part-name %) (keys (:gears world)))
          other-gear-name (if (= part-name a) b a)
          gear (get-in world [:parts part-name])
          position (get-transform-position (:transform gear))
          offset (vector-subtract position (:start-position world))]
      (move-part world other-gear-name offset))
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
          (move-other-gear part-name)
          (dissoc-in [:edited-part])))
    world))
