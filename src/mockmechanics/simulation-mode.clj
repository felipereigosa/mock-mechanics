
(declare create-part-bodies)

(defn simulation-mode-entered [w]
  (while (not (:use-weld-groups @world)))
  w)

(defn simulation-mode-pressed [world event]
  (if-let [{:keys [part-name point]} (get-part-collision world event)]
    (let [part (get-in world [:parts part-name])
          world (if (in? (:type part) [:button :block :cylinder :cone :sphere])
                  (-> world
                      (assoc-in [:parts part-name :value] 1)
                      (assoc-in [:pressed-part] part-name))
                  world)]
      (if-let [part-name (get-first-dof world part-name)]
        (let [part (get-in world [:parts part-name])
              transform (:transform part)
              inverse-transform (get-inverse-transform transform)
              local-point (apply-transform inverse-transform point)]
          (assoc-in world [:mouse-force]
                    {:part-name part-name
                     :local-point local-point
                     :line (get-spec-line world event)}))
        world))
    world))

(defn simulation-mode-moved [world event]
  (if (nil? (:mouse-force world))
    world
    (assoc-in world [:mouse-force :line] (get-spec-line world event))))

(defn simulation-mode-released [world event]
  (let [world (if (nil? (:pressed-part world))
                world
                (assoc-in world [:parts (:pressed-part world) :value] 0))
        world (if (nil? (:mouse-force world))
                world
                (snap-part world (get-in world [:mouse-force :part-name])))]
    (-> world
        (dissoc-in [:pressed-part])
        (dissoc-in [:mouse-force]))))
