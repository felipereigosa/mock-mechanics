
(ns temp.core)

(declare create-part-bodies)

(defn simulation-mode-entered [w]
  (while (not (:use-weld-groups @world)))
  w)

(defn simulation-mode-pressed [world {:keys [x y]}]
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
    world))

(defn simulation-mode-moved [world event]
  (if (nil? (:force world))
    world
    (let [x (:x event)
          y (:y event)
          mouse-line (unproject-point world [x y])]
      (assoc-in world [:force :line] mouse-line))))

(defn simulation-mode-released [world event]
  (let [world (if (nil? (:pressed-part world))
                world
                (assoc-in world [:parts (:pressed-part world) :value] 0))
        world (if (nil? (:force world))
                world
                (snap-part world (get-in world [:force :part-name])))]
    (-> world
        (dissoc-in [:pressed-part])
        (dissoc-in [:force]))))
