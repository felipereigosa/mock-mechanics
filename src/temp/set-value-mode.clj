
(ns temp.core)

(defn set-part-value [world x y]
  (if-let [part-name (get-part-at world x y)]
    (let [part (get-in world [:parts part-name])]
      (println! "set value of " part-name)
      (read-input
       world
       (fn [w text]
         (let [value (parse-float text)
               value (if (= (:type part) :wagon)
                       (/ value
                          (reduce + (:track-lengths part)))
                       value)]
           (-> w
               (assoc-in [:parts part-name :value] value)
               (prepare-tree))))))
    world))

(defn set-value-mode-draw [world]
  (draw-text! :red "set value" 10 600 20))

(defn set-value-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)
        {:keys [part-name point]} (get-part-collision world x y)
        part (get-in world [:parts part-name])
        world (assoc-in world [:press-time] (get-current-time))]
    (case (:type part)
      :wagon (let [transform (:transform part)
                   inverse-transform (get-inverse-transform transform)
                   local-point (apply-transform inverse-transform point)
                   mouse-line (unproject-point world [x y])]
               (-> world
                   (assoc-in [:force] {:part-name part-name
                                       :velocity 0
                                       :line mouse-line
                                       :point local-point})
                   (prepare-tree)))
      :track
      (-> world
          (assoc-in [:track-force] {:part-name part-name
                                    :point point
                                    :start-value (:value part)
                                    })
          (prepare-tree))
      world)))

(defn special-track-moved [world x y]
  (let [{:keys [part-name point start-value]} (:track-force world)
        key (if (:use-weld-groups world)
              :weld-groups
              :parts)
        track (get-in world [key part-name])
        transform (:transform track)
        rotation (get-rotation-component transform)
        track-direction (apply-transform rotation [0 1 0])
        plane (get-camera-plane world point)
        [p0 p1 p2] plane
        v1 (vector-subtract p1 p0)
        v2 (vector-subtract p2 p0)
        plane-normal (vector-normalize (vector-cross-product v1 v2))
        line (unproject-point world [x y])
        p2 (line-plane-intersection line plane)
        side-vector (vector-normalize
                     (vector-cross-product track-direction plane-normal))
        side-line [point side-vector]
        p3 (point-line-projection p2 side-line)
        v (vector-subtract p3 point)
        s (/ (- (vector-dot-product v side-vector)) 2)
        new-value (+ start-value s)]
    (println! "value = " new-value)
    (assoc-in world [:parts part-name :value] new-value)))

(defn set-value-mode-moved [world event]
  (let [x (:x event)
        y (:y event)]
    (cond
      (:force world)
      (let [mouse-line (unproject-point world [x y])
            part-name (get-in world [:force :part-name])
            wagon (get-in world [:parts part-name])
            new-value (:value wagon)
            loop-length (reduce + (:track-lengths wagon))
            ]
        (println! "value = " (* new-value loop-length))
        (assoc-in world [:force :line] mouse-line))

      (:track-force world)
      (special-track-moved world x y)

      :else world)))

(defn set-value-mode-released [world event]
  (let [world (-> world
                  (dissoc-in [:force])
                  (dissoc-in [:track-force]))
        delay (- (get-current-time) (:press-time world))]
    (if (< delay 200)
      (set-part-value world (:x event) (:y event))
      world)))
