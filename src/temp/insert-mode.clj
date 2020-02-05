
(ns temp.core)

(defn insert-mode-draw [world]
  (fill-rect! (make-color 70 70 70) 330 580 800 150)

  (let [{:keys [image x y]} (:insert-menu world)]
    (draw-image! image (+ x (:insert-offset world)) y))

  (let [box (get-in world [:insert-menu :regions
                           (:insert-type world)])
        {:keys [x y w h]} box]
    (dotimes [i 3]
      (draw-rect! :black (+ x (:insert-offset world)) y (- w i) (- h i 1)))))

(defn insert-part [world type color x y]
  (if-let [anchor (get-anchor-point world x y)]
    (let [part (create-part type color (:info world))
          name (gen-keyword type)
          offset (get-part-offset part)
          parent-name (:part anchor)
          parent (get-in world [:parts parent-name])
          transform (anchor->transform offset anchor parent)]
      (-> world
          (assoc-in [:parts name] part)
          (assoc-in [:parts name :transform] transform)
          (create-relative-transform name parent-name)))
    world))

(declare create-sphere)
(declare set-wagon-loop)

(defn insert-wagon [world color x y]
  (let [part-name (get-part-at world x y)]
    (if (and (not-nil? part-name)
             (= (get-in world [:parts part-name :type]) :track))
      (let [part (create-part :wagon color (:info world))
            name (gen-keyword :wagon)
            transform (get-in world [:parts part-name :transform])]
        (-> world
            (assoc-in [:parts name] part)
            (assoc-in [:parts name :transform] transform)
            (assoc-in [:parts part-name :children name]
                      (make-transform [0 0 0] [1 0 0 0]))
            (set-wagon-loop name part-name)))
      world)))

(defn insert-sphere [world x y]
  (if-let [collision (get-part-collision world x y)]
    (let [normal (get-collision-normal world collision)
          offset (vector-multiply (vector-normalize normal)
                                  (:sphere-radius world))
          position (vector-add (:point collision) offset)]
      (create-sphere world position))
    (let [line (unproject-point world [x y])
          ground-plane [[0 0 0] [1 0 0] [0 0 1]]
          offset [0 (:sphere-radius world) 0]
          point (line-plane-intersection line ground-plane)
          position (vector-add point offset)]
      (create-sphere world position))))

(do
1

(defn insert-mode-pressed [world event]
  (if (> (:y event) 500)
    (-> world
        (assoc-in [:start-offset] (:insert-offset world))
        (assoc-in [:start-point] (:x event))
        (assoc-in [:press-time] (get-current-time)))        
    world))

(defn insert-mode-moved [world event]
  (if (nil? (:start-point world))
    world
    (let [dx (- (:x event) (:start-point world))
          new-offset (within (+ (:start-offset world) dx)
                             (:insert-min-offset world) 0)
          world (assoc-in world [:insert-offset] new-offset)]
      (redraw!)
      world)))

(defn insert-mode-released [world event]
  (let [x (:x event)
        y (:y event)
        elapsed (- (get-current-time) (:press-time world))
        world (dissoc-in world [:start-point])]
    (if (< elapsed 200)
      (let [world (assoc-in world [:insert-offset] (:start-offset world))]
        (if (> (:y event) 500)
          (if-let [region (get-region-at (:insert-menu world)
                                         (- x (:insert-offset world)) y)]
            (assoc-in world [:insert-type] region)
            world)
          (let [type (:insert-type world)
                color (get-in world [:info type :color])]
            (case (:insert-type world)
              :wagon
              (insert-wagon world color x y)

              :sphere
              (insert-sphere world x y)
              
              (insert-part world type color x y)))))
      world)))
)
