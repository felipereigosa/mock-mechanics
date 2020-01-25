
(ns temp.core)

(defn insert-mode-draw [world]
  (let [{:keys [image x y]} (:insert-menu world)]
    (draw-image! image x y))

  (let [box (get-in world [:insert-menu :regions
                           (:insert-type world)])
        {:keys [x y w h]} box]
    (dotimes [i 3]
      (draw-rect! :black x y (- w i) (- h i 1)))))

(defn insert-part [world type color x y]
  (let [part (create-part type color (:info world))
        name (gen-keyword type)
        offset (get-part-offset part)
        spec (get-closest-spec world x y)
        parent-name (:part spec)
        parent (get-in world [:parts parent-name])
        transform (spec->transform offset spec parent)]
    (-> world
        (assoc-in [:parts name] part)
        (assoc-in [:parts name :transform] transform)
        (create-relative-transform name parent-name))))

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

(declare create-sphere)

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

(defn insert-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [region (get-region-at (:insert-menu world) x y)]
      (assoc-in world [:insert-type] region)
      (case (:insert-type world)
        :block
        (insert-part world :block :white x y)

        :cylinder
        (insert-part world :cylinder :orange x y)

        :cone
        (insert-part world :cone :green x y)

        :track
        (insert-part world :track :red x y)

        :wagon
        (insert-wagon world :yellow x y)

        :chip
        (insert-part world :chip :gray x y)

        :cpu
        (insert-part world :cpu :blue x y)

        :probe
        (insert-part world :probe :purple x y)

        :button
        (insert-part world :button :black x y)

        :sphere
        (insert-sphere world x y)))))

(defn insert-mode-moved [world event]
  (let [x (:x event)
        y (:y event)
        spec (get-closest-spec world x y)
        color (if (= (get-in world [:parts (:part spec) :type]) :track)
                :yellow
                :black)
        transform (make-transform (:position spec) (:rotation spec))]
    (update-in world [:cursor] (fn [cursor]
                                 (-> cursor
                                     (assoc-in [:transform] transform)
                                     (set-mesh-color color))))))

(defn draw-cursor! [world]
  (when (and
         (= (:mode world) :insert)
         (not= (:insert-type world) :wagon))
    (draw-mesh! world (:cursor world))))
