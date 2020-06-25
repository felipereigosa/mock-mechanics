
(ns temp.core)

(declare move-part-pressed)
(declare move-part-moved)
(declare move-part-released)
(declare set-wagon-loop)

(defn add-mode-draw [world]
  (let [add-menu (:add-menu world)]
    (let [{:keys [image x y w h]} add-menu]
      (fill-rect! (make-color 70 70 70) x y (+ w 30) (+ h 20))
      (draw-image! image x y))

    (let [box (get-in add-menu [:regions (:add-type world)])
          {:keys [x y w h]} (get-absolute-region box add-menu)]
      (dotimes [i 3]
        (draw-rect! :black x y (- w i) (- h i 1))))))

(defn add-wagon [world color x y]
  (let [part-name (get-part-at world x y)]
    (if (and (not-nil? part-name)
             (= (get-in world [:parts part-name :type]) :track))
      (let [layer (apply min (:visible-layers world))
            part (create-part :wagon color layer (:info world))
            name (gen-keyword :wagon)
            transform (get-in world [:parts part-name :transform])]
        (-> world
            (assoc-in [:parts name] part)
            (assoc-in [:parts name :transform] transform)
            (assoc-in [:parts part-name :children name]
                      (make-transform [0 0 0] [1 0 0 0]))
            (set-wagon-loop name part-name)))
      world)))

(defn get-ground-anchor [world part collision]
  (let [offset [0 (get-part-offset part) 0]
        position (vector-add offset (:point collision))]
    (make-transform position [1 0 0 0])))

(defn get-block-anchor [world part collision]
  (let [parent-name (:part-name collision)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        parent-rotation (get-rotation-component parent-transform)
        local-normal (vector-normalize (get-collision-normal world collision))
        global-normal (apply-transform parent-rotation local-normal)
        offset (get-part-offset part)
        position (vector-add (:point collision)
                             (vector-multiply global-normal offset))
        normal-rotation (make-transform [0 0 0]
                                        (quaternion-from-normal local-normal))
        rotation (get-transform-rotation
                  (combine-transforms normal-rotation parent-rotation))]
    (make-transform position rotation)))

(defn get-track-anchor [world part collision]
  (let [parent-name (:part-name collision)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        parent-rotation (get-rotation-component parent-transform)
        rotation (get-transform-rotation parent-transform)
        position (apply-transform parent-transform
                                  [0 (get-part-offset part) 0])]
    (make-transform position rotation)))

(defn get-cylinder-anchor [world part collision]
  (let [normal (vector-normalize (get-collision-normal world collision))]
    (if (or (vector= normal [0 1 0])
            (vector= normal [0 -1 0]))
      (get-block-anchor world part collision)
      nil)))

(defn get-track-head-anchor [world collision]
  (let [index (first (:collision collision))
        mesh (:track-head-model world)
        triangles (partition 3 (partition 3 (:vertices mesh)))
        [a b c] (nth triangles index)
        v1 (vector-subtract b a)
        v2 (vector-subtract c a)
        normal (vector-normalize (vector-cross-product v1 v2))

        transform (get-in world [:parts (:track-head world) :transform])
        position (apply-transform transform normal)
        normal-rotation (make-transform [0 0 0]
                                        (quaternion-from-normal normal))
        rotation (get-rotation-component transform)
        final-rotation (combine-transforms normal-rotation rotation)
        rotation (get-transform-rotation final-rotation)]
    (make-transform position rotation)))

(defn place-part-at [world part-name collision]
  (let [parent-name (:part-name collision)
        parent (get-in world [:parts parent-name])
        part (get-in world [:parts part-name])
        transform (if (:track-head collision)
                    (get-track-head-anchor world collision)
                    (case (:type parent)
                      :ground (get-ground-anchor world part collision)
                      :block (get-block-anchor world part collision)
                      :wagon (get-block-anchor world part collision)
                      :track (get-track-anchor world part collision)
                      :cylinder (get-cylinder-anchor world part collision)
                      (make-transform [0 0 0] [1 0 0 0])))]
    (-> world
        (assoc-in [:parts part-name :transform] transform)
        (create-relative-transform part-name parent-name))))

(defn can-place-part-at? [world collision]
  (let [target (get-in world [:parts (:part-name collision)])]
    (cond
      (in? (:type target)
           [:block :wagon :track :ground])
      true
      (= (:type target) :cylinder)
      (let [normal (vector-normalize
                    (get-collision-normal world collision))]
        (if (or (vector= normal [0 1 0])
                (vector= normal [0 -1 0]))
          true
          (user-message! "can't place on the side of cylinder")))
      :else
      (user-message! "can't place part on" (no-colon (:type target))))))

(defn add-mode-pressed [world event]
  (let [{:keys [x y]} event]
    (if (inside-box? (:add-menu world) x y)
      (if-let [region (get-region-at (:add-menu world) x y)]
        (-> world
            (assoc-in [:add-type] region)
            (show-hint :add region))
        world)
      (let [type (:add-type world)
            color (get-in world [:info type :color])
            world (tree-will-change world)]
        (case (:add-type world)
          :wagon
          (add-wagon world color x y)

          (let [layer (apply min (:visible-layers world))
                part (create-part type color layer (:info world))
                part-name (gen-keyword type)
                collision (get-collision world x y)
                parent (get-in world [:parts (:part-name collision)])
                world (if (can-place-part-at? world collision)
                        (-> world
                            (assoc-in [:parts part-name] part)
                            (place-part-at part-name collision))
                        world)]
            (if (= (:type parent) :track)
              world
              (-> world
                  (move-part-pressed part-name nil)
                  (move-part-moved event :grain 0.25)))))))))

(defn set-track-head [world x y]
  (let [line (unproject-point world [x y])
        track-names (get-parts-with-type (:parts world) :track)
        track-distances (remove-nil
                         (map (fn [track-name]
                                (let [track (get-in world [:parts track-name])
                                      p (get-transform-position (:transform track))
                                      d (point-line-distance p line)]
                                  (if (< d 0.25)
                                    [track-name d]
                                    nil)))                                 
                              track-names))
        track-name (first (first (sort-by second track-distances)))]
    (assoc-in world [:track-head] track-name)))

(defn add-mode-moved [world event]
  (if (and (= (:add-type world) :track)
           (nil? (:edited-part world)))
    (set-track-head world (:x event) (:y event))
    (let [grain-size (if (:shift-pressed world)
                       0.05
                       0.25)]
      (move-part-moved world event :grain grain-size))))

(defn add-mode-released [world event]
  (-> world
      (move-part-released event)
      (tree-changed)))
