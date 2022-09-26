(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

(declare move-part-pressed)
(declare move-part-moved)
(declare move-part-released)
(declare set-wagon-loop)
(declare get-loop-value)

(defn add-mode-draw [world]
  (let [add-menu (:add-menu world)]
    (let [{:keys [image x y w h]} add-menu]
      (fill-rect! (make-color 70 70 70) x y (+ w 30) (+ h 20))
      (draw-image! image x y))

    (let [box (or (get-in add-menu [:regions (:add-type world)])
                  (get-in add-menu [:regions :new]))
          {:keys [x y w h]} (get-absolute-region box add-menu)]
      (dotimes [i 3]
        (draw-rect! :black x y (- w i -2) (- h i 1))))))

(defn add-wagon-to-track [world wagon-name track-name event]
  (let [transform (get-in world [:parts track-name :transform])]
    (-> world
        (assoc-in [:parts wagon-name :transform] transform)
        (assoc-in [:parts track-name :children wagon-name]
                  (make-transform [0 0 0] [1 0 0 0]))
        (set-wagon-loop wagon-name track-name)
        (assoc-in [:parts wagon-name :value]
                  (get-loop-value world track-name event))
        (compute-transforms :parts))))

(defn add-wagon [world color spec]
  (let [track-name (get-part-at world spec)]
    (if (and (not-nil? track-name)
             (= (get-in world [:parts track-name :type]) :track))
      (let [layer (apply min (:visible-layers world))
            wagon (create-part :wagon color layer (:info world))
            wagon-name (gen-keyword :wagon)]
        (-> world
            (assoc-in [:parts wagon-name] wagon)
            (add-wagon-to-track wagon-name track-name spec)))
      world)))

(defn get-ground-anchor [world part collision]
  (let [offset [0 (get-part-offset part) 0]
        position (vector/add offset (:point collision))]
    (make-transform position [1 0 0 0])))

(defn get-block-anchor [world part collision]
  (let [parent-name (:part-name collision)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        parent-rotation (get-rotation-component parent-transform)
        local-normal (vector/normalize (get-collision-normal world collision))
        global-normal (apply-transform parent-rotation local-normal)
        offset (get-part-offset part)
        position (vector/add (:point collision)
                             (vector/multiply global-normal offset))
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
  (let [normal (vector/normalize (get-collision-normal world collision))]
    (if (or (vector/equal? normal [0 1 0])
            (vector/equal? normal [0 -1 0]))
      (get-block-anchor world part collision)
      nil)))

(defn get-track-head-anchor [world collision]
  (let [index (first (:collision collision))
        mesh (:track-head-model world)
        triangles (partition 3 (partition 3 (:vertices mesh)))
        [a b c] (nth triangles index)
        v1 (vector/subtract b a)
        v2 (vector/subtract c a)
        normal (vector/normalize (vector/cross-product v1 v2))]
    (if (vector/equal? normal [0 -1 0])
      nil
      (let [transform (get-in world [:parts (:track-head world) :transform])
            position (apply-transform transform normal)
            normal-rotation (make-transform [0 0 0]
                                            (quaternion-from-normal normal))
            rotation (get-rotation-component transform)
            final-rotation (combine-transforms normal-rotation rotation)
            rotation (get-transform-rotation final-rotation)]
        (make-transform position rotation)))))

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
      (let [normal (vector/normalize
                     (get-collision-normal world collision))]
        (if (or (vector/equal? normal [0 1 0])
                (vector/equal? normal [0 -1 0]))
          true
          (user-message! "can't place on the side of cylinder")))
      :else
      (user-message! "can't place part on" (kw->str (:type target))))))

(defn add-gear [world spec]
  (if (nil? (:first-gear-part world))
    (do
      (user-message! "Now click on another track or wagon")
      (assoc-in world [:first-gear-part] (get-part-at world spec)))
    (let [part-1-name (:first-gear-part world)
          part-2-name (get-part-at world spec)
          world (-> world
                    (assoc-in [:parts part-1-name :free] true)
                    (assoc-in [:parts part-2-name :free] true))
          part-1 (get-in world [:parts part-1-name])
          part-2 (get-in world [:parts part-2-name])]
      (if (or (= (:type part-1) :wagon)
              (= (:type part-2) :wagon))
        (-> world
            (add-gear-and-rack part-1-name part-2-name)
            (dissoc-in [:first-gear-part]))
        (read-input world
                    (fn [w text]
                      (-> w
                          (add-two-gears part-1-name part-2-name
                                         (parse-float text))
                          (dissoc-in [:first-gear-part])
                          (save-checkpoint!)
                          (tree-changed))))))))

(defn add-part [world type color event]
  (let [layer (apply min (:visible-layers world))
        part (create-part type color layer (:info world))
        part-name (gen-keyword type)
        collision (if event
                    (get-collision world event)
                    {:part-name :ground-part
                     :point [0 0 0]})
        parent (get-in world [:parts (:part-name collision)])
        world (if (can-place-part-at? world collision)
                (-> world
                    (assoc-in [:parts part-name] part)
                    (assoc-in [:last-added] part-name)
                    (place-part-at part-name collision))
                world)]
    (if (or (= (:type parent) :track)
            (nil? event))
      world
      (-> world
          (move-part-pressed part-name nil)
          (move-part-moved event :grain 0.25)))))

(declare recompute-cable-length)

(defn add-pulley-to-cable [world cable-name index pulley-name]
  (-> world
      (update-in [:parts cable-name :parts]
                 #(vector-insert % pulley-name (inc index)))
      (recompute-cable-length cable-name)
      (assoc-in [:parts pulley-name :cable] cable-name)
      (assoc-in [:parts pulley-name :pulley?] true)
      (dissoc-in [:cable-start])))

(defn add-cable [world event]
  (if (nil? (:cable-start world))
    (do
      (user-message! "Now click on another object")
      (-> world
          (add-part :probe :almost-black event)
          (#(assoc-in % [:cable-start] (:last-added %)))))
    (let [{:keys [part-name segment-index]} (get-part-collision world event)
          part (get-in world [:parts part-name])]
      (if (= (:type part) :cable)
        (add-pulley-to-cable world part-name segment-index (:last-added world))
        (let [world (add-part world :probe :almost-black event)
              end (:last-added world)
              start (:cable-start world)]
          (-> world
              (add-part :cable :white nil)
              ((fn [w]
                 (let [cable-name (:last-added w)]
                   (-> w
                       (assoc-in [:parts cable-name :parts] [start end])
                       (assoc-in [:parts start :cable] cable-name)
                       (assoc-in [:parts end :cable] cable-name)
                       (assoc-in [:last-cable] cable-name)))))
              (dissoc-in [:cable-start])))))))

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
          :wagon (add-wagon world color event)
          :gear (add-gear world event)
          :cable (add-cable world event)
          (add-part world (:add-type world) color event))))))

(defn set-track-head [world event]
  (if (= (:add-type world) :track)
    (let [line (get-spec-line world event)
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
      (assoc-in world [:track-head] track-name))
    (assoc-in world [:track-head] nil)))

(defn add-mode-moved [world event]
  (let [world (set-track-head world event)
        grain-size (cond
                     (:shift-pressed world) 0.05
                     (:control-pressed world) 0.01
                     :else 0.25)]
    (move-part-moved world event :grain grain-size)))

(defn finish-cable [world]
  (if-let [cable-name (:last-cable world)]
    (let [cable (get-in world [:parts cable-name])
          positions (get-cable-positions world cable)
          length (vector/distance (first positions) (last positions))]
      (-> world
          (dissoc-in [:last-cable])
          (assoc-in [:parts cable-name :value] length)))
    world))

(defn add-mode-released [world event]
  (-> world
      (move-part-released event)
      finish-cable
      tree-changed))

(defn add-mode-exited [world]
  (-> world
      (assoc-in [:track-head] nil)
      (assoc-in [:first-gear-part] nil)
      (assoc-in [:cable-start] nil)))

(defn draw-track-head! [world]
  (if (and (= (:mode world) :add)
           (= (:add-type world) :track))
    (if-let [track-head-name (:track-head world)]
      (let [transform (get-in world [:parts track-head-name :transform])
            mesh (:track-head-model world)
            mesh (assoc-in mesh [:transform] transform)]
        (draw-mesh! world mesh)))))
