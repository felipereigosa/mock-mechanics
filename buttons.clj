        (= menu-name :reflex-buttons)
        (if (in? button-name [:pick :add :remove])
          (-> world
              (assoc-in [:saved-mode] (:mode world))
              (assoc-in [:mode] :pick)
              (assoc-in [:mode-action] button-name))
          (do
            (println! "other button")
            world))
      

        (= menu-name :tools)
        (if (= button-name :cable)
          (assoc-in world [:mode] :cable)
          (-> world
              (assoc-in [:mode] :insert)
              (assoc-in [:mode-part] button-name)))

        (assoc-in world [:mode] button-name)

    (if-let [cable-name (get-cable-at world (:x event) (:y event))]
      (assoc-in world [:cables cable-name :color] (:mode-color world))
      world)

(defn move-mode-pressed [world event]
  (let [moving-part (get-part-at world (:x event) (:y event))
        part (get-in world [:parts moving-part])
        transform (:transform part)
        rotation (get-transform-rotation transform)]
    (-> world
        (detach-child moving-part)
        (assoc-in [:moving-part] moving-part)
        (assoc-in [:start-rotation] rotation))))

(defn move-mode-moved [world event]
  (if-let [moving-part (:moving-part world)]
    (let [x (:x event)
          y (:y event)
          snap-specs (filter (fn [spec]
                               (not (= (:part spec) moving-part)))
                             (:snap-specs world))
          snap-spec (get-closest-snap-point world x y snap-specs)
          part (get-in world [:parts moving-part])
          offset (get-in world [:info (:type part) :offset])]
      (if (nil? snap-spec)
        (let [plane-point (get-ground-camera-point world x y offset)
              transform (make-transform plane-point (:start-rotation world))]
          (-> world
              (assoc-in [:parts moving-part :transform] transform)
              (assoc-in [:snap-spec] nil)))
        (let [final-point (:position snap-spec)
              final-rotation (:rotation snap-spec)
              rotation-transform (make-transform [0 0 0] final-rotation)
              offset [0 offset 0]
              offset (apply-transform rotation-transform offset)
              final-point (vector-add final-point offset)
              final-transform (make-transform final-point final-rotation)]
          (-> world
              (assoc-in [:parts moving-part :transform] final-transform)
              (assoc-in [:snap-spec] snap-spec)))))
    world))

(defn move-mode-released [world event]
  (if-let [moving-part (:moving-part world)]
    (if-let [snap-spec (:snap-spec world)]
      (let [static-part (:part snap-spec)]
        (-> world
            (attach-child moving-part snap-spec)
            (dissoc-in [:moving-part])))
      (-> world
          (dissoc-in [:moving-part])
          (compute-tracks-directions)))
    world))

  (let [[menu button] (get-current-button world)
        {:keys [x y w h]} (get-in world [:menus menu :regions button])
        color :black]
    (draw-rect! color x y w h)
    (draw-rect! color (+ x 1) y w h)
    (draw-rect! color (- x 1) y w h)
    (draw-rect! color x (+ y 1) w h)
    (draw-rect! color x (- y 1) w h))
