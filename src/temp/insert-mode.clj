
(ns temp.core)

(defn insert-mode-draw [world]
  (fill-rect! (make-color 70 70 70) 330 580 800 70)

  (let [{:keys [image x y]} (:insert-menu world)]
    (draw-image! image x y))

  (let [box (get-in world [:insert-menu :regions
                           (:insert-type world)])
        {:keys [x y w h]} box]
    (dotimes [i 3]
      (draw-rect! :black x y (- w i) (- h i 1)))))

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
  ;; (let [{:keys [x y]} event]
  ;;   (if (> (:y event) 545)
  ;;     (if-let [region (get-region-at (:insert-menu world) x y)]
  ;;       (assoc-in world [:insert-type] region)
  ;;       world)
  ;;     (let [type (:insert-type world)
  ;;           color (get-in world [:info type :color])]
  ;;       (case (:insert-type world)
  ;;         :wagon
  ;;         (insert-wagon world color x y)

  ;;         :sphere
  ;;         (insert-sphere world x y)
          
  ;;         (if-let [anchor (get-anchor-point world x y)]
  ;;           (let [part (create-part type color (:info world))
  ;;                 part-name (gen-keyword type)
  ;;                 offset (get-part-offset part)
  ;;                 parent-name (:part anchor)
  ;;                 parent (get-in world [:parts parent-name])
  ;;                 transform (anchor->transform offset anchor parent)
  ;;                 v1 (apply-rotation transform [1 0 0])
  ;;                 v2 (apply-rotation transform [0 0 1])
  ;;                 point (:position anchor)
  ;;                 plane [point
  ;;                        (vector-add point v1)
  ;;                        (vector-add point v2)]]
  ;;             (-> world
  ;;                 (assoc-in [:parts part-name] part)
  ;;                 (assoc-in [:parts part-name :transform] transform)
  ;;                 (create-relative-transform part-name parent-name)
  ;;                 (assoc-in [:plane] plane)
  ;;                 (assoc-in [:edited-part] part-name)))
  ;;           world)))))
  (let [{:keys [x y]} event
        type :block
        color :white
        ]
    ;; (if-let [collision (get-part-collision world x y)]
    ;;   (if-let [anchor (get-anchor-point world collision)]
    ;;     (let [part (create-part type color (:info world))
    ;;           part-name (gen-keyword type)
    ;;           offset (get-part-offset part)
    ;;           parent-name (:part anchor)
    ;;           parent (get-in world [:parts parent-name])
    ;;           transform (anchor->transform offset anchor parent)
    ;;           ]
    ;;       (println! part)
    ;;       (-> world
    ;;           ;; (assoc-in [:parts part-name] part)
    ;;           ;; (assoc-in [:parts part-name :transform] transform)
    ;;           ;; (create-relative-transform part-name parent-name)
    ;;           ;; (assoc-in [:edited-part] part-name)
    ;;           ;; (create-weld-groups)
    ;;           ))
    ;;     world)
    ;;   world)
    (println! "pressed")
    world
    ))

(defn insert-mode-moved [world event]
  ;; (if-let [part-name (:edited-part world)]
  ;;   (let [{:keys [x y]} event
  ;;         line (unproject-point world [x y])
  ;;         touch-point (line-plane-intersection line (:plane world))
  ;;         part (get-in world [:parts part-name])
  ;;         rotation (get-transform-rotation (:transform part))
  ;;         rotation-transform (get-rotation-component (:transform part))
  ;;         offset (apply-transform rotation-transform [0 (get-part-offset part) 0])
  ;;         transform (make-transform (vector-add offset touch-point) rotation)]
  ;;     (-> world
  ;;         (assoc-in [:parts part-name :transform] transform)))
  ;;   world)
  world
  )

(defn insert-mode-released [world event]
  ;; (if-let [part-name (:edited-part world)]
  ;;   (let [parent-name (get-parent-part world part-name)]
  ;;     (-> world
  ;;         (create-relative-transform part-name parent-name)
  ;;         (dissoc-in [:edited-part])))
  ;;   world)
  world
  )

)
