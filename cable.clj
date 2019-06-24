;; -------------------------------------------------------------------------------;;
;; cable mode

(defn draw-segment! [world start end color]
  (let [mesh (-> (:cable-mesh world)
                 (set-mesh-color color)
                 (mesh-to-from start end))]
    (draw-mesh! world mesh)))

(defn draw-cable! [world [name cable]]
  (let [points (actualize-points world (:points cable))
        points (if (= name (:new-cable world))
                 (conj (vec points) (get-mesh-position (:cursor world)))
                 points)]

    (draw-mesh! world (set-mesh-position (:anchor-mesh world) (first points)))

    (if (not (= name (:new-cable world)))
      (draw-mesh! world (set-mesh-position (:anchor-mesh world) (last points))))

    (doseq [i (range 1 (dec (count points)))]
      (let [point (nth points i)]
        (draw-mesh! world (set-mesh-position (:pulley-mesh world) point))))

    (dotimes [i (dec (count points))]
      (draw-segment! world
                     (nth points i)
                     (nth points (inc i))
                     (:color cable)))))

(defn compute-cable-length [world cable-name]
  (let [cable (get-in world [:cables cable-name])
        points (actualize-points world (:points cable))]
    (reduce + (map (fn [a b]
                     (vector-length (vector-subtract a b)))
                   points (rest points)))))

(defn terminate-cable [world]
  (if (:new-cable world)
    (let [cable-name (:new-cable world)
          length (compute-cable-length world cable-name)]
      (-> world
          (dissoc-in [:new-cable])
          (assoc-in [:cables cable-name :length] length)))
    world))

(defn cable-mode-pressed [world event]
  (if (:cursor-snapped world)
    (let [x (:x event)
          y (:y event)
          snap-spec (get-closest-snap-point world x y (:snap-specs world))]
      (if-let [new-cable (:new-cable world)]
        (update-in world [:cables new-cable :points] #(conj % snap-spec)) 
        (let [name (gen-keyword :cable)]
          (-> world
              (assoc-in [:cables name] {:points [snap-spec]
                                        :color :white})
              (assoc-in [:new-cable] name)))))
    (terminate-cable world)))

(defn cable-mode-moved [world event] world)
(defn cable-mode-released [world event] world)

(defn cable-mode-exited [world]
  (terminate-cable world))

(defn get-free-dof [world cable]
  ;;###############################################
  (get-part-with-color world :green))

(defn enforce-cable-length [world cable-name]
  ;; (let [cable (get-in world [:cables cable-name])
  ;;       dof-name (get-free-dof world cable) 
  ;;       dof-part (get-in world [:parts dof-name])
  ;;       desired-length (:length cable)
  ;;       current-length (compute-cable-length world cable-name)
  ;;       dl (- desired-length current-length)
  ;;       test-length (-> world
  ;;                       (update-in [:parts dof-name :value] #(+ % 0.1))
  ;;                       (compute-transforms)
  ;;                       (compute-cable-length cable-name))
  ;;       dt (-  test-length current-length)
  ;;       dvalue (/ (* 0.1 dl) dt)]
  ;;   (update-in world [:parts dof-name :value] #(+ % dvalue)))
  world
  )

(defn enforce-cable-lengths [world]
  ;;###################################################
  (if-let [cable-name (first (first (:cables world)))]
    (enforce-cable-length world cable-name)
    world))

