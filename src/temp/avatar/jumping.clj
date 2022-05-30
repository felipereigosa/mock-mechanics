
(defn enter-jumping-state [world]
  (let [block-name (get-in world [:avatar :block])]
    (-> world
        (assoc-in [:parts block-name :value] 0)
        (assoc-in [:avatar :start-jump-height]
                  (get-in world [:avatar :position 1])))))

(defn set-new-relative-transform [world block-name point]
  (let [block (get-solid-block world block-name)
        transform (:transform block)
        inverse-transform (get-inverse-transform transform)
        avatar (:avatar world)
        relative-position (apply-transform inverse-transform point)
        absolute-direction (vector-rotate [0 0 1] [0 1 0] (:angle avatar))
        block-rotation (get-rotation-component (:transform block))
        inverse-rotation (get-inverse-transform block-rotation)
        relative-direction (apply-transform inverse-rotation absolute-direction)]
    (-> world
        (assoc-in [:avatar :block] block-name)
        (assoc-in [:avatar :relative-position] relative-position)
        (assoc-in [:avatar :relative-direction] relative-direction))))

(defn jump-handle-direction-keys [world]
  (let [acceleration (get-acceleration world)]
    (if (not (vector= acceleration [0 0 0]))
      (let [avatar (:avatar world)
            velocity (:velocity avatar)
            acceleration (vector-multiply acceleration 0.0002)
            new-velocity (vector-add velocity acceleration)
            max-speed (:max-speed avatar)
            new-velocity (if (> (vector-length new-velocity) max-speed)
                           (vector-multiply
                            (vector-normalize new-velocity) max-speed)
                           new-velocity)
            angle (vector-angle [0 0 1] acceleration [0 1 0])]
        (-> world
            (assoc-in [:avatar :velocity] new-velocity)
            (assoc-in [:avatar :angle] angle)))
      world)))

(defn jump-handle-keys [world]
  (cond
    (avatar-key-pressed? world "j")
    (if-let [normal (:wall-normal world)]
      (-> world
          (assoc-in [:avatar :vertical-velocity] 0.2)
          (assoc-in [:avatar :velocity] (vector-multiply normal 0.1)))
      world)
    
    (avatar-key-pressed? world "k")
    (-> world
        (avatar-press-part)
        (assoc-in [:avatar :part-pressed] true))

    (and
     (avatar-key-released? world "k")
     (get-in world [:avatar :part-pressed]))
    (-> world
        (avatar-release-part)
        (assoc-in [:avatar :part-pressed] false))

    (avatar-key-on? world "u") (rotate-cameraman world -1)
    (avatar-key-on? world "o") (rotate-cameraman world 1)
    :else world))

(defn get-end-block [world point]
  (let [avatar (:avatar world)
        [_ _ block-point block-name] (:down-collision avatar)]
    (if (and (not-nil? block-point)
             (< (distance block-point point) 0.3))
      [block-name block-point]
      nil)))


;;----------------------------------------------------------------------;;

(do
1

(defn set-jump-velocity [world vertical] ;;########################
  (let [;; avatar (:avatar world)
        ;; velocity (:velocity avatar)
        ;; absolute-velocity (:absolute-velocity avatar)
        ;; block-velocity (-> absolute-velocity
        ;;                    (vector-subtract velocity)
        ;;                    (vector-multiply 0.7))
        ;; final-velocity (-> velocity
        ;;                    (vector-multiply 1.5)
        ;;                    (vector-add block-velocity))
        ]
    (-> world
        ;; (assoc-in [:avatar :vertical-velocity] vertical)
        ;; (assoc-in [:avatar :velocity] final-velocity)
        (assoc-in [:jump-counter] 0)
        )))

(declare get-shadow-transform)
(declare draw-shadow!)
(declare draw-rope!)

(defn avatar-mode-draw-3d [world]
  (let [avatar (:avatar world)
        state (:state avatar)
        position (vector-subtract (:position avatar) [0 0.3 0])
        angle (:angle avatar)
        avatar-position (:position avatar)
        tilt-rotation (if (= state :jumping)
                        (make-transform [0 0 0] [1 0 0 0])
                        (get-rotation-component
                         (get-shadow-transform world avatar-position)))
        angle-rotation (make-transform [0 0 0] [0 1 0 (+ angle 90)])
        rotation-transform (combine-transforms angle-rotation tilt-rotation)
        ;; position (assoc-in position [1] 0.0)
        position-transform (make-transform position [1 0 0 0])
        transform (combine-transforms rotation-transform position-transform)
        mesh (:avatar-mesh world)
        mesh (assoc-in mesh [:transform] transform)]
    (draw-mesh! world mesh)
    (draw-shadow! world)
    (draw-rope! world)))

;; (defn update-jumping-pose [world]
;;   (let [avatar (:avatar world)
;;         v (:vertical-velocity avatar)
;;         height (second (:position avatar))
;;         s (:start-jump-height avatar)
;;         t (if (pos? v)
;;             (map-between-ranges height 0.44 2.475 0 0.5)
;;             (map-between-ranges height 2.475 0.44 0.5 1))
;;         end (+ 80 33)
;;         frame (within (map-between-ranges t 0 1 80 end) 80 end)
;;         ]
    
;;     ;; (assoc-in world [:avatar-mesh :index]
;;     ;;           (within (map-between-ranges height (- s 0.6) (+ s 1.05) 80 109)
;;     ;;                   80 109))

;;     world
;;     ;; (println! height)

;;     ;; (assoc-in world [:avatar-mesh :index] frame)
;;     ))

(defn get-end-block [world point]
  (let [avatar (:avatar world)
        [_ _ block-point block-name] (:down-collision avatar)]
    (if (and (not-nil? block-point)
             (< (distance block-point point) 0.3))
      [block-name block-point]
      nil)))

(defn update-jumping-state [world]
  (let [end-counter 60
        avatar (:avatar world)
        ;; v (+ (:vertical-velocity avatar) -0.01)
        ;; vertical-velocity (vector-multiply [0 1 0] v)
        horizontal-velocity (:velocity avatar)
        ;; velocity (vector-add vertical-velocity horizontal-velocity)
        velocity horizontal-velocity
        ;; new-position (vector-add (:position avatar) velocity)

        counter (:jump-counter world)

        buffer (int (* end-counter 0.2))
        new-position (if (< buffer counter (- end-counter buffer))
                       (vector-add (:position avatar) velocity)
                       (:position avatar))

        mesh (:avatar-mesh world)
        frame-index (round (:index mesh))
        matrix (nth (:bone-matrices mesh) frame-index)
        animation-height (* (second (apply-matrix matrix [0 0 0]))
                            (first (:scale mesh)))
        animation-height (max 0.01 (- animation-height 0.333))
        new-position (assoc-in new-position [1] animation-height)
        [end-block end-point] (get-end-block world new-position)

        ]


    ;;   (cond
    ;;     (and (< v 0.0) end-block)
    ;;     (-> world
    ;;         (set-new-relative-transform end-block end-point)
    ;;         (compute-avatar-transform)
    ;;         (change-state :running))

    ;;     (< (nth new-position 1) -5)
    ;;     (reset-avatar world)

    ;;     :else
    ;;     (-> world
    ;;         (assoc-in [:avatar :position] new-position)
    ;;         (assoc-in [:avatar :vertical-velocity] v)
    ;;         (update-jumping-pose)
    ;;         (jump-handle-keys)
    ;;         (jump-handle-direction-keys)          
    ;;         (handle-collisions))))
    (if (> (:jump-counter world) end-counter)
      (-> world
          (set-new-relative-transform end-block end-point)
          (compute-avatar-transform)
          (assoc-in [:avatar :velocity] [0 0 0])
          (change-state :running))

      (let [start 80
            end (+ 80 33)
            ]
        (-> world
            (update-in [:jump-counter] inc)
            (assoc-in [:avatar-mesh :index]
                      (within (map-between-ranges counter 0 end-counter start end)
                              start end))
            (assoc-in [:avatar :position] new-position)
            ;;         (assoc-in [:avatar :vertical-velocity] v)
            ;;         (update-jumping-pose)
            (jump-handle-keys)
            (jump-handle-direction-keys)          
            (handle-collisions))))))
)

(do
1

;; (defn set-down-collision2 [world]
;;   (let [avatar (:avatar world)
;;         point (:position avatar)
;;         point (vector-subtract point [0 0.19 0])
;;         collision (->> (map (fn [block-name]
;;                               (let [block (get-solid-block world block-name)]
;;                                 (if-let [collision (get-mesh-collision
;;                                                     block (:transform block)
;;                                                     (:scale block) [point [0 -1 0]])]
;;                                   (conj collision block-name)
;;                                   nil)))
;;                             (:close-block-names avatar))
;;                        (filter not-nil?)
;;                        (sort-by second <)
;;                        (first))
;;         ]
;;     (println! collision)
;;     ;; (assoc-in world [:avatar :down-collision] collision)
;;     ))

(defn get-end-block [world point]
  (let [avatar (:avatar world)
        [_ _ block-point block-name] (:down-collision avatar)]
    ;; (if (and (not-nil? block-point)
    ;;          (< (distance block-point point) 0.3))
    ;;   [block-name block-point]
    ;;   nil)
    [block-name block-point]
    ))

;; (clear-output!)
;; (let [world @world
;;       point (vector-add [0 0.1 0] [0 0.1 0];; (get-in world [:avatar :position])
;;                         )
;;       avatar (:avatar world)
;;       ]
;;   ;; (set-down-collision2 world)

;;   (get-end-block world (:position avatar))
;;   nil
;;   )
)
