
(ns temp.core (:gen-class))

(defn rotate-cameraman [world angle]
  (let [pivot (get-in world [:avatar :position])
        eye (get-in world [:cameraman :position])
        new-eye (vector-rotate eye pivot [0 1 0] angle)]
    (assoc-in world [:cameraman :position] new-eye)))

(defn handle-buttons [world]
  (cond
    (joystick-button-pressed? :x) (-> world
                                      (assoc-in [:avatar :vertical-velocity] 0.15)
                                      (change-state :jumping))
    (joystick-button-repeated? :bottom-left) (rotate-cameraman world -2)
    (joystick-button-repeated? :bottom-right) (rotate-cameraman world 2)
    :else world))

(defn handle-joystick [world]
  (let [axes (:axes @joystick)
        vx (:horizontal axes)
        vz (:vertical axes)
        joystick-vector [vx 0 vz]]
    (if (float= (vector-length joystick-vector) 0.0)
      world
      (let [[vx _ vz] (if (> (vector-length joystick-vector) 1)
                        (vector-normalize joystick-vector)
                        joystick-vector)
            angle (- (atan2 (- vz) vx) 90)
            avatar (:avatar world)
            cameraman (:cameraman world)
            dv (vector-subtract (:position avatar) (:position cameraman))
            dv (assoc dv 1 0)
            dv (vector-rotate dv [0 1 0] angle)
            velocity (:velocity avatar)
            new-velocity (vector-add velocity dv)
            max-speed (:max-speed avatar)
            new-velocity (if (> (vector-length new-velocity) max-speed)
                           (vector-multiply
                            (vector-normalize new-velocity) max-speed)
                           new-velocity)
            mesh-angle (+ 90 (vector-angle new-velocity [1 0 0] [0 -1 0]))]
        (-> world
            (assoc-in [:avatar :velocity] new-velocity)
            (assoc-in [:avatar :angle] mesh-angle))))))
  
;; (defn get-block-direction [world block-name]
;;   (let [vector [1 0 0]
;;         block (get-in world [:blocks block-name])
;;         transform (:transform block)
;;         rotation (get-transform-rotation transform)
;;         t2 (make-transform [0 0 0] rotation)
;;         [x _ z] (apply-transform t2 vector)]
;;     (vector-normalize [x 0 z])))

;; (defn compute-relative-direction [world]
;;   (let [avatar (:avatar world)
;;         block-name (get-in avatar [:relative-position :block])
;;         start-direction (get-block-direction world block-name)
;;         start-angle (:angle avatar)]
;;     (assoc-in world [:avatar :relative-direction]
;;               {:start-direction start-direction
;;                :start-angle start-angle
;;                :block block-name})))

(defn move-avatar [world]
  (let [avatar (:avatar world)
        velocity (:velocity avatar)
        stopped (:stopped avatar)
        fc (:friction-coefficient avatar)
        ]
    (cond
      ;; (and (not stopped)
      ;;      (< (vector-length velocity) 0.1))
      ;; (-> world
      ;;     (compute-relative-direction)
      ;;     (assoc-in [:avatar :stopped] true)
      ;;     (assoc-in [:avatar :animation-counter] 0))

      ;; (and stopped
      ;;      (> (vector-length velocity) 0.1))
      ;; (-> world
      ;;     (assoc-in [:avatar :stopped] false)
      ;;     (dissoc-in [:avatar :relative-direction])
      ;;     (assoc-in [:avatar :animation-counter] 0))

      :else      
      (let [{:keys [block offset]} (:relative-position avatar)
            mesh (get-solid-block world block)
            transform (:transform mesh)
            position (apply-transform transform offset)
            new-position (vector-add position velocity)]
        (if-let [ground-position (project-down world mesh new-position)]
          (let [inverse-transform (get-inverse-transform transform)
                new-offset (apply-transform inverse-transform ground-position)]
            (-> world
                (assoc-in [:avatar :relative-position :offset] new-offset)
                (update-in [:avatar :velocity] #(vector-multiply % fc))))
          world)))))

(defn enter-running-state [world]
  (-> world
      (assoc-in [:avatar :stopped] true)
      ;; (compute-relative-direction)
      ))

(defn exit-running-state [world]
  (dissoc-in world [:avatar :relative-direction]))

(defn set-avatar-position [world]
  (if-let [{:keys [block offset]} (get-in world [:avatar
                                                 :relative-position])]
    (let [mesh (get-solid-block world block)
          transform (:transform mesh)
          position (vector-add [0 0.25 0] (apply-transform transform offset))]
      (assoc-in world [:avatar :position] position))
    world))

;; (defn set-avatar-direction [world]
;;   (if-let [{:keys [block start-angle start-direction]}
;;            (get-in world [:avatar :relative-direction])]
;;     (let [current-direction (get-block-direction world block)
;;           angle-offset (vector-angle start-direction current-direction [0 1 0])]
;;         (assoc-in world [:avatar :angle] (+ start-angle angle-offset)))
;;     world))

(defn update-running-state [world]
  ;; (println! (get-in world [:avatar :relative-position :offset]))
  (-> world
      (set-avatar-position)
      ;; (set-avatar-direction)
      ;; (handle-collisions)
      (move-avatar)
      (handle-buttons)
      (handle-joystick)
      ))
