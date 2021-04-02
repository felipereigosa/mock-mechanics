
(ns temp.core (:gen-class))

(declare change-state)
(declare get-solid-block)
(declare project-down)
(declare avatar-press-part)
(declare avatar-release-part)
(declare avatar-key-on?)
(declare avatar-key-pressed?)
(declare avatar-key-released?)

(defn rotate-cameraman [world angle]
  (let [pivot (get-in world [:avatar :position])
        eye (get-in world [:cameraman :position])
        new-eye (vector-rotate eye pivot [0 1 0] angle)]
    (assoc-in world [:cameraman :position] new-eye)))

(defn update-pose [world]
  (let [pose-counter (get-in world [:avatar :pose-counter])
        num-poses (count (get-in world [:avatar :poses :running]))]
    (if (> pose-counter 5)
      (-> world
          (assoc-in [:avatar :pose-counter] 0)
          (update-in [:avatar :pose-index] #(mod (inc %) num-poses)))
      (update-in world [:avatar :pose-counter] inc))))

(defn run-handle-direction-keys [world]
  (let [direction (cond
                    (and
                     (avatar-key-on? world "s")
                     (avatar-key-on? world "e"))
                    [-1 0 -1]

                    (and
                     (avatar-key-on? world "s")
                     (avatar-key-on? world "d"))
                    [-1 0 1]

                    (and
                     (avatar-key-on? world "f")
                     (avatar-key-on? world "e"))
                    [1 0 -1]

                    (and
                     (avatar-key-on? world "f")
                     (avatar-key-on? world "d"))
                    [1 0 1]

                    (avatar-key-on? world "s") [-1 0 0]
                    (avatar-key-on? world "f") [1 0 0]
                    (avatar-key-on? world "d") [0 0 1]
                    (avatar-key-on? world "e") [0 0 -1]
                    
                    :else [0 0 0])]
    (if (float= (vector-length direction) 0.0)
      world
      (let [[vx _ vz] (vector-normalize direction)
            angle (- (atan2 (- vz) vx) 90)
            avatar (:avatar world)
            cameraman (:cameraman world)
            dv (vector-subtract (:position avatar) (:position cameraman))
            dv (assoc dv 1 0)
            dv (vector-rotate dv [0 1 0] angle)
            velocity (:velocity avatar)
            new-velocity (vector-add velocity dv)
            ;; max-speed (if (not (joystick-button-released? :top-right))
            ;;             (* 2.5 (:max-speed avatar))
            ;;             (:max-speed avatar))

            max-speed (:max-speed avatar)
            new-velocity (if (> (vector-length new-velocity) max-speed)
                           (vector-multiply
                            (vector-normalize new-velocity) max-speed)
                           new-velocity)
            absolute-direction (vector-normalize dv)
            block (get-solid-block world (:block avatar))
            block-rotation (get-rotation-component (:transform block))
            inverse-rotation (get-inverse-transform block-rotation)
            relative-direction (apply-transform inverse-rotation absolute-direction)]
        (-> world
            (update-pose)
            (assoc-in [:avatar :velocity] new-velocity)
            (assoc-in [:avatar :relative-direction] relative-direction))))))

(defn run-handle-other-keys [world]
  (let [keys (get-in world [:avatar :keys])]
    (cond
      ;; (avatar-key-pressed? world "q")
      ;; (change-state world :vision)
      
      (avatar-key-pressed? world "j")
      (-> world
          (assoc-in [:avatar :vertical-velocity] 0.15)
          (change-state :jumping))

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
      
      (avatar-key-on? world "u") (rotate-cameraman world -2)
      (avatar-key-on? world "o") (rotate-cameraman world 2)
      :else world)))

(defn enter-running-state [world]
  (let [block-name (get-in world [:avatar :block])]
    (if (not= block-name :ground)
      (-> world
          (assoc-in [:parts block-name :value] 1)
          (assoc-in [:avatar :stopped] true))
      world)))

(defn move-avatar [world]
  (let [avatar (:avatar world)
        velocity (:velocity avatar)
        stopped (:stopped avatar)
        stop-velocity (* (:max-speed avatar) 0.5)]
    (cond
      (and (not stopped)
           (< (vector-length velocity) stop-velocity))
      (-> world
          (assoc-in [:avatar :pose-index] 0)
          (assoc-in [:avatar :pose-counter] 0)
          (assoc-in [:avatar :stopped] true))

      (and stopped
           (> (vector-length velocity) stop-velocity))
      (assoc-in world [:avatar :stopped] false)

      :else
      (let [avatar (:avatar world)
            velocity (:velocity avatar)
            fc (:friction-coefficient avatar)
            block (:block avatar)
            mesh (get-solid-block world block)
            transform (:transform mesh)
            position (apply-transform transform
                                      (:relative-position avatar))
            new-position (vector-add position velocity)
            new-position (vector-add new-position [0 0.1 0])]
        (if-let [ground-position (project-down world mesh new-position)]
          (let [inverse-transform (get-inverse-transform transform)
                offset (apply-transform inverse-transform ground-position)]
            (-> world
                (assoc-in [:avatar :relative-position] offset)
                (update-in [:avatar :velocity] #(vector-multiply % fc))))
          (-> world
              (assoc-in [:avatar :vertical-velocity] 0.03)
              (change-state :jumping)))))))

(defn compute-avatar-transform [world]
  (let [avatar (:avatar world)
        block (:block avatar)
        
        mesh (get-solid-block world block)
        transform (:transform mesh)
        relative-position (:relative-position avatar)
        position (->> relative-position
                      (apply-transform transform)
                      (vector-add [0 0.3 0]))
        relative-direction (:relative-direction avatar)
        rotation-transform (get-rotation-component transform)
        direction (apply-transform rotation-transform relative-direction)
        angle (vector-angle direction [0 0 1] [0 -1 0])]
    (-> world
        (assoc-in [:avatar :position] position)
        (assoc-in [:avatar :angle] angle))))     

(defn update-running-state [world]
  (-> world
      (run-handle-other-keys)
      (run-handle-direction-keys)
      (move-avatar)      
      (compute-avatar-transform)
      ;; (handle-collisions)
      ))
