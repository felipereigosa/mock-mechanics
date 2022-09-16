(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

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
        eye (get-in world [:avatar :cameraman :position])
        new-eye (vector/rotate eye pivot [0 1 0] angle)]
    (assoc-in world [:avatar :cameraman :position] new-eye)))

(defn get-acceleration [world]
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
    (if (float= (vector/length direction) 0.0)
      [0 0 0]
      (let [[vx _ vz] (vector/normalize direction)
            angle (- (atan2 (- vz) vx) 90)
            avatar (:avatar world)
            cameraman (:cameraman avatar)
            dv (vector/subtract (:position avatar) (:position cameraman))
            dv (assoc dv 1 0)]
        (vector/rotate dv [0 1 0] angle)))))

(defn run-handle-direction-keys [world]
  (let [acceleration (get-acceleration world)]
    (if (not (vector/equal? acceleration [0 0 0]))
      (let [avatar (:avatar world)
            velocity (:velocity avatar)
            new-velocity (vector/add velocity acceleration)
            max-speed (:max-speed avatar)
            new-velocity (if (> (vector/length new-velocity) max-speed)
                           (vector/multiply
                             (vector/normalize new-velocity) max-speed)
                           new-velocity)
            absolute-direction (vector/normalize new-velocity)
            avatar (:avatar world)
            block (get-solid-block world (:block avatar))
            block-rotation (get-rotation-component (:transform block))
            inverse-rotation (get-inverse-transform block-rotation)
            relative-direction (apply-transform inverse-rotation absolute-direction)]
        (-> world
            (assoc-in [:avatar :velocity] new-velocity)
            (assoc-in [:avatar :relative-direction] relative-direction)))
      world)))

(defn set-jump-velocity [world vertical]
  (let [avatar (:avatar world)
        velocity (:velocity avatar)
        absolute-velocity (:absolute-velocity avatar)
        block-velocity (-> absolute-velocity
                           (vector/subtract velocity)
                           (vector/multiply 0.7))
        final-velocity (-> velocity
                           (vector/multiply 1.5)
                           (vector/add block-velocity))]
    (-> world
        (assoc-in [:avatar :vertical-velocity] vertical)
        (assoc-in [:avatar :velocity] final-velocity))))

(defn run-handle-other-keys [world]
  (let [keys (get-in world [:avatar :keys])]
    (cond
      (avatar-key-pressed? world "j")
      (-> world
          (set-jump-velocity 0.15)
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

      (avatar-key-on? world "u") (rotate-cameraman world -1)
      (avatar-key-on? world "o") (rotate-cameraman world 1)
      :else world)))

(defn enter-running-state [world]
  (let [block-name (get-in world [:avatar :block])]
    (-> world
        (assoc-in [:parts block-name :value] 1)
        (assoc-in [:avatar :stopped] true)
        (assoc-in [:avatar-mesh :index] 60))))

(defn project-down [world block point]
  (let [line [point [0 -1 0]]
        [_ d point] (get-mesh-collision block (:transform block)
                                        (:scale block) line)]
    (if (and (not-nil? d)
             (< d -0.1))
      nil
      point)))

(defn move-avatar [world]
  (let [avatar (:avatar world)
        acceleration (get-acceleration world)
        stopped (:stopped avatar)]
    (cond
      (and (not stopped)
           (vector/equal? acceleration [0 0 0]))
      (-> world
          (assoc-in [:avatar-mesh :index] 60)
          (assoc-in [:avatar :stopped] true))

      (and stopped
           (not (vector/equal? acceleration [0 0 0])))
      (-> world
          (assoc-in [:avatar-mesh :index] 0)
          (assoc-in [:avatar :stopped] false))

      :else
      (let [velocity (:velocity avatar)
            fc (:friction-coefficient avatar)
            block-name (:block avatar)
            block (get-solid-block world block-name)
            transform (:transform block)
            position (apply-transform transform
                                      (:relative-position avatar))
            new-position (vector/add position velocity)
            new-position (vector/add new-position [0 0.1 0])]
        (if-let [ground-position (project-down world block new-position)]
          (let [inverse-transform (get-inverse-transform transform)
                offset (apply-transform inverse-transform ground-position)]
            (-> world
                (assoc-in [:avatar :relative-position] offset)
                (update-in [:avatar :velocity] #(vector/multiply % fc))))
          (-> world
              (set-jump-velocity 0.03)
              (change-state :jumping)))))))

(defn compute-avatar-transform [world]
  (let [avatar (:avatar world)
        block (:block avatar)

        mesh (get-solid-block world block)
        transform (:transform mesh)
        relative-position (:relative-position avatar)
        position (->> relative-position
                      (apply-transform transform)
                      (vector/add [0 0.3 0]))
        relative-direction (:relative-direction avatar)
        rotation-transform (get-rotation-component transform)
        direction (apply-transform rotation-transform relative-direction)
        direction (assoc-in direction [1] 0)
        direction (vector/normalize direction)
        angle (vector/angle direction [0 0 1] [0 -1 0])]
    (-> world
        (assoc-in [:avatar :position] position)
        (assoc-in [:avatar :angle] angle))))

(declare set-new-relative-transform)

(defn climb-small-elevation [world]
  (let [avatar (:avatar world)
        old-block (:block avatar)
        [_ _ point new-block] (:down-collision avatar)]
    (if (and (not-nil? new-block)
             (not= new-block old-block))
      (-> world
          (assoc-in [:parts old-block :value] 0)
          (assoc-in [:parts new-block :value] 1)
          (set-new-relative-transform new-block point)
          (compute-avatar-transform))
      world)))

(declare handle-collisions)

(defn update-running-pose [world]
  (update-in world [:avatar-mesh]
             (fn [mesh]
               (let [index (:index mesh)]
                 (if (< index 60)
                   (assoc-in mesh [:index] (in-range (+ index 0.6) 0 59))
                   (assoc-in mesh [:index]
                             (in-range (+ index 0.3) 60 79)))))))

(defn update-running-state [world]
  (let [avatar (:avatar world)
        velocity (vector/subtract (:position avatar)
                                  (:last-position avatar))]
    (-> world
        (assoc-in [:avatar :last-position] (:position avatar))
        (assoc-in [:avatar :absolute-velocity] velocity)
        (run-handle-direction-keys)
        (run-handle-other-keys)
        (handle-collisions)
        (move-avatar)
        (compute-avatar-transform)
        (update-running-pose)
        (climb-small-elevation))))
