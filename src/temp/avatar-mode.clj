
(load "avatar/running")
(load "avatar/jumping")

(def avatar-keys ["s" "d" "f" "e"
                  "j" "u" "o" "k" "q"])

(defn load-poses [root-name]
  (let [obj-names (filter #(.endsWith % ".obj")
                          (get-files-at (str "res/" root-name)))]
    (apply merge (map (fn [obj-name]
                        (let [l (- (count obj-name) 4)
                              number (parse-int (subs obj-name 0 l))
                              filename (str "res/" root-name "/" obj-name)
                              model (create-model-mesh filename
                                                       [0 0 0] [1 0 0 0]
                                                       0.15 nil)]
                          {number model}))
                      obj-names))))

(defn reset-avatar [world]
  (assoc-in world [:avatar]
    {:cameraman {:position [0 0 70]
                 :distance 30
                 :height 15
                 }
     :acceleration [0 0 0]
     :velocity [0 0 0]
     :block :ground
     :radius 0.20
     :relative-position [0 0.25 0]
     :relative-direction [0 0 1]
     :position [0 0 0]
     :angle 0
     :poses {:running (load-poses "run")
             :jumping (load-poses "jump")}
     :pose-index 0
     :pose-counter 0
     :max-speed 0.06
     :state :running
     :friction-coefficient 0.8
     :vertical-velocity 0.0
     :shadow-mesh (create-model-mesh "res/cylinder.obj"
                    [0 0 0] [1 0 0 0]
                    1 :black)
     :rope-mesh (create-model-mesh "res/rope.obj"
                  [0 0 0] [1 0 0 0]
                  1 :black)
     :keys (apply merge (map (fn [key] {key :released}) avatar-keys))
     }))

(defn get-solid-block [world block-name]
  (if (= block-name :ground)
    (:other-ground world)
    (let [part (get-in world [:parts block-name])
          transform (use-root-relative-transform world block-name)]
      (-> (get-in world [:info :block :model])
          (assoc-in [:transform] transform)
          (assoc-in [:scale] (:scale part))))))

(defn set-down-collision [world]
  (let [point (get-in world [:avatar :position])
        point (vector-subtract point [0 0.19 0])
        avatar (:avatar world)
        collision (->> (map (fn [block-name]
                              (let [block (get-solid-block world block-name)]
                                (if-let [collision (get-mesh-collision
                                                    block (:transform block)
                                                    (:scale block) [point [0 -1 0]])]
                                  (conj collision block-name)
                                  nil)))
                            (:close-block-names avatar))
                       (filter not-nil?)
                       (sort-by second <)
                       (first))]
    (assoc-in world [:avatar :down-collision] collision)))

(defn get-shadow-transform [world point]
  (if-let [collision (get-in world [:avatar :down-collision])]
    (let [[index d point block-name] collision
          block (get-solid-block world block-name)
          triangles (partition 3 (partition 3 (:vertices block)))
          [a b c] (nth triangles index)
          v1 (vector-subtract b a)
          v2 (vector-subtract c a)
          local-normal (vector-cross-product v1 v2)
          block-rotation (get-rotation-component (:transform block))
          global-normal (apply-transform block-rotation local-normal)]
      (if (< (vector-angle global-normal [0 1 0]) 60)
        (let [position (vector-add point [0 0.001 0])
              rotation (quaternion-from-normal global-normal)]
          (make-transform position rotation))))))

(defn draw-shadow! [world]
  (let [avatar (:avatar world)
        avatar-position (:position avatar)
        avatar-height (nth avatar-position 1)]
    (if-let [transform (get-shadow-transform world avatar-position)]
      (let [shadow-height (second (get-transform-position transform))
            floor-distance (- avatar-height 0.25 shadow-height)
            size (map-between-ranges floor-distance 0 3 0.5 0.00)        
            scale (vector-multiply [1 0.0 1] size)
            mesh (-> (:shadow-mesh avatar)
                     (assoc-in [:transform] transform)
                     (assoc-in [:scale] scale))]
        (draw-mesh! world mesh)))))

(defn get-force-point [world]
  (if-let [{:keys [part-name local-point]}
           (get-in world [:avatar :force])]
    (let [part (get-in world [:weld-groups part-name])
          transform (:transform part)]
      (apply-transform transform local-point))
    (if (:pressed-part world)
      (:pressed-point world)
      nil)))

(defn draw-rope! [world]
  (if-let [start-point (get-force-point world)]
    (let [end-point (vector-subtract
                     (get-in world [:avatar :position])
                     [0 0.1 0])
          v (vector-subtract end-point start-point)
          scale [0.02 (vector-length v) 0.02]
          middle (vector-add (vector-multiply v 0.5) start-point)
          rotation (quaternion-from-normal v)
          transform (make-transform middle rotation)
          mesh (-> (get-in world [:avatar :rope-mesh])
                   (assoc-in [:scale] scale)
                   (assoc-in [:transform] transform))]
      (draw-mesh! world mesh))))

(defn avatar-mode-draw-3d [world]
  (let [avatar (:avatar world)
        state (:state avatar)
        position (:position avatar)
        angle (:angle avatar)
        rotation [0 1 0 angle]
        transform (make-transform position rotation)
        pose-index (:pose-index avatar)
        mesh (get-in avatar [:poses state pose-index])
        mesh (assoc-in mesh [:transform] transform)]
    (draw-mesh! world mesh)
    (draw-shadow! world)
    (draw-rope! world)
    ))

(defn normalize-cameraman [world]
  (let [avatar (:avatar world)
        avatar-position (:position avatar)
        cameraman (:cameraman avatar)
        cameraman-position (:position cameraman)
        to-cameraman (vector-normalize
                      (vector-subtract
                       cameraman-position avatar-position))
        to-cameraman (vector-multiply to-cameraman (:distance cameraman))
        new-cameraman-position (vector-add avatar-position to-cameraman)]
    (assoc-in world [:avatar :cameraman :position] new-cameraman-position)))

(defn get-state-function [state fn-name]
  (let [fn-name (subs (str fn-name) 1)
        function-name (str fn-name "-" (subs (str state) 1) "-state")]
    (ns-resolve 'temp.core (symbol function-name))))

(defn change-state [world new-state]
  (let [avatar (:avatar world)
        current-state (:state avatar)
        exit-fn (or (get-state-function current-state :exit) identity)
        enter-fn (or (get-state-function new-state :enter) identity)]
    (-> world
        (exit-fn)
        (assoc-in [:avatar :state] new-state)
        (assoc-in [:avatar :pose-index] 0)
        (assoc-in [:avatar :pose-counter] 0)
        (enter-fn))))

(defn update-state [world]
  (let [state (get-in world [:avatar :state])
        update-fn (get-state-function state :update)]
    (update-fn world)))

(defn set-cameraman-from-camera [world]
  (let [position (assoc-in (get-in world [:camera :eye]) [1] 0)]
    (-> world
      (assoc-in [:avatar :cameraman :position] position))))

(defn avatar-mode-entered [world]
  (let [parts (:parts world)
        blocks (vec (map first (filter is-solid-part? parts)))]
    (-> world
      ;; (set-cameraman-from-camera)
      (assoc-in [:block-names] (conj blocks :ground))
      (change-state :running))))

(defn avatar-press-part [world]
  (let [avatar (:avatar world)
        position (vector-subtract (:position avatar) [0 0.1 0])
        direction (vector-rotate [0 0 1] [0 1 0] (:angle avatar))
        spec {:x 100000
              :y 100000
              :line [position direction]}]
    (if-let [{:keys [part-name point distance]}
             (get-part-collision (compute-transforms world :parts) spec)]
      (if (< distance 1.0)
        (let [part (get-in world [:parts part-name])
              world (if (in? (:type part) [:button :block :cylinder :cone :sphere])
                      (-> world
                          (assoc-in [:parts part-name :value] 1)
                          (assoc-in [:pressed-part] part-name)
                          (assoc-in [:pressed-point] point))
                      world)]
          (if-let [dof-part-name (get-first-dof world part-name)]
            (let [part (get-in world [:weld-groups dof-part-name])
                  transform (:transform part)
                  inverse-transform (get-inverse-transform transform)
                  local-point (apply-transform inverse-transform point)]
              (assoc-in world [:avatar :force]
                        {:part-name dof-part-name
                         :local-point local-point
                         :vector [0 0 0]
                         :distance distance}))
            world))
        world)
      world)))

(defn avatar-release-part [world]
  (let [world (if (nil? (:pressed-part world))
                world
                (assoc-in world [:parts (:pressed-part world) :value] 0))
        world (if (nil? (get-in world [:avatar :force]))
                world
                (snap-part world (get-in world [:avatar :force :part-name])))]
    (-> world
        (dissoc-in [:pressed-part])
        (dissoc-in [:avatar :force]))))

(defn avatar-key-pressed [world key]
  (assoc-in world [:avatar :keys key] :pressed))

(defn avatar-key-released [world key]
  (assoc-in world [:avatar :keys key] :released))

(defn update-keys [keys]
  (reduce (fn [ks key-name]
            (update-in ks [key-name]
                       #(if (= % :pressed)
                          :repeated
                          %)))
          keys
          avatar-keys))

(defn avatar-key? [key]
  (in? key avatar-keys))

(defn avatar-key-on? [world key]
  (let [keys (get-in world [:avatar :keys])]
    (or
     (= (get keys key) :pressed)
     (= (get keys key) :repeated))))

(defn avatar-key-pressed? [world key]
  (let [keys (get-in world [:avatar :keys])]
    (= (get keys key) :pressed)))

(defn avatar-key-released? [world key]
  (let [keys (get-in world [:avatar :keys])]
    (= (get keys key) :released)))

(defn reactivate-avatar! [world elapsed]
  (let [keys (get-in world [:avatar :keys])]
    (if (not (every? #(= % :released) (vals keys)))
      (reset! avatar-active-time 0)
      (swap! avatar-active-time #(+ elapsed %)))))

(defn update-avatar-force [world]
  (if (nil? (get-in world [:avatar :force]))
    world
    (let [avatar (:avatar world)
          avatar-force (:force avatar)
          {:keys [part-name local-point]} avatar-force]
      (if (or (= (:block avatar) :ground)
              (not= (get-first-dof world (:block avatar))
                    part-name))
        (let [rope-length (:distance avatar-force)
              part (get-in world [:weld-groups part-name])
              transform (:transform part)
              p1 (apply-transform transform local-point)
              p2 (vector-subtract (:position avatar) [0 0.1 0])
              d (distance p1 p2)]
          (if (> d rope-length)
            (let [strength (- d rope-length)
                  force-vector (-> (vector-subtract p2 p1)
                                   (vector-normalize)
                                   (vector-multiply strength))]
              (assoc-in world [:avatar :force :vector] force-vector))
            world))
        world))))

(defn compute-close-blocks [world]
  (let [avatar (:avatar world)
        avatar-position (:position avatar)
        avatar-radius (* 2 (:radius avatar))]
    (assoc-in world [:avatar :close-block-names]
      (filter (fn [block-name]
                (let [block (get-solid-block world block-name)
                      transform (:transform block)
                      position (get-transform-position transform)
                      scale (:scale block)
                      d (distance position avatar-position)
                      block-radius (vector-length (map * [0.5 0.5 0.5] scale))]
                  (< d (+ block-radius avatar-radius))))
        (:block-names world)))))

(defn point-block-projection [point block]
  (let [transform (:transform block)
        inverse-transform (get-inverse-transform transform)
        local-point (apply-transform inverse-transform point)
        scale (map #(* 0.5 %) (:scale block))
        bounded-point (map (fn [p s]
                             (within p (- s) s)) local-point scale)]
    (apply-transform transform bounded-point)))

(defn handle-collisions [world]
  (let [avatar (:avatar world)
        position (:position avatar)
        radius (:radius avatar)]
    (reduce (fn [w block-name]
              (let [block (get-solid-block world block-name)
                    collision-point (point-block-projection position block)
                    to-point (vector-subtract collision-point position)
                    d (vector-length to-point)]
                (if (< 0.0 d radius)
                  (let [wrong-velocity (vector-project (:velocity avatar) to-point)]
                    (if (pos? (vector-dot-product wrong-velocity to-point))
                      (update-in w [:avatar :velocity]
                                 #(vector-subtract % wrong-velocity))
                      w))
                  w)))
            world
            (:close-block-names avatar))))

(defn avatar-mode-released [world event]
  (set-cameraman-from-camera world))

(defn set-view [world]
  (if (:last-point world)
    world
    (let [avatar (:avatar world)
          cameraman (:cameraman avatar)
          pivot (:position avatar)
          cameraman-position (:position cameraman)
          eye (assoc (:position cameraman)
                     1 (:height cameraman))
          to-eye (vector-subtract eye pivot)
          x-angle (- 90 (vector-angle to-eye [0 1 0]))
          y-angle (vector-angle (assoc to-eye 1 0) [0 0 1] [0 1 0])]
      (-> world
          (assoc-in [:camera :x-angle] x-angle)
          (assoc-in [:camera :y-angle] y-angle)
          (assoc-in [:camera :pivot] pivot)
          (compute-camera)))))

(defn print-jump-parameters [world]
  (let [avatar (:avatar world)
        cameraman (:cameraman avatar)
        camera (:camera world)]
    (prn
      {:address (:last-saved-machine world)
       :block (:block avatar)
       :relative-position (:relative-position avatar)
       :relative-direction (:relative-direction avatar)
       :cameraman-position (:position cameraman)
       :x-angle (:x-angle camera)
       :y-angle (:y-angle camera)
       :pivot (:pivot camera)
       })))

(defn avatar-mode-update [world elapsed]
  (reactivate-avatar! world elapsed)
  (-> world
      (update-state)
      (compute-close-blocks)
      (set-down-collision)
      (update-avatar-force)
      (update-in [:avatar :keys] update-keys)
      (normalize-cameraman)
      (set-view)
      ))

