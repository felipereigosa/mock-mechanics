
(ns temp.core (:gen-class))

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
            {:velocity [0 0 0]
             :block :ground
             :relative-position [0 0.25 0]
             :relative-direction [0 0 1]
             :position [0 0 0]
             :angle 0
             :poses {:running (load-poses "run")
                     :jumping (load-poses "jump")}
             :pose-index 0
             :pose-counter 0
             :max-speed 0.045
             :state :running
             :friction-coefficient 0.8
             :vertical-velocity 0.0
             :shadow-mesh (create-model-mesh "res/cylinder.obj"
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

;; (defn project-down-full [world block point]
;;   (let [line [point [0 -1 0]]
;;         collision (get-mesh-collision block (:transform block)
;;                                       (:scale block) line)
;;         d (:d collision)]
;;     (if (and (not-nil? d)
;;              (< d -0.1))
;;       nil
;;       collision)))

(defn project-down [world block point]
  ;;###################################################
  (let [line [point [0 -1 0]]
        [_ d ground-point]
        (get-mesh-collision block (:transform block)
                            (:scale block) line)]
    (if (and (not-nil? d)
             (< d -0.1))
      nil
      ground-point)))

;; (defn get-down-collision [world point]
;;   (->> (map (fn [block-name]
;;               (let [block (get-solid-block world block-name)]
;;                 (if-let [collision (project-down-full world block point)]
;;                   [block-name collision]
;;                   nil)))
;;             (:block-names world))
;;        (filter not-nil?)
;;        (sort-by (comp second second) <)
;;        (first)))

(defn get-shadow-transform [world point]
    ;; (let [;; block-transform (:transform block)
    ;;       ;; ;; block-rotation (get-transform-rotation block-transform)
    ;;       ;; [_ h _] point-underneath
    ;;       ;; position (assoc-in point [1] (+ h 0.01))
    ;;       ;; rotation [1 0 0 0]
    ;;       ]
    ;;   ;; (make-transform position rotation)
    ;;   ;; nil
    ;;   (make-transform [0 0 0] [1 0 0 0])
  ;;   )

  ;; (if-let [[block-name collision] (get-down-collision world point)]
  ;;   (let [block (get-solid-block world block-name)
  ;;         ;; block-transform (:transform block)
  ;;         ;; [_ h _] (nth collision 2)
  ;;         ;; position (assoc-in point [1] (+ h 0.01))
  ;;         ;;       ;; rotation [1 0 0 0]
  ;;         position (vector-add (nth collision 2) [0 0.01 0])
  ;;         rotation [1 0 0 0]
  ;;         ]
  ;;     ;; (println! (get-collision-normal world collision))
  ;;     ;; (println! collision)
  ;;     (make-transform position rotation))
  ;;   nil)
  (make-transform [0 0 0] [1 0 0 0])
  )
   
;; (clear-output!)
;; (let [world @world
;;       position (get-in world [:avatar :position])
;;       collision [6 0.29999992596235536 [2.992332935333252 0.45603035343560916 -2.73593807220459]]
;;       ]
;;   ;; (println! 
;;   ;;  (get-shadow-transform world position)
;;   ;;  )
;;   (println! (get-collision-normal collision) )
;;   )

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

(defn avatar-mode-draw-3d! [world]
  (let [avatar (:avatar world)
        state (:state avatar)]
    (if (not= state :vision)
      (let [position (:position avatar)
            angle (:angle avatar)
            rotation [0 1 0 angle]
            transform (make-transform position rotation)
            pose-index (:pose-index avatar)
            mesh (get-in avatar [:poses state pose-index])
            mesh (assoc-in mesh [:transform] transform)]
        (draw-mesh! world mesh)
        (draw-shadow! world)))))

(defn normalize-cameraman [world]
  (let [avatar (:avatar world)
        avatar-position (:position avatar)
        cameraman (:cameraman world)
        cameraman-position (:position cameraman)
        to-cameraman (vector-normalize
                      (vector-subtract
                       cameraman-position avatar-position))
        to-cameraman (vector-multiply to-cameraman (:distance cameraman))
        new-cameraman-position (vector-add avatar-position to-cameraman)]
    (assoc-in world [:cameraman :position] new-cameraman-position)))

(defn set-view [world]
  (let [avatar (:avatar world)
        state (:state avatar)]
    (if (= state :vision)
      (let [position (vector-add (:position avatar) [0 0.1 0])
            y-angle (:angle avatar)
            direction (vector-rotate [0 0 1] [0 1 0] y-angle)
            x-axis (vector-rotate [1 0 0] [0 1 0] y-angle)
            direction (vector-rotate direction x-axis (:head-angle avatar))
            p1 (vector-add position (vector-multiply direction -5))
            p2 (vector-add position direction)
            matrix (get-look-at-matrix p1 p2 [0 1 0])]
        (assoc-in world [:view-matrix] matrix))
      (let [cameraman (:cameraman world)
            avatar-position (:position avatar)
            cameraman-position (:position cameraman)
            cameraman-position (assoc cameraman-position
                                      1 (:height cameraman))
            matrix (get-look-at-matrix cameraman-position
                                       avatar-position [0 1 0])]
        (assoc-in world [:view-matrix] matrix)))))

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

(defn avatar-mode-entered [world]
  (let [parts (:parts world)
        blocks (vec (map first (filter is-solid-part? parts)))]
    (-> world
        (assoc-in [:block-names] (conj blocks :ground))
        (change-state :running))))

(defn avatar-mode-exited [world]
  (rotate-camera world 0 0))

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
        (let [world (let [part (get-in world [:parts part-name])]
                      (if (in? (:type part) [:button :block :cylinder :cone :sphere])
                        (-> world
                            (assoc-in [:parts part-name :value] 1)
                            (assoc-in [:pressed-part] part-name))
                        world))]
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
          {:keys [part-name local-point]} avatar-force
          rope-length (:distance avatar-force)
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
        world))))

(defn avatar-mode-update [world elapsed]
  (reactivate-avatar! world elapsed)

  (-> world
      (update-state)
      (update-avatar-force)
      (update-in [:avatar :keys] update-keys)
      (normalize-cameraman)
      (set-view)
      ))
