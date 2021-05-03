
;; (defn get-weld-parent-part [world part-name]
;;   (let [weld-groups (:weld-groups world)]
;;     (find-if (fn [name]
;;                (let [children (get-in weld-groups [name :children])]
;;                  (in? part-name (keys children))))
;;              (keys weld-groups))))

(defn get-function-segment [function t]
  (cond
    (<= t 0.0) [(nth function 0) (nth function 1)]

    (>= t 1.0)
    (let [n (count function)]
      [(nth function (- n 2)) (nth function (- n 1))])

    :else
    (let [pairs (map vector function (rest function))]
      (find-if (fn [[[t0 & _] [t1 & _]]]
                 (<= t0 t t1))
               pairs))))

(defn get-wagon-direction [world wagon-name]
  (let [w2 (set-value-0-transform world wagon-name)
        wagon (get-in w2 [:parts wagon-name])
        loop-fn (compute-translated-loop-fn (:loop-fn wagon))
        transform (:transform wagon)
        loop-fn (map (fn [[t v]]
                       [t (apply-transform transform v)])
                     loop-fn)
        value (within (:value wagon) 0.0 1.0)
        [[_ p0] [_ p1]] (get-function-segment loop-fn value)]
    (vector-normalize (vector-subtract p1 p0))))

;; (defn apply-force-to-wagon [world elapsed]
;;   (if-let [{:keys [part-name velocity line point]} (:force world)]
;;     (let [key (if (:use-weld-groups world)
;;                 :weld-groups
;;                 :parts)
;;           wagon (get-in world [key part-name])
;;           transform (:transform wagon)
;;           p1 (apply-transform transform point)
;;           p2 (point-line-projection p1 line)
;;           force-vector (vector-subtract p2 p1)
;;           track-direction (get-wagon-direction world part-name)
;;           force-component (/ (vector-dot-product force-vector track-direction)
;;                              (vector-length track-direction))
;;           acceleration (* force-component 100)
;;           value (get-in world [:parts part-name :value])
;;           dt (* elapsed 0.001)
;;           dv (* acceleration dt)
;;           dampening-factor 0.80
;;           dampening-factor 1.0 ;;############################
;;           velocity (* (+ velocity dv) dampening-factor)
;;           dvalue (* velocity dt)
;;           value (+ value dvalue)]
;;       (-> world
;;           (assoc-in [:parts part-name :value] (within value 0 1))
;;           (assoc-in [:force :velocity] velocity)))
;;     world))

;; (defn apply-force-to-track [world elapsed]
;;   (if-let [{:keys [part-name velocity line point]} (:force world)]
;;     (let [track (get-in world [:weld-groups part-name])
;;           transform (:transform track)
;;           p1 (apply-transform transform point)
;;           p2 (point-line-projection p1 line)
;;           force-vector (vector-subtract p2 p1)
;;           rotation (get-rotation-component transform)
;;           track-direction (apply-transform rotation [0 1 0])
;;           track-position (get-transform-position transform)
;;           track-line [track-position track-direction]
;;           p3 (point-line-projection p1 track-line)
;;           arm-vector (vector-subtract p1 p3)
;;           ;; theta (vector-angle arm-vector force-vector track-direction)
;;           ;; r (vector-length arm-vector)
;;           ;; F (vector-length force-vector)
;;           ;; force-component (* r F (sin theta))
;;           ;; acceleration (* force-component 10)
;;           sign (if (pos? (vector-dot-product
;;                           (vector-cross-product arm-vector force-vector)
;;                           track-direction))
;;                  1
;;                  -1)
;;           acceleration (* sign 20 (vector-length force-vector))
;;           value (get-in world [:parts part-name :value])
;;           dt (* elapsed 0.001)
;;           dv (* acceleration dt)
;;           dampening-factor 0.7
;;           velocity (* (+ velocity dv) dampening-factor)
;;           dvalue (* velocity dt)
;;           value (+ value dvalue)
;;           max-angle (get-in world [:parts part-name :max-angle])]
;;       (-> world
;;           (assoc-in [:parts part-name :value] (if (nil? max-angle)
;;                                                 value
;;                                                 (within value 0 max-angle)))
;;           (assoc-in [:force :velocity] velocity)))
;;     world))

;; (defn apply-force [world elapsed]
;;   (if-let [part-name (get-in world [:force :part-name])]
;;     (let [part (get-in world [:parts part-name])]
;;       (case (:type part)
;;         :wagon (apply-force-to-wagon world elapsed)
;;         :track (apply-force-to-track world elapsed)
;;         world))
;;     world))

(defn get-first-dof [world part-name]
  (let [part (get-in world [:parts part-name])]
    (if (:free part)
      part-name
      (let [parent-name (get-parent-part world part-name)]
        (if (= parent-name :ground-part)
          nil
          (recur world parent-name))))))

(defn get-scaled-transform [scale transform]
  (let [[sx sy sz] scale
        scale-matrix (get-scale-matrix sx sy sz)
        other-matrix (get-transform-matrix transform)
        final-matrix (multiply-matrices scale-matrix other-matrix)]
    (matrix->transform final-matrix)))

(defn create-all-pairs [elements]
  (let [n (count elements)]
    (vec (mapcat (fn [i]
                   (map (fn [j]
                          [(nth elements i) (nth elements j)])
                        (range (inc i) n)))
                 (range n)))))

(defn blocks-collide? [world a-name b-name]
  (if (= (get-first-dof world a-name)
         (get-first-dof world b-name))
    false
    (let [a-block (get-in world [:parts a-name])
          b-block (get-in world [:parts b-name])
          model (get-in world [:info :block :model])
          vertices [[-0.5 0.5 0.5] [0.5 0.5 0.5]
                    [-0.5 -0.5 0.5] [0.5 -0.5 0.5]
                    [-0.5 0.5 -0.5] [0.5 0.5 -0.5]
                    [-0.5 -0.5 -0.5] [0.5 -0.5 -0.5]]

          a-transform (get-scaled-transform
                       (get-in world [:parts a-name :scale])
                       (use-root-relative-transform world a-name))
          
          b-transform (get-scaled-transform
                       (get-in world [:parts b-name :scale])
                       (use-root-relative-transform world b-name))
          
          ia-transform (get-inverse-transform a-transform)
          ib-transform (get-inverse-transform b-transform)
          a->b-transform (combine-transforms a-transform ib-transform)
          b->a-transform (combine-transforms b-transform ia-transform)
          a-vertices (map #(apply-transform b->a-transform %) vertices)
          b-vertices (map #(apply-transform a->b-transform %) vertices)
          all-vertices (concat a-vertices b-vertices)]
      (some (fn [[x y z]]
              (and
               (<= -0.5 x 0.5)
               (<= -0.5 y 0.5)
               (<= -0.5 z 0.5)))
            all-vertices))))

(defn get-colliding-pairs [world]
  (let [block-relative-transforms
        (filter (fn [[name value]]
                  (= (:type value) :block))
                (:root-relative-transforms world))
        block-names (keys block-relative-transforms)]
    (filter (fn [[a b]]
              (blocks-collide? world a b))
            (create-all-pairs block-names))))

(defn get-force-pair [world a-name b-name]
  (let [a-transform (get-scaled-transform
                     (get-in world [:parts a-name :scale])
                     (use-root-relative-transform world a-name))
        b-transform (get-scaled-transform
                     (get-in world [:parts b-name :scale])
                     (use-root-relative-transform world b-name))
        a-position (get-transform-position a-transform)
        b-position (get-transform-position b-transform)
        v (vector-subtract a-position b-position)
        force-function (fn [d]
                         ;; (if (> d 0.55)
                         ;;   0
                           (* 3 (pow Math/E (* 2 (- d)))))
        v (vector-multiply (vector-normalize v)
                           (force-function (vector-length v)))
        midpoint (vector-multiply (vector-add a-position b-position) 0.5)

        a-dof (or (get-first-dof world a-name) :ground-part)
        a-dof-transform (get-in world [:weld-groups a-dof :transform])
        a-dof-inverse-transform (get-inverse-transform a-dof-transform)
        a-dof-local-point (apply-transform a-dof-inverse-transform midpoint)

        b-dof (or (get-first-dof world b-name) :ground-part)
        b-dof-transform (get-in world [:weld-groups b-dof :transform])
        b-dof-inverse-transform (get-inverse-transform b-dof-transform)
        b-dof-local-point (apply-transform b-dof-inverse-transform midpoint)
        ]
    [{:part-name a-dof
      :local-point a-dof-local-point
      :vector v}
     {:part-name b-dof
      :local-point b-dof-local-point
      :vector (vector-multiply v -1)}
     ]))
  
(defn get-collision-forces [world]
  (let [pairs (get-colliding-pairs world)]
    (flatten (map (fn [[a b]]
                    (get-force-pair world a b))
                  pairs))))

(defn apply-force-to-wagon [world force elapsed]
  (let [{:keys [part-name vector]} force
        track-direction (get-wagon-direction world part-name)
        force-component (/ (vector-dot-product vector track-direction)
                           (vector-length track-direction))
        acceleration (* force-component 500)
        value (get-in world [:parts part-name :value])
        dt (* elapsed 0.001)
        dv (* acceleration dt)
        dvalue (* dv dt)
        value (+ value dvalue)]
    (assoc-in world [:parts part-name :value] (within value 0 1))))

(defn apply-force-to-track [world force elapsed]
  (if-let [{:keys [part-name vector local-point]} force]
    (let [key (if (:use-weld-groups world)
                :weld-groups
                :parts)
          track (get-in world [key part-name])
          transform (:transform track)
          p1 (apply-transform transform local-point)
          track-direction (get-track-direction track)
          track-position (get-transform-position transform)
          track-line [track-position track-direction]
          p2 (point-line-projection p1 track-line)
          arm-vector (vector-subtract p1 p2)
          sign (if (pos? (vector-dot-product
                          (vector-cross-product arm-vector vector)
                          track-direction)) 1 -1)
          perpendicular (vector-cross-product arm-vector track-direction)
          vector (vector-project vector perpendicular)
          acceleration (* sign 100 (vector-length vector))
          value (get-in world [:parts part-name :value])
          dt (* elapsed 0.001)
          dv (* acceleration dt)
          dvalue (* dv dt)
          value (+ value dvalue)
          max-angle (get-in world [:parts part-name :max-angle])]
      (assoc-in world [:parts part-name :value]
                (if (nil? max-angle)
                  value
                  (within value 0 max-angle))))
    world))

(defn apply-force [world force elapsed]
  (if-let [part-name (:part-name force)]
    (let [part (get-in world [:parts part-name])]
      (case (:type part)
        :wagon (apply-force-to-wagon world force elapsed)
        :track (apply-force-to-track world force elapsed)
        world))
    world))

(defn update-mouse-force [world]
  (if-let [mouse-force (:mouse-force world)]
    (let [{:keys [part-name local-point line]} mouse-force
          key (if (:use-weld-groups world)
                :weld-groups
                :parts)
          part (get-in world [key part-name])
          transform (:transform part)
          p1 (apply-transform transform local-point)
          p2 (point-line-projection p1 line)
          force-vector (vector-subtract p2 p1)]
      (assoc-in mouse-force [:vector] force-vector))
    nil))

(defn apply-forces [world elapsed]
  (let [mouse-force (update-mouse-force world)
        avatar-force (get-in world [:avatar :force])
        forces (remove-nil (concat [mouse-force avatar-force]
                                 ;; (get-collision-forces world)
                                   ))]
    (reduce (fn [w force]
              (apply-force w force elapsed))
            world
            forces)))

;; (defn get-scaled-transform [scale transform]
;;   (let [[sx sy sz] scale
;;         scale-matrix (get-scale-matrix sx sy sz)
;;         other-matrix (get-transform-matrix transform)
;;         final-matrix (multiply-matrices scale-matrix other-matrix)]
;;     (matrix->transform final-matrix)))

;; (defn create-all-pairs [elements]
;;   (let [n (count elements)]
;;     (vec (mapcat (fn [i]
;;                    (map (fn [j]
;;                           [(nth elements i) (nth elements j)])
;;                         (range (inc i) n)))
;;                  (range n)))))

;; (defn blocks-collide? [world a-name b-name]
;;   ;;############################################## include edges
;;   (let [a-parent (get-parent-part world a-name)
;;         b-parent (get-parent-part world a-name)]
;;     (if (or (= a-parent b-name)
;;             (= b-parent a-name))
;;       false
;;       (let [a-block (get-in world [:parts a-name])
;;             b-block (get-in world [:parts b-name])
;;             model (get-in world [:info :block :model])
;;             vertices [[-0.5 0.5 0.5] [0.5 0.5 0.5]
;;                       [-0.5 -0.5 0.5] [0.5 -0.5 0.5]
;;                       [-0.5 0.5 -0.5] [0.5 0.5 -0.5]
;;                       [-0.5 -0.5 -0.5] [0.5 -0.5 -0.5]]

;;             a-transform (get-scaled-transform
;;                          (get-in world [:parts a-name :scale])
;;                          (use-root-relative-transform world a-name))
            
;;             b-transform (get-scaled-transform
;;                          (get-in world [:parts b-name :scale])
;;                          (use-root-relative-transform world b-name))
            
;;             ia-transform (get-inverse-transform a-transform)
;;             ib-transform (get-inverse-transform b-transform)
;;             a->b-transform (combine-transforms a-transform ib-transform)
;;             b->a-transform (combine-transforms b-transform ia-transform)
;;             a-vertices (map #(apply-transform b->a-transform %) vertices)
;;             b-vertices (map #(apply-transform a->b-transform %) vertices)
;;             all-vertices (concat a-vertices b-vertices)]
;;         (some (fn [[x y z]]
;;                 (and
;;                  (<= -0.5 x 0.5)
;;                  (<= -0.5 y 0.5)
;;                  (<= -0.5 z 0.5)))
;;               all-vertices)))))

;; (defn get-colliding-pairs [world]
;;   (let [block-relative-transforms
;;         (filter (fn [[name value]]
;;                   (= (:type value) :block))
;;                 (:root-relative-transforms world))
;;         block-names (keys block-relative-transforms)]
;;     (filter (fn [[a b]]
;;               (blocks-collide? world a b))
;;             (create-all-pairs block-names))))

;; (defn reverse-collision [world [a b]]
;;   (if-let [dof-name (or (get-first-dof world a)
;;                         (get-first-dof world b))]
;;     (let [part (get-in world [:parts dof-name])]
;;       (assoc-in world [:parts dof-name :value] (:saved-value part)))
;;     world))
  
;; (defn reverse-collisions [world]
;;   (if (= (:mode world) :property)
;;     world
;;     (if-let [pairs (get-colliding-pairs world)]
;;       (let [world (reduce (fn [w pair]
;;                             (reverse-collision world pair))
;;                           world
;;                           pairs)]
;;         (compute-transforms world
;;                             (if (:use-weld-groups world)
;;                               :weld-groups
;;                               :parts)))
;;       world)))

;; (defn save-values [world]
;;   (let [parts (map-map (fn [[name part]]
;;                          {name
;;                           (assoc-in part [:saved-value] (:value part))})
;;                        (:parts world))]
;;     (assoc-in world [:parts] parts)))
