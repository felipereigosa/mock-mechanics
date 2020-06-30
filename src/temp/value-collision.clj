
(ns temp.core)

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
  ;;############################################## include edges
  (let [a-parent (get-parent-part world a-name)
        b-parent (get-parent-part world a-name)]
    (if (or (= a-parent b-name)
            (= b-parent a-name))
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
              all-vertices)))))

(defn get-colliding-pairs [world]
  (let [block-relative-transforms
        (filter (fn [[name value]]
                  (= (:type value) :block))
                (:root-relative-transforms world))
        block-names (keys block-relative-transforms)]
    (filter (fn [[a b]]
              (blocks-collide? world a b))
            (create-all-pairs block-names))))

(defn reverse-collision [world [a b]]
  (if-let [dof-name (or (get-first-dof world a)
                        (get-first-dof world b))]
    (let [part (get-in world [:parts dof-name])]
      (assoc-in world [:parts dof-name :value] (:saved-value part)))
    world))
  
(defn reverse-collisions [world]
  (if (= (:mode world) :property)
    world
    (if-let [pairs (get-colliding-pairs world)]
      (let [world (reduce (fn [w pair]
                            (reverse-collision world pair))
                          world
                          pairs)]
        (compute-transforms world
                            (if (:use-weld-groups world)
                              :weld-groups
                              :parts)))
      world)))

(defn save-values [world]
  (let [parts (map-map (fn [[name part]]
                         {name
                          (assoc-in part [:saved-value] (:value part))})
                       (:parts world))]
    (assoc-in world [:parts] parts)))


