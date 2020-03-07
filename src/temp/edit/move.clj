
(ns temp.core)

(defn get-block-plane [block normal]
  (let [transform (:transform block)
        rotation-transform (get-rotation-component transform)
        inverse-rotation (get-inverse-transform rotation-transform)
        normal (vector-normalize (apply-transform inverse-rotation normal))
        [a1 a2] (map #(apply-transform rotation-transform %)
                     (filter #(float= 0.0 (vector-dot-product % normal))
                             [[1 0 0] [0 1 0] [0 0 1]]))
        a (vector-add
           (get-transform-position transform)
           (apply-transform rotation-transform
                            (map #(* %1 %2 0.5) normal
                                 (:scale block))))
        b (vector-add a a1)
        c (vector-add a a2)]
    [a b c]))

(defn get-track-plane [track]
  (let [transform (:transform track)
        rotation-transform (get-rotation-component transform)
        x (apply-transform rotation-transform [1 0 0])
        z (apply-transform rotation-transform [0 0 1])
        origin (get-transform-position transform)]
    [origin
     (vector-add origin x)
     (vector-add origin z)]))

(defn move-part-pressed [world part-name point]
  (let [part-position (get-part-position world part-name)
        part (get-in world [:parts part-name])
        vx (apply-rotation (:transform part) [1 0 0])
        vy (apply-rotation (:transform part) [0 1 0])
        vz (apply-rotation (:transform part) [0 0 1])
        point (or point
                  (vector-subtract
                   part-position
                   (vector-multiply vy (get-part-offset part))))
        parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
                offset (vector-subtract part-position point)
        plane (case (:type parent)
                :ground
                [[0.25 0 0.25] [1.25 0 0.25] [0.25 0 1.25]]

                :track
                (get-track-plane parent)

                (get-block-plane parent vy))
        
        y-offset (vector-multiply vy (point-plane-distance point plane))
        plane (map #(vector-add % y-offset) plane)
        xz-offset (vector-subtract offset (vector-project offset vy))
        plane (map #(vector-subtract % xz-offset) plane)]
    (-> world
        (assoc-in [:edited-part] part-name)
        (create-weld-groups)
        (assoc-in [:plane] plane)
        (assoc-in [:offset] offset))))

(defn move-part-moved [world {:keys [x y]} & {:keys [grain]}]
  (if-let [part-name (:edited-part world)]
    (let [line (unproject-point world [x y])
          plane (:plane world)
          point (line-plane-intersection line plane)
          offset (:offset world)
          grain-size (or grain
                         (if (:shift-pressed world)
                           0.25
                           0.05))
          point (get-normalized-plane-point plane point grain-size)
          point (vector-add point offset)]
      (update-in world [:parts part-name]
                 #(set-part-position % point)))
    world))

(defn move-part-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)
          position (get-part-position world part-name)]
      (-> world
          (set-value-0-transform part-name)
          (update-in [:parts part-name]
                     #(set-part-position % position))
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))

(defn move-mode-pressed [world {:keys [x y]}]
  (if-let [{:keys [part-name point]} (get-part-collision world x y)]
    (let [type (get-in world [:parts part-name :type])]
      (if (= type :wagon)
        (do
          (println! "can't move wagon")
          world)
        (move-part-pressed world part-name point)))
    world))

(defn move-mode-moved [world event]
  (move-part-moved world event))

(defn move-mode-released [world event]
  (move-part-released world event))
