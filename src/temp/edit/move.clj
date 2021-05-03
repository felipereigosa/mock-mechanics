
(defn get-block-plane [block normal]
  (let [transform (:transform block)
        rotation-transform (get-rotation-component transform)
        inverse-rotation (get-inverse-transform rotation-transform)
        local-normal (vector-normalize (apply-transform inverse-rotation normal))
        [a1 a2] (->> [[1 0 0] [0 1 0] [0 0 1]]
                     (map (fn [v]
                            [(abs (vector-dot-product v local-normal)) v]))
                     (sort-by first)
                     (take 2)
                     (map second)
                     (map #(apply-transform rotation-transform %)))
        n2 (vector-cross-product a2 a1)
        [a1 a2] (if (neg? (vector-dot-product normal n2))
                  [a2 a1]
                  [a1 a2])
        a (vector-add
           (get-transform-position transform)
           (apply-transform rotation-transform
                            (map #(* %1 %2 0.5) local-normal
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
        transform (:transform part)
        vx (apply-rotation transform [1 0 0])
        vy (apply-rotation transform [0 1 0])
        vz (apply-rotation transform [0 0 1])
        point (or point
                  (vector-subtract
                   part-position
                   (vector-multiply vy (get-part-offset part))))
        parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        offset (vector-subtract part-position point)
        original-plane (case (:type parent)
                         :ground [[0.25 0 0.25] [1.25 0 0.25] [0.25 0 1.25]]
                         :track (get-track-plane parent)
                         (get-block-plane parent vy))
        y-offset (vector-multiply vy (point-plane-distance point original-plane))
        plane (map #(vector-add % y-offset) original-plane)
        xz-offset (vector-subtract offset (vector-project offset vy))
        plane (map #(vector-subtract % xz-offset) plane)]
    (-> world
        (assoc-in [:edited-part] part-name)
        (assoc-in [:plane] plane)
        (assoc-in [:original-plane] original-plane)
        (assoc-in [:offset] offset))))

(defn move-part-moved [world event & {:keys [grain]}]
  (if-let [part-name (:edited-part world)]
    (let [line (get-spec-line world event)
          plane (:plane world)
          point (line-plane-intersection line plane)
          offset (:offset world)
          grain-size (or grain
                         (cond
                           (:shift-pressed world) 0.25
                           (:control-pressed world) 0.01
                           :else 0.05))
          point (get-normalized-plane-point plane point grain-size)
          v (vector-subtract point (first plane))
          point (vector-add point offset)
          [a b c] (:original-plane world)
          v1 (vector-subtract b a)
          v2 (vector-subtract c a)
          v (vector-subtract point a)
          ox (vector-scalar-projection v v1)
          oy (vector-scalar-projection v v2)]
      (user-message! "x = " (format "%.2f" ox)
                     ", y = " (format "%.2f" oy))
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

(declare move-mode-moved)

(defn move-mode-pressed [world event]
  (if-let [{:keys [part-name point]} (get-part-collision world event)]
    (let [type (get-in world [:parts part-name :type])]
      (if (= type :wagon)
        (do
          (user-message! "can't move wagon")
          world)
        (-> world
            (move-part-pressed part-name point)
            (move-mode-moved event))))
    world))

(defn move-mode-moved [world event]
  (move-part-moved world event))

(defn move-mode-released [world event]
  (move-part-released world event))
