
(ns temp.core (:gen-class))

(defn set-block-size [world block-name original-scale
                      original-center increase]
  (let [block (get-in world [:parts block-name])
        new-scale (map (fn [a b]
                         (if (zero? b)
                           a
                           (abs b)))
                       original-scale increase)
        scale-change (vector-subtract new-scale original-scale)
        value (if (some neg? increase)
                -0.5
                0.5)
        part-rotation (get-transform-rotation (:transform block))
        rotation-transform (make-transform [0 0 0] part-rotation)
        offset (apply-transform rotation-transform
                                (vector-multiply scale-change value))
        new-center (vector-add original-center offset)
        new-transform (make-transform new-center part-rotation)]
    (-> world
        (assoc-in [:parts block-name :scale] (map abs new-scale))
        (assoc-in [:parts block-name :transform] new-transform))))

(do
1

(defn get-child-local-direction [world parent-name child-name]
  (let [parent (get-in world [:parts parent-name])
        relative-transform (get-in parent [:children child-name])
        rotation (get-rotation-component relative-transform)]
    (apply-transform rotation [0 1 0])))

(defn save-moveable-children-transforms [world normal part-name]
  (let [children (get-in world [:parts part-name :children])
        moving-children (filter (fn [child-name]
                                  (let [direction (get-child-local-direction world part-name child-name)]
                                    (vector= direction normal)))                                
                                (keys children))]
    (assoc-in world [:saved-children-transforms]
              (select-keys children moving-children))))

(defn scale-block-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            vertices (get-in world [:info :block :model :vertices])
            triangles (partition 3 (partition 3 vertices))
            [a b c] (nth triangles index)
            v1 (vector-subtract b a)
            v2 (vector-subtract c a)
            normal (vector-cross-product v1 v2)
            rotation-transform (get-rotation-component (:transform part))
            v (apply-transform rotation-transform normal)
            scale (:scale part)
            center (get-transform-position (:transform part))]
        (-> world
            (assoc-in [:edited-part] part-name)
            (assoc-in [:adjust-line] [point (vector-normalize v)])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)
            (assoc-in [:normal] normal)
            (save-moveable-children-transforms normal part-name)))
        world)))

(defn move-children [world part-name]
  (let [part (get-in world [:parts part-name])
        v (vector-multiply (:scale-offset world) 0.5)
        rotation (get-rotation-component (:transform part))
        v (apply-transform (get-inverse-transform rotation) v)
        offset-transform (make-transform v [1 0 0 0])]
    (reduce (fn [w child-name]
              (if-let [old-transform (get (:saved-children-transforms world)
                                          child-name)]
                (let [new-transform (combine-transforms old-transform
                                                        offset-transform)]
                  (assoc-in w [:parts part-name :children child-name]
                                new-transform))
                (create-relative-transform w child-name part-name)))
            world
            (keys (:children part)))))

(defn scale-block-moved [world event]
  (if-let [block-name (:edited-part world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          grain-size (if (:shift-pressed world) 0.5 0.1)
          d (snap-value d grain-size)
          scale (:original-scale world)
          center (:original-center world)
          normal (:normal world)
          l1 (+ d (abs (reduce + (map * normal scale))))
          l2 (within l1 0.1 10)
          increase-vector (map * (:normal world) [l2 l2 l2])
          scale-offset (vector-multiply (second adjust-line) d)]
      (user-message! (format "side: %.2f" l2))
      (-> world
          (set-block-size block-name scale center increase-vector)
          (assoc-in [:scale-offset] scale-offset)
          (#(if (float= l1 l2)
             (move-children % block-name)
             %))))
    world))

(defn scale-block-released [world event]
  (if-let [block-name (:edited-part world)]
    (let [parent-name (get-parent-part world block-name)]
      (-> world
          (create-relative-transform block-name parent-name)
          (dissoc-in [:edited-part])))
    world))
)

(defn set-track-size [world track-name original-scale original-center height]
  (let [track (get-in world [:parts track-name])
        new-scale (assoc original-scale 1 height)
        scale-change (vector-subtract new-scale original-scale)
        part-rotation (get-transform-rotation (:transform track))
        rotation-transform (make-transform [0 0 0] part-rotation)
        new-center (->> scale-change
                        (apply-transform rotation-transform)
                        (vector-add original-center))
        new-transform (make-transform new-center part-rotation)]
    (-> world
        (assoc-in [:parts track-name :scale] new-scale)
        (assoc-in [:parts track-name :transform] new-transform))))

(defn scale-track-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point _]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            scale (:scale part)
            transform (:transform part)
            center (get-transform-position transform)
            rotation-transform (get-rotation-component transform)
            v (apply-transform rotation-transform [0 1 0])]
        (-> world
            (assoc-in [:edited-part] part-name)
            (assoc-in [:adjust-line] [point (vector-normalize v)])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)))
      world)))

(defn scale-track-moved [world event]
  (if-let [track-name (:edited-part world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          grain-size (if (:shift-pressed world) 0.5 0.1)
          d (snap-value d grain-size)
          scale (:original-scale world)
          center (:original-center world)
          normal (second adjust-line)
          l (within (+ (apply max scale) d) grain-size 10)]
      (user-message! (format "length: %.2f" l))
      (-> world
          (set-track-size track-name scale center l)
          (assoc-in [:track-length] l)))
    world))

(defn scale-track-released [world event]
  (if-let [track-name (:edited-part world)]
    (let [parent-name (get-parent-part world track-name)
          scale (:original-scale world)
          world (-> world
                    (assoc-in [:parts track-name :scale] scale)
                    (set-value-0-transform track-name))
          center (get-part-position world track-name)
          track-length (:track-length world)]
      (-> world
          (set-track-size track-name scale center track-length)
          (create-relative-transform track-name parent-name)
          (dissoc-in [:edited-part])))
    world))

(defn scale-cylinder-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]}
             (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            inverse-transform (get-inverse-transform (:transform part))
            local-point (apply-transform inverse-transform point)
            half-height (/ (second (:scale part)) 2)
            [lx ly lz] local-point
            local-normal (cond
                           (float= ly half-height) [0 1 0]
                           (float= ly (- half-height)) [0 -1 0]
                           :else (vector-normalize [lx 0 lz]))
            rotation-transform (get-rotation-component (:transform part))
            v (apply-transform rotation-transform local-normal)
            scale (:scale part)
            center (get-transform-position (:transform part))]
        (-> world
            (assoc-in [:edited-part] part-name)
            (assoc-in [:adjust-line] [point v])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)
            (assoc-in [:normal] local-normal)
            (save-moveable-children-transforms local-normal part-name)))
        world)))

(defn scale-cylinder-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          scale (:original-scale world)
          center (:original-center world)
          normal (:normal world)]
      (if (vector= (vector-cross-product normal [0 1 0]) [0 0 0])
        (let [grain-size (if (:shift-pressed world) 0.5 0.1)
              d (snap-value d grain-size)
              l1 (+ d (second scale))
              l2 (max l1  0.1)
              scale-offset (vector-multiply (second adjust-line) d)]
          (user-message! (format "height: %.2f" l2))
          (-> world
              (set-block-size part-name scale center [0 (* l2 (second normal)) 0])
              (assoc-in [:scale-offset] scale-offset)
              (#(if (float= l1 l2)
                  (move-children % part-name)
                  %))))
        (let [grain-size (if (:shift-pressed world) 0.25 0.05)
              d (snap-value d grain-size)
              d2 (max (+ (first scale) (* d 2)) 0.1)
              new-scale [d2 (second scale) d2]]
          (user-message! (format "diameter: %.2f" d2))
          (assoc-in world [:parts part-name :scale] new-scale))))
    world))

(defn scale-cylinder-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)]
      (-> world
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))

(defn scale-cone-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]}
             (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            inverse-transform (get-inverse-transform (:transform part))
            local-point (apply-transform inverse-transform point)
            half-height (/ (second (:scale part)) 2)
            [lx ly lz] local-point
            local-normal (if (pos? ly)
                           [0 1 0]
                           (vector-normalize [lx 0 lz]))
            rotation-transform (get-rotation-component (:transform part))
            v (apply-transform rotation-transform local-normal)
            scale (:scale part)
            center (get-transform-position (:transform part))]
        (-> world
            (assoc-in [:edited-part] part-name)
            (assoc-in [:adjust-line] [point v])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)
            (assoc-in [:normal] local-normal)))
        world)))

(defn scale-cone-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          scale (:original-scale world)
          center (:original-center world)]
      (if (= (:normal world) [0 1 0])
        (let [grain-size (if (:shift-pressed world) 0.5 0.1)
              d (snap-value d grain-size)
              l (max (+ d (second scale)) 0.1)]
          (user-message! (format "height: %.2f" l))
          (set-block-size world part-name scale center [0 l 0]))
        (let [grain-size (if (:shift-pressed world) 0.25 0.05)
              d (snap-value d grain-size)
              d2 (max (+ (first scale) (* d 2)) 0.1)
              new-scale [d2 (second scale) d2]]
          (user-message! (format "diameter: %.2f" d2))
          (assoc-in world [:parts part-name :scale] new-scale))))
    world))

(defn scale-cone-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)]
      (-> world
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))

(defn scale-sphere-pressed [world {:keys [x y]}]
  (if-let [{:keys [part-name point index]}
           (get-part-collision world x y)]
    (let [center (get-part-position world part-name)
          v (vector-normalize (vector-subtract point center))]
      (-> world
          (assoc-in [:edited-part] part-name)
          (assoc-in [:adjust-line] [center v])))
    world))

(defn scale-sphere-moved [world {:keys [x y]}]
  (if-let [part-name (:edited-part world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [x y])
          d (line-line-closest-point adjust-line mouse-line)
          grain-size (if (:shift-pressed world) 0.25 0.05)
          d (snap-value d grain-size)
          d2 (within (* d 2) 0.1 10)
          new-scale (vec (repeat 3 d2))]
      (user-message! (format "diameter: %.2f" d2))
      (assoc-in world [:parts part-name :scale] new-scale))
    world))

(defn scale-sphere-released [world event]
  (dissoc-in world [:edited-part]))

(defn scale-mode-pressed [world event]
  (if-let [part-name (get-part-at world (:x event) (:y event))]
    (let [type (get-in world [:parts part-name :type])
          world (assoc-in world [:scale-type] type)]
      (case type
        :block (scale-block-pressed world event)
        :track (scale-track-pressed world event)
        :cylinder (scale-cylinder-pressed world event)
        :cone (scale-cone-pressed world event)
        :sphere (scale-sphere-pressed world event)
        (do
          (user-message! "can't scale" (kw->str type))
          world)))
    world))

(defn scale-mode-moved [world event]
  (case (:scale-type world)
    :block (scale-block-moved world event)
    :track (scale-track-moved world event)
    :cylinder (scale-cylinder-moved world event)
    :cone (scale-cone-moved world event)
    :sphere (scale-sphere-moved world event)
    world))

(defn scale-mode-released [world event]
  (let [world (case (:scale-type world)
                :block (scale-block-released world event)
                :track (scale-track-released world event)
                :cylinder (scale-cylinder-released world event)
                :cone (scale-cone-released world event)
                :sphere (scale-sphere-released world event)
                world)]
    (dissoc-in world [:scale-type])))

