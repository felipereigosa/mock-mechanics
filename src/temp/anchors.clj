
(ns temp.core)

(defn anchor->transform [offset anchor parent]
  (let [final-rotation (:rotation anchor)
        rotation-transform (make-transform [0 0 0] final-rotation)
        offset [0 offset 0]
        offset (apply-transform rotation-transform offset)
        final-point (if (= (:type parent) :track)
                      (get-transform-position (:transform parent))
                      (:position anchor))
        final-point (vector-add final-point offset)]
    (make-transform final-point final-rotation)))

(defn get-normalized-plane-point [plane point grain]
  (let [[a b c] plane
        v1 (vector-subtract b a)
        v2 (vector-subtract c a)
        s (point-line-coordinate point [a v1])
        t (point-line-coordinate point [a v2])
        s (* grain (round (/ s grain)))
        t (* grain (round (/ t grain)))]
    (reduce vector-add [a
                        (vector-multiply v1 s)
                        (vector-multiply v2 t)])))

(defn get-block-plane [block normal]
  (let [transform (:transform block)
        rotation-transform (get-rotation-component transform)
        normal (vector-normalize normal)
        [a1 a2] (map #(apply-transform rotation-transform %)
                     (filter #(float-equals? 0.0 (vector-dot-product % normal))
                             [[1 0 0] [0 1 0] [0 0 1]]))
        a (vector-add
           (get-transform-position transform)
           (apply-transform rotation-transform
                            (map #(* %1 %2 0.5) normal
                                 (:scale block))))
        b (vector-add a a1)
        c (vector-add a a2)]
    [a b c]))

(defn get-block-anchor-point [world collision]
  (let [block-name (:part-name collision)
        block (get-in world [:parts block-name])
        local-normal (get-collision-normal world collision)
        plane (get-block-plane block local-normal)
        transform (:transform block)
        rotation-transform (get-rotation-component transform)
        global-normal (apply-transform rotation-transform local-normal)
        rotation (quaternion-from-normal global-normal)
        point (get-normalized-plane-point plane (:point collision) 0.2)]
    {:position point
     :rotation rotation
     :part block-name}))

(defn get-track-anchor-point [world collision]
  (let [track-name (:part-name collision)
        track (get-in world [:parts track-name])
        local-normal (get-collision-normal world collision)
        transform (:transform track)
        rotation-transform (get-rotation-component transform)
        global-normal (apply-transform rotation-transform local-normal)
        rotation (quaternion-from-normal global-normal)]
    {:position [0 0 0]
     :rotation rotation
     :part track-name}))

(defn get-ground-anchor-point [world x y]
  (let [plane [[0.25 0 0.25] [1.25 0 0.25] [0.25 0 1.25]]
        line (unproject-point world [x y])
        point (line-plane-intersection line plane)
        point (get-normalized-plane-point plane point 0.5)]
    {:position point
     :rotation [1 0 0 0]
     :part :ground-part}))

(defn get-anchor-point [world x y]
  (if-let [collision (get-part-collision world x y)]
    (let [part (get-in world [:parts (:part-name collision)])
          type (:type part)]
      (cond
        (in? type [:wagon :block])
        (get-block-anchor-point world collision)
        
        (= type :track)
        (get-track-anchor-point world collision)))
    (get-ground-anchor-point world x y)))
