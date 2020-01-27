
(ns temp.core)

(defn make-spec [position rotation point]
  (let [rotation-transform (make-transform [0 0 0] rotation)
        transform (make-transform position rotation)
        p (apply-transform transform point)
        normal-table {[ 1  0  0] [0 0 1 -90]
                      [-1  0  0] [0 0 1 90]
                      [ 0  1  0] [1 0 0 0]
                      [ 0 -1  0] [1 0 0 180]
                      [ 0  0  1] [1 0 0 90]
                      [ 0  0 -1] [1 0 0 -90]}
        extra-rotation (get normal-table (map round (vector-normalize point)))
        final-rotation (get-transform-rotation
                        (combine-transforms
                         (make-transform [0 0 0] extra-rotation)
                         (make-transform [0 0 0] rotation)))]
    [p final-rotation]))

(defn get-snap-specs [world]
  (vec
   (remove-nil
    (mapcat (fn [[name part]]
              (let [part (get-in world [:parts name])]
                (if (= (:type part) :track)
                  (let [transform (:transform part)
                        position (get-transform-position transform)
                        rotation (get-transform-rotation transform)
                        points (get-in world [:info (:type part) :points])
                        [sa sb sc] (if (= (:type part) :track)
                                     [1 1 1]
                                     (:scale part))
                        points (map (fn [[a b c]]
                                      [(* sa a) (* sb b) (* sc c)])
                                    points)]
                    (map (fn [p]
                           (let [[pos rot] (make-spec position rotation p)]
                             {:position pos
                              :rotation rot
                              :part name}))
                         points))
                  nil)))
            (:parts world)))))

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

(do
1

(defn get-collision-with-part [world px py part-name]
  (let [line (unproject-point world [px py])
        part (get-in world [:parts part-name])
        type (:type part)
        info (get-in world [:info type])
        mesh (:model info)
        transform (:transform part)
        scale (:scale part)
        [i d p] (get-mesh-collision mesh transform scale line)]
    {:part-name part-name
     :distance d
     :point p
     :index i}))

(defn draw-blocks-to-bitmap [world]
  ;;############################################################
  (let [block-names (filter not-nil?
                            (map (fn [[name part]]
                                   (if (in? (:type part) [:block :wagon])
                                     name
                                     nil))
                                 (:parts world)))
        ]
    (println! block-names)

    ;; save list of blocks/wagons
    ;; update bitmap
    ))

(clear-output!)
(draw-blocks-to-bitmap @world)

(defn fast-get-block-at [world px py & rest]
  ;;############################################################
  ;; only blocks and wagons
  ;; no hidden parts
  ;; no excluded parts

  ;; get value of bitmap pixel at px,py
  ;; use value to get nth element from the list

  (cond
    (< (distance [px py] [205.0 318.0]) 40) :block11686-copy11891-copy11901    
    (< (distance [px py] [301.0 292.0]) 40) :block11686-copy11901              
    (< (distance [px py] [446.0 341.0]) 40) :block11686                        
    ))

(defn get-block-spec [world x y excluded-parts]
  (if-let [block-name (fast-get-block-at world x y)]
    (let [collision (get-collision-with-part world x y block-name)
          part (get-in world [:parts (:part-name collision)])
          {:keys [part-name point index]} collision
          part-transform (:transform part)
          normal (get-collision-normal world collision)
          normal-transform (make-transform [0 0 0] (quaternion-from-normal normal))
          transform (combine-transforms normal-transform part-transform)
          face-plane (get-block-plane part normal)
          grain (if (:shift-pressed world)
                  0.05
                  0.2)
          point (get-normalized-plane-point face-plane point grain)]
      {:position point
       :rotation (get-transform-rotation transform)
       :part part-name})
    nil))      
)

(defn get-ground-spec [world line]
  (let [plane [[0.25 0 0.25] [1.25 0 0.25] [0.25 0 1.25]]
        point (line-plane-intersection line plane)
        grain (if (:shift-pressed world)
                0.05
                0.25)
        point (get-normalized-plane-point plane point grain)]
    {:position point
     :rotation [1 0 0 0]
     :part :ground-part}))

(defn get-closest-spec [world x y & rest]
  (let [line (unproject-point world [x y])
        close-snap-specs (filter (fn [spec]
                                   (< (point-line-distance
                                       (:position spec) line) 0.2))
                                 (:snap-specs world))

        ground-spec (if (> (get-in world [:camera :x-angle]) 0)
                      (get-ground-spec world line)
                      nil)
        excluded-parts (first rest)
        block-spec (get-block-spec world x y excluded-parts)        
        specs (filter not-nil? (conj (vec close-snap-specs)
                                     ground-spec block-spec))
        specs (filter (fn [spec]
                        (not (in? (:part spec) excluded-parts)))
                      specs)
        eye (get-in world [:camera :eye])]
    (first (sort-by (fn [spec]
                      (distance eye (:position spec)))
                    specs))))
