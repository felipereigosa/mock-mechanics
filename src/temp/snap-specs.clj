
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

(defn get-block-snap-spec [world collision]
  (let [{:keys [part-name point index]} collision
        centered false ;;#################################
        part (get-in world [:parts part-name])
        part-transform (:transform part)
        normal (get-collision-normal world collision)
        normal-transform (make-transform [0 0 0] (quaternion-from-normal normal))
        transform (combine-transforms normal-transform part-transform)]
    {:position point
     :rotation (get-transform-rotation transform)
     :part part-name}))

(defn get-closest-snap-point [world x y & rest]
  (let [line (unproject-point world [x y])
        close-snap-specs (filter (fn [spec]
                                   (< (point-line-distance (:position spec) line) 0.2))
                                 (:snap-specs world))
        plane [[0 0 0] [1 0 0] [0 0 1]]
        ground-spec {:position (line-plane-intersection line plane)
                     :rotation [1 0 0 0]
                     :part :ground-part}
        block-spec (if-let [collision (get-part-collision world x y)]
                     (let [part (get-in world [:parts (:part-name collision)])]
                       (if (in? (:type part) [:block :wagon])
                         (get-block-snap-spec world collision)
                         nil))
                     nil)
        specs (filter not-nil? (conj (vec close-snap-specs) ground-spec block-spec))
        excluded-parts (first rest)
        specs (filter (fn [spec]
                        (not (in? (:part spec) excluded-parts)))
                      specs)
        eye (get-in world [:camera :eye])
        ]
    (first (sort-by (fn [spec]
                      (distance eye (:position spec)))
                    specs))))
