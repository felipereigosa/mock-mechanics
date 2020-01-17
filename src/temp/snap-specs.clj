
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

;;---

(defn get-snap-specs [world]
  (let [;; grid-specs (if (> (get-in world [:camera :x-angle]) 0)
        ;;              (vec (map (fn [[a b]]
        ;;                          (let [x (- (* a 0.5) 5.75)
        ;;                                y (- (* b 0.5) 5.75)]
        ;;                            {:position [x 0 y]
        ;;                             :rotation [1 0 0 0]
        ;;                             :part :ground-part}))
        ;;                        (create-combinations (range 24) (range 24))))
        ;;              [])

        grid-specs [{:position [0.25 0 0.25]
                     :rotation [1 0 0 0]
                     :part :ground-part}

                    {:position [0.75 0 0.25]
                     :rotation [1 0 0 0]
                     :part :ground-part}
                    ]
        
        face-specs (vec
                    (remove-nil
                     (mapcat (fn [[name part]]
                               (let [part (get-in world [:parts name])
                                     transform (:transform part)
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
                                      points)))
                             (:parts world))))]
    (vec (concat grid-specs face-specs))))

(defn get-closest-snap-point [world x y snap-specs]
  (let [line (unproject-point world [x y])
        close-points (filter (fn [spec]
                               (< (point-line-distance (:position spec) line) 0.2))
                             snap-specs)
        eye (get-in world [:camera :eye])]
    (first (sort-by (fn [spec]
                      (distance (:position spec) eye)) close-points))))
