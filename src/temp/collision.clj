
(defn project-point [world point]
  (let [p-matrix (:projection-matrix world)
        v-matrix (:view-matrix world)
        matrix (multiply-matrices v-matrix p-matrix)
        point (into-array Float/TYPE (conj point 1.0))
        point-2d (vec (multiply-matrix-vector matrix point))
        [x y _ _] (map #(/ % (nth point-2d 3)) point-2d)]
    [(int (* (/ (inc x) 2) (:window-width world)))
     (int (* (/ (inc (- y)) 2) (:window-height world)))]))

(defn unproject-point [world [x y]]
  (let [dx (dec (/ x (/ (:window-width world) 2)))
        dy (- (dec (/ y (/ (:window-height world) 2))))
        p-matrix (:projection-matrix world)
        v-matrix (:view-matrix world)
        matrix (multiply-matrices v-matrix p-matrix)
        inverse-matrix (get-inverse-matrix matrix)
        p-2d-a (into-array Float/TYPE [dx dy -1.0 1.0])
        p-3d-a (into [] (multiply-matrix-vector inverse-matrix p-2d-a))
        p-3d-a (map (slots / _ (nth p-3d-a 3)) p-3d-a)
        p-3d-a (into [] (butlast p-3d-a))

        p-2d-b (into-array Float/TYPE [dx dy 0.0 1.0])
        p-3d-b (into [] (multiply-matrix-vector inverse-matrix p-2d-b))
        p-3d-b (map (slots / _ (nth p-3d-b 3)) p-3d-b)
        p-3d-b (into [] (butlast p-3d-b))]
    [p-3d-a (vector-normalize (vector-subtract p-3d-b p-3d-a))]))

(defn distance-comparator [a b]
  (cond
    (nil? a) false
    (nil? b) true
    (and (nil? a) (nil? b)) a
    :else (< a b)))

(defn get-mesh-triangles [mesh transform scale]
  (let [vertices (partition 3 (into [] (:vertices mesh)))
        matrix (multiply-matrices
                (apply get-scale-matrix scale)
                (get-transform-matrix transform))
        vertices (map (fn [[x y z]]
                        (let [vertex (into-array Float/TYPE [x y z 1])]
                          (butlast (into [] (multiply-matrix-vector
                                             matrix vertex)))))
                      vertices)]
    (partition 3 vertices)))

(defn get-mesh-collision [mesh transform scale line]
  (let [triangles (get-mesh-triangles mesh transform scale)
        measured-triangles (map (fn [i]
                                  {:d (line-triangle-distance
                                       line (nth triangles i))
                                   :i i})
                                (range (count triangles)))
        collision (first (sort-by :d distance-comparator measured-triangles))]
    (if (nil? (:d collision))
      nil
      [(:i collision) (:d collision) (line-get-point line (:d collision))])))

(defn get-collision-model [world type]
  (or (get-in world [:info type :collision-model])
      (get-in world [:info :block :model])))

(defn get-collision-normal [world collision]
  (let [{:keys [part-name point index]} collision
        type (get-in world [:parts part-name :type])
        mesh (get-collision-model world type)
        triangles (partition 3 (partition 3 (:vertices mesh)))
        [a b c] (nth triangles index)
        v1 (vector-subtract b a)
        v2 (vector-subtract c a)]
    (vector-cross-product v1 v2)))

(defn get-spec-line [world spec]
  (or (:line spec)
      (unproject-point world [(:x spec) (:y spec)])))

(defn get-part-collision [world spec]
  (let [line (get-spec-line world spec)
        distances (map (fn [[part-name part]]
                         (if (not (in? (:layer part)
                                       (:visible-layers world)))
                           nil
                           (let [type (:type part)
                                 info (get-in world [:info type])
                                 mesh (get-collision-model world type)
                                 transform (if (= (:type part) :track)
                                             (get-tail-transform part)
                                             (:transform part))
                                 scale (:scale part)
                                 [i d p] (get-mesh-collision mesh transform scale line)]
                             (if (nil? d)
                               nil
                               {:part-name part-name
                                :distance d
                                :point p
                                :index i}))))
                       (:parts world))
        distances (filter (fn [distance]
                            (not (or (nil? distance)
                                     (= (:part-name distance) :ground-part))))
                          distances)]
    (first (sort-by :distance distances))))

(defn get-part-at [world spec]
  (:part-name (get-part-collision world spec)))

(defn get-track-head-collision [world spec]
  (if-let [track-head-name (:track-head world)]
    (let [transform (get-in world [:parts track-head-name :transform])
          mesh (:track-head-model world)
          scale (:scale mesh)
          line (get-spec-line world spec)
          collision (get-mesh-collision mesh transform scale line)]
      {:part-name track-head-name
       :track-head true
       :collision collision
       :distance (second collision)})
    nil))

(defn get-ground-collision [world spec]
  (let [plane [[0 0 0] [1 0 0] [0 0 1]]
        line (get-spec-line world spec)]
    {:part-name :ground-part
     :point (line-plane-intersection line plane)}))

(defn get-collision [world spec]
  (let [c-track-head (get-track-head-collision world spec)
        c-part (get-part-collision world spec)
        c-ground (get-ground-collision world spec)]
    (cond
      (and (nil? c-track-head)
           (nil? c-part))
      c-ground

      (distance-comparator (:distance c-track-head)
                           (:distance c-part)) c-track-head
      
      :else c-part)))
