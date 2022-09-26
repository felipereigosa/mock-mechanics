
(ns mockmechanics.core)

(declare get-first-dof)

(defn get-segment [world cable start-point end-point]
  (let [v (vector/subtract end-point start-point)
        thickness (:thickness cable)
        scale [thickness (vector/length v) thickness]
        middle (vector/add (vector/multiply v 0.5) start-point)
        rotation (quaternion-from-normal v)
        transform (make-transform middle rotation)]
    (-> (get-in world [:info :cable :model])
        (set-mesh-color (:color cable))
        (assoc-in [:scale] scale)
        (assoc-in [:transform] transform))))

(defn get-cable-positions [world cable]
  (map (fn [probe-name]
         (let [transform (use-root-relative-transform world probe-name)]
           (get-transform-position transform)))
       (:parts cable)))

(defn draw-cables! [world]
  (let [cable-names (get-parts-with-type (:parts world) :cable)]
    (doseq [cable-name cable-names]
      (let [cable (get-in world [:parts cable-name])
            positions (get-cable-positions world cable)]
        (when (in? (:layer cable) (:visible-layers world))
          (dotimes [i (dec (count positions))]
            (draw-mesh! world (get-segment
                                world
                                cable
                                (nth positions i)
                                (nth positions (inc i))))))))))

(defn get-cable-forces [world cable-name]
  (let [cable (get-in world [:parts cable-name])
        positions (get-cable-positions world cable)
        vectors (map #(vector/subtract %1 %2) positions (rest positions))
        length (reduce + (map vector/length vectors))
        displacement (- (:value cable) length)
        k 3
        get-vector (fn [pos m]
                     (-> (pos vectors)
                         vector/normalize
                         (vector/multiply (* displacement k m))))

        get-local-point (fn [dof position]
                          (let [transform (get-in world [:weld-groups dof :transform])
                                inverse-transform (get-inverse-transform transform)]
                            (apply-transform inverse-transform position)))

        start (get-first-dof world (first (:parts cable)))
        start-position (first positions)
        end (get-first-dof world (last (:parts cable)))
        end-position (last positions)]
    (cond-> []
      start (conj {:part-name start
                   :local-point (get-local-point start start-position)
                   :vector (get-vector first 1)})
      end (conj {:part-name end
                 :local-point (get-local-point end end-position)
                 :vector (get-vector last -1)}))))

(defn get-cables-forces [world]
  (->> (get-parts-with-type (:parts world) :cable)
       (map #(get-cable-forces world %))
       flatten))

(defn make-cable-parts [world cable-name]
  (let [cable (get-in world [:parts cable-name])
        positions (get-cable-positions world cable)]
    (map (fn [start end i]
           (let [segment (-> (get-segment world cable start end)
                             (assoc-in [:layer] (:layer cable))
                             (assoc-in [:type] :cable)
                             (assoc-in [:segment-index] i))]
             [cable-name segment]))
         positions (rest positions) (range))))

(defn get-replaced-cable-parts [world]
  (let [cable-names (get-parts-with-type (:parts world) :cable)
        cable-parts (map #(make-cable-parts world %) cable-names)
        other-parts (filter #(not= (:type (second %)) :cable) (:parts world))]
    (concat other-parts (apply concat cable-parts))))

(defn recompute-cable-length [world cable-name]
  (let [cable (get-in world [:parts cable-name])
        positions (get-cable-positions world cable)
        vectors (map #(vector/subtract %1 %2) positions (rest positions))
        length (reduce + (map vector/length vectors))]
    (assoc-in world [:parts cable-name :value] length)))
