
(ns mockmechanics.cables)

(defn draw-cable! [world cable-name]
  (let [cable (get-in world [:cables cable-name])
        positions (map (fn [probe-name]
                         (let [transform (use-root-relative-transform world probe-name)]
                           (get-transform-position transform)))
                       (:parts cable))]

    (dotimes [i (dec (count positions))]
      (draw-segment! world
                     (nth positions i)
                     (nth positions (inc i))))))

(defn draw-cables! [world]
  (doseq [cable (keys (:cables world))]
    (draw-cable! world cable)))

(defn get-cable-forces [world cable-name]
  (let [cable (get-in world [:cables cable-name])
        positions (map (fn [probe-name]
                         (let [transform (use-root-relative-transform world probe-name)]
                           (get-transform-position transform)))
                       (:parts cable))

        vectors (map #(vector/subtract %1 %2) positions (rest positions))
        length (reduce + (map vector/length vectors))
        displacement (- (:length cable) length)
        k 3
        get-vector (fn [pos m]
                     (-> (pos vectors)
                         vector/normalize
                         (vector/multiply (* displacement k m))))]
    (if (neg? displacement)
      [{:part-name (get-first-dof world (first (:parts cable)))
        :local-point [0 0 0]
        :vector (get-vector first 1)}
       {:part-name (get-first-dof world (last (:parts cable)))
        :local-point [0 0 0]
        :vector (get-vector last -1)}]
      [])))

(defn get-cables-forces [world]
  (->> (:cables world)
       keys
       (map #(get-cable-forces world %))
       flatten))
