
(ns temp.core (:gen-class))

(defn tracks-connected? [world t0-name t1-name]
  (let [parts (:parts world)
        t0 (get-in parts [t0-name])
        t1 (get-in parts [t1-name])]
    (or
     (in? t1-name (keys (:children t0)))
     (in? t0-name (keys (:children t1))))))

(defn get-track-neighbours [world part-name]
  (filter (fn [other-part-name]
            (and
             (not (= other-part-name part-name))
             (tracks-connected? world other-part-name part-name)))
          (get-parts-with-type (:parts world) :track)))

(defn grow-loop [world loop color]
  (let [start (first loop)
        end (last loop)
        get-next (fn [tip]
                   (first
                    (filter (fn [part-name]
                              (let [part (get-in world [:parts part-name])]
                                (and
                                 (= (:type part) :track)
                                 (= (:color part) color)
                                 (not (in? part-name loop)))))
                            (get-track-neighbours world tip))))
        before (get-next start)
        after (get-next end)]
    (if (and (nil? before)
             (nil? after))
      loop
      (let [new-loop (cond
                       (or (nil? before)
                           (= before after))
                       (conj loop after)

                       (nil? after)
                       (vec (concat [before] loop))

                       :else
                       (vec (concat [before] (conj loop after))))]
        (recur world new-loop color)))))

(defn get-tail-point [world track-name]
  (let [track (get-in world [:parts track-name])
        [_ sy _] (:scale track)]
    (apply-transform (:transform track) [0 (- sy) 0])))

(defn get-loop-names [world t0-name]
  (let [t0 (get-in world [:parts t0-name])
        loop-color (:color t0)
        loop-names (grow-loop world [t0-name] loop-color)]
    (if (in? (get-parent-part world (first loop-names))
             loop-names)
      (vec (reverse loop-names))
      loop-names)))

(defn get-loop-value [world track-name spec]
  (let [track (get-in world [:parts track-name])
        track-plane (get-track-plane track)
        collision-point (:point (get-part-collision world spec))
        d (point-plane-distance collision-point track-plane)
        all-names (get-loop-names world track-name)
        before-names (conj (vec (take-while #(not= % track-name) all-names)) track-name)
        total-length (reduce + (map (fn [track-name]
                                      (get-in world [:parts track-name :scale 1]))
                                    all-names))
        partial-length (reduce + (map (fn [track-name]
                                        (get-in world [:parts track-name :scale 1]))
                                      before-names))]
    (/ (+ partial-length d) total-length)))

(defn get-track-loop [world t0-name]
  (let [loop-names (get-loop-names world t0-name)
        points (map (fn [name]
                      (get-part-position world name))
                    loop-names)
        tail-point (get-tail-point world (first loop-names))
        points (cons tail-point points)
        t0 (get-in world [:parts t0-name])
        inverse-transform (get-inverse-transform (:transform t0))]
    (vec (map #(apply-transform inverse-transform %) points))))

(defn is-extra-point? [a b c]
  (float=
   (vector-dot-product
    (vector-normalize (vector-subtract b a))
    (vector-normalize (vector-subtract c a)))
   1.0))

(defn remove-extra-points [points]
  (concat [(first points)]
          (filter not-nil?
                  (map (fn [n]
                         (let [a (nth points (dec n))
                               b (nth points n)
                               c (nth points (inc n))]
                           (if (is-extra-point? a b c)
                             nil
                             b)))
                       (range 1 (dec (count points)))))
          [(last points)]))

(defn set-wagon-loop [world wagon-name track-name]
  (let [loop (remove-extra-points (get-track-loop world track-name))
        lengths (map (fn [a b]
                       (vector-length (vector-subtract a b)))
                     loop (rest loop))
        total-length (reduce + lengths)
        times (map #(/ % total-length) (accumulate lengths))
        loop-fn (map vector times loop)
        wagon (get-in world [:parts wagon-name])
        old-length (reduce + (:track-lengths wagon))
        new-length (reduce + lengths)
        new-value (within (* (:value wagon) (/ old-length new-length)) 0 1)]
    (update-in world [:parts wagon-name]
               (fn [wagon]
                 (-> wagon
                     (assoc-in [:value] new-value)
                     (assoc-in [:loop-fn] loop-fn)
                     (assoc-in [:track-lengths] lengths))))))

(defn reset-wagons [world]
  (let [wagon-names (get-parts-with-type (:parts world) :wagon)]
    (reduce (fn [w wagon-name]
              (let [track-name (get-parent-part w wagon-name)]
                (set-wagon-loop w wagon-name track-name)))
            world wagon-names)))
