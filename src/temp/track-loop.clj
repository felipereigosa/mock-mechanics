
(ns temp.core)

(defn tracks-connected? [world p0-name p1-name]
  (let [parts (:parts world)
        p0 (get-in parts [p0-name])
        p1 (get-in parts [p1-name])]
    (or
     (and (in? p1-name (keys (:children p0)))
          (nil? (:loop-fn p1)))

     (and (in? p0-name (keys (:children p1)))
          (nil? (:loop-fn p0))))))

(defn get-track-neighbours [world part-name]
  (filter (fn [other-part-name]
            (and
             (not (= other-part-name part-name))
             (tracks-connected? world other-part-name part-name)))
          (keys (:parts world))))

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

(defn get-track-loop [world t0-name]
  (let [t0 (get-in world [:parts t0-name])
        loop-color (:color t0)
        loop-names (grow-loop world [t0-name] loop-color)
        loop-names (if (in? (get-parent-part world (first loop-names))
                            loop-names)
                     (vec (reverse loop-names))
                     loop-names)
        points (map (fn [name]
                      (get-part-position world name))
                    loop-names)
        tail-point (get-tail-point world (first loop-names))
        points (cons tail-point points)
        inverse-transform (get-inverse-transform (:transform t0))]
    (vec (map #(apply-transform inverse-transform %) points))))

(defn is-extra-point? [a b c]
  (float-equals?
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
        loop-fn (map vector times loop)]
    (update-in world [:parts wagon-name]
               (fn [wagon]
                 (-> wagon
                     (assoc-in [:loop-fn] loop-fn)
                     (assoc-in [:track-lengths] lengths))))))
