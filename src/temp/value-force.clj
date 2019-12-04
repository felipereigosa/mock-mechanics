
(ns temp.core)

(defn get-weld-parent-part [world part-name]
  (let [weld-groups (:weld-groups world)]
    (find-if (fn [name]
               (let [children (get-in weld-groups [name :children])]
                 (in? part-name (keys children))))
             (keys weld-groups))))

(defn get-function-segment [function t]
  (cond
    (<= t 0.0) [(nth function 0) (nth function 1)]

    (>= t 1.0)
    (let [n (count function)]
      [(nth function (- n 2)) (nth function (- n 1))])

    :else
    (let [pairs (map vector function (rest function))]
      (find-if (fn [[[t0 & _] [t1 & _]]]
                 (<= t0 t t1))
               pairs))))

(defn get-wagon-direction [world wagon-name]
  (let [wagon (get-in world [:parts wagon-name])
        loop-fn (if (:use-weld-groups world)
                  (compute-translated-loop-fn (:loop-fn wagon))
                  (:loop-fn wagon))
        parent-name (if (:use-weld-groups world)
                      (do
                        (println! "direction error")
                        (get-weld-parent-part world wagon-name))
                      
                      (get-parent-part world wagon-name))

        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        loop-fn (map (fn [[t v]]
                       [t (apply-transform parent-transform v)])
                     loop-fn)
        value (within (:value wagon) 0.0 1.0)
        [[_ p0] [_ p1]] (get-function-segment loop-fn value)]
    (vector-normalize (vector-subtract p1 p0))))

(defn apply-force-to-wagon [world elapsed]
  (if-let [{:keys [part-name velocity line point]} (:force world)]
    (let [wagon (get-in world [:parts part-name])
          transform (:transform wagon)
          p1 (apply-transform transform point)
          p2 (point-line-projection p1 line)
          force-vector (vector-subtract p2 p1)
          track-direction (get-wagon-direction world part-name)
          force-component (/ (vector-dot-product force-vector track-direction)
                             (vector-length track-direction))
          acceleration (* force-component 100)
          value (get-in world [:parts part-name :value])
          dt (* elapsed 0.001)
          dv (* acceleration dt)
          dampening-factor 0.80
          velocity (* (+ velocity dv) dampening-factor)
          dvalue (* velocity dt)
          value (+ value dvalue)]
      (-> world
          (assoc-in [:parts part-name :value] (within value 0 1))
          (assoc-in [:force :velocity] velocity)))
    world))

(defn apply-force-to-track [world elapsed]
  (if-let [{:keys [part-name velocity line point]} (:force world)]
    (let [track (get-in world [:parts part-name])
          transform (:transform track)
          p1 (apply-transform transform point)
          p2 (point-line-projection p1 line)
          force-vector (vector-subtract p2 p1)
          rotation (get-rotation-component transform)
          track-direction (apply-transform rotation [0 1 0])
          track-position (get-transform-position transform)
          track-line [track-position track-direction]
          p3 (point-line-projection p1 track-line)
          arm-vector (vector-subtract p1 p3)
          theta (vector-angle arm-vector force-vector track-direction)
          r (vector-length arm-vector)
          F (vector-length force-vector)
          force-component (* r F (sin theta))
          acceleration (* force-component 100)
          value (get-in world [:parts part-name :value])
          dt (* elapsed 0.001)
          dv (* acceleration dt)
          dampening-factor 0.80
          velocity (* (+ velocity dv) dampening-factor)
          dvalue (* velocity dt)
          value (+ value dvalue)]
      (-> world
          (assoc-in [:parts part-name :value] value)
          (assoc-in [:force :velocity] velocity)))
    world))

(defn apply-force [world elapsed]
  (if-let [part-name (get-in world [:force :part-name])]
    (let [part (get-in world [:parts part-name])]
      (case (:type part)
        :wagon (apply-force-to-wagon world elapsed)
        :track (apply-force-to-track world elapsed)
        world))
    world))

(defn get-first-dof [world part-name]
  (let [part (get-in world [:parts part-name])]
    (if (:free part)
      part-name
      (let [parent-name (get-parent-part world part-name)]
        (if (= parent-name :ground-part)
          nil
          (recur world parent-name))))))
