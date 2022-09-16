(ns mockmechanics.library.vector
  (:require [mockmechanics.library.util :as util]
            [mockmechanics.library.matrix :as matrix]))

(defn dot-product [v1 v2]
  (reduce + (map * v1 v2)))

(defn length [v]
  (Math/sqrt (dot-product v v)))

(defn add [v1 v2]
  (vec (map + v1 v2)))

(defn subtract [v1 v2]
  (vec (map - v1 v2)))

(defn multiply [v amount]
  (vec (map (partial * amount) v)))

(defn cross-product [a b]
  (if (= (count a) (count b) 3)
    (let [[a0 a1 a2] a
          [b0 b1 b2] b]
      [(- (* a1 b2) (* a2 b1))
       (- (* a2 b0) (* a0 b2))
       (- (* a0 b1) (* a1 b0))])
    (throw Exception)))

(defn normalize [v]
  (vec (map #(/ % (length v)) v)))

(defn rotate
  ([[x y z] [ax ay az] angle]
   (let [vector (float-array [x y z 1.0])
         matrix (matrix/get-rotation angle ax ay az)
         [x y z _] (vec (matrix/multiply-vector matrix vector))]
     [x y z]))
  ([v pivot axis angle]
   (let [v (subtract v pivot)
         v (rotate v axis angle)]
     (add v pivot))))

(defn angle-2 [[x1 y1] [x2 y2]]
  (let [sa (- (Math/atan2 y2 x2) (Math/atan2 y1 x1))
        sb (+ sa (* 2 util/pi))
        sc (- sa (* 2 util/pi))
        a (util/abs sa)
        b (util/abs sb)
        c (util/abs sc)
        m (min a b c)]
    (cond
      (= m a) (util/to-degrees sa)
      (= m b) (util/to-degrees sb)
      :else (util/to-degrees sc))))

(defn angle-3 [a b]
  (let [v (/ (dot-product a b)
             (* (length a) (length b)))]
    (cond
      (>= v 1) 0
      (<= v -1) 180
      :else (util/acos v))))

(defn angle
  ([a b]
   (case (count a)
     2 (angle-2 a b)
     3 (angle-3 a b)
     (throw Exception)))
  ([a b axis]
   (let [value (angle a b)
         cross-axis (cross-product a b)]
     (if (util/float= (length cross-axis) 0.0)
       value
       (let [cross-axis (normalize cross-axis)
             axis (normalize axis)
             sign (dot-product cross-axis axis)]
         (* sign value))))))

(defn distance [a b]
  (length (subtract a b)))

(defn scalar-projection [a b]
  (dot-product a (normalize b)))

(defn orthogonal [vector]
  (let [temp (if (< (angle vector [1 0 0]) 20)
               [0.0 0.0 1.0]
               [1.0 0.0 0.0])]
    (cross-product vector temp)))

(defn equal? [a b]
  (every? #(util/float= % 0.0) (subtract a b)))

(defn interpolate [p1 p2 t]
  (vec (map (fn [a b]
              (+ (* a (- 1.0 t)) (* b t)))
            p1 p2)))

(defn project [v other]
  (let [other (normalize other)
        length (dot-product v other)]
    (multiply other length)))
