
(in-ns 'temp.core)

(defn vector-length [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn vector-add [[a0 a1 a2] [b0 b1 b2]]
  [(+ a0 b0)
   (+ a1 b1)
   (+ a2 b2)])

(defn vector-subtract [[a0 a1 a2] [b0 b1 b2]]
  [(- a0 b0)
   (- a1 b1)
   (- a2 b2)])

(defn vector-multiply [[x y z] v]
  [(* x v) (* y v) (* z v)])

(defn vector-dot-product [[a0 a1 a2] [b0 b1 b2]]
  (+ (* a0 b0) (* a1 b1) (* a2 b2)))

(defn vector-cross-product [[a0 a1 a2] [b0 b1 b2]]
  [ (- (* a1 b2) (* a2 b1))
    (- (* a2 b0) (* a0 b2))
    (- (* a0 b1) (* a1 b0))])

(defn vector-normalize [[x y z]]
  (let [l (vector-length [x y z])]
    [(float (/ x l)) (float (/ y l)) (float (/ z l))]))

(declare get-rotation-matrix)
(declare multiply-matrix-vector)

(defn vector-rotate
  ([[x y z] [ax ay az] angle]
   (let [vector (into-array Float/TYPE [x y z 1.0])
         matrix (get-rotation-matrix angle ax ay az)
         [x y z _] (into [] (multiply-matrix-vector matrix vector))]
     [x y z]))
  ([v pivot axis angle]
   (let [v (vector-subtract v pivot)
         v (vector-rotate v axis angle)]
     (vector-add v pivot))))

(defn vector-dot-product [[x1 y1 z1] [x2 y2 z2]]
  (+ (* x1 x2) (* y1 y2) (* z1 z2)))

(defn vector-angle
  ([a b]
   (let [v (/ (vector-dot-product a b) (* (vector-length a) (vector-length b)))]
     (cond
       (>= v 1) 0
       (<= v -1) 180
       :else (acos v))))
  ([a b axis]
   (let [value (vector-angle a b)
         cross-axis (vector-cross-product a b)]
     (if (= (vector-length cross-axis) 0.0)
       value
       (let [cross-axis (vector-normalize cross-axis)
             axis (vector-normalize axis)
             sign (vector-dot-product cross-axis axis)]
         (* sign value))))))  

(defn vector-scalar-projection [a b]
  (vector-dot-product a (vector-normalize b)))

(defn vector-orthogonal [vector]
  (let [temp (if (< (vector-angle vector [1 0 0]) 20)
               [0.0 0.0 1.0]
               [1.0 0.0 0.0])]
    (vector-cross-product vector temp)))
