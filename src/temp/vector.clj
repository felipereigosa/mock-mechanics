
(in-ns 'temp.core)

(defn vector-dot-product [v1 v2]
  (reduce + (map * v1 v2)))

(defn vector-length [v]
  (Math/sqrt (vector-dot-product v v)))

(defn vector-add [v1 v2]
  (vec (map + v1 v2)))

(defn vector-subtract [v1 v2]
  (vec (map - v1 v2)))

(defn vector-multiply [v amount]
  (vec (map (partial * amount) v)))

(defn vector-cross-product [a b]
  (if (= (count a) (count b) 3)
    (let [[a0 a1 a2] a
          [b0 b1 b2] b]
      [(- (* a1 b2) (* a2 b1))
       (- (* a2 b0) (* a0 b2))
       (- (* a0 b1) (* a1 b0))])
    (throw Exception)))

(defn vector-normalize [v]
  (vec (map #(/ % (vector-length v)) v)))

(declare get-rotation-matrix)
(declare multiply-matrix-vector)

;; (defn vector-rotate
;;   ([v angle]
;;    (let [[x y] v
;;          c (cos angle)
;;          s (sin angle)
;;          x-prime (- (* x c) (* y s))
;;          y-prime (+ (* y c) (* x s))]
;;      [x-prime y-prime]))
  
;;   ([v axis angle]
;;    ;;   ([[x y z] [ax ay az] angle]
;;    ;;    (let [vector (into-array Float/TYPE [x y z 1.0])
;;    ;;          matrix (get-rotation-matrix angle ax ay az)
;;    ;;          [x y z _] (into [] (multiply-matrix-vector matrix vector))]
;;    ;;      [x y z]))
;;    2) ;;###########################
  
;;   ([v pivot axis angle]
;;    (let [v (vector-subtract v pivot)
;;          v (vector-rotate v axis angle)]
;;      (vector-add v pivot))))

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

(defn vector-angle-2 [[x1 y1] [x2 y2]]
  (let [sa (- (Math/atan2 y2 x2) (Math/atan2 y1 x1))
        sb (+ sa (* 2 pi))
        sc (- sa (* 2 pi))
        a (abs sa)
        b (abs sb)
        c (abs sc)
        m (min a b c)]
    (cond
      (= m a) (to-degrees sa)
      (= m b) (to-degrees sb)
      :else (to-degrees sc))))

(defn vector-angle-3 [a b]
  (let [v (/ (vector-dot-product a b)
             (* (vector-length a) (vector-length b)))]
    (cond
      (>= v 1) 0
      (<= v -1) 180
      :else (acos v))))

(defn vector-angle
  ([a b]
   (case (count a)
     2 (vector-angle-2 a b)
     3 (vector-angle-3 a b)
     (throw Exception)))
  ([a b axis]
   (let [value (vector-angle a b)
         cross-axis (vector-cross-product a b)]
     (if (= (vector-length cross-axis) 0.0)
       value
       (let [cross-axis (vector-normalize cross-axis)
             axis (vector-normalize axis)
             sign (vector-dot-product cross-axis axis)]
         (* sign value))))))

;; (defn vector-scalar-projection [a b]
;;   (vector-dot-product a (vector-normalize b)))

;; (defn vector-orthogonal [vector]
;;   (let [temp (if (< (vector-angle vector [1 0 0]) 20)
;;                [0.0 0.0 1.0]
;;                [1.0 0.0 0.0])]
;;     (vector-cross-product vector temp)))

(defn vector= [a b]
  (every? identity (map float-equals? a b)))
  
(defn distance [a b]
  (vector-length (vector-subtract a b)))

(defn vector-scalar-projection [a b]
  (vector-dot-product a (vector-normalize b)))

;;#####--------------------------------------------------------------------------;;

;; (in-ns 'temp.core)

;; (defn vector-length [[x y z]]
;;   (Math/sqrt (+ (* x x) (* y y) (* z z))))

;; (defn vector-add [[a0 a1 a2] [b0 b1 b2]]
;;   [(+ a0 b0)
;;    (+ a1 b1)
;;    (+ a2 b2)])

;; (defn vector-subtract [[a0 a1 a2] [b0 b1 b2]]
;;   [(- a0 b0)
;;    (- a1 b1)
;;    (- a2 b2)])

;; (defn vector-multiply [[x y z] v]
;;   [(* x v) (* y v) (* z v)])

;; (defn vector-dot-product [[a0 a1 a2] [b0 b1 b2]]
;;   (+ (* a0 b0) (* a1 b1) (* a2 b2)))

;; (defn vector-cross-product [[a0 a1 a2] [b0 b1 b2]]
;;   [ (- (* a1 b2) (* a2 b1))
;;     (- (* a2 b0) (* a0 b2))
;;     (- (* a0 b1) (* a1 b0))])

;; (defn vector-normalize [[x y z]]
;;   (let [l (vector-length [x y z])]
;;     [(float (/ x l)) (float (/ y l)) (float (/ z l))]))

;; (declare get-rotation-matrix)
;; (declare multiply-matrix-vector)

;; (defn vector-rotate
;;   ([[x y z] [ax ay az] angle]
;;    (let [vector (into-array Float/TYPE [x y z 1.0])
;;          matrix (get-rotation-matrix angle ax ay az)
;;          [x y z _] (into [] (multiply-matrix-vector matrix vector))]
;;      [x y z]))
;;   ([v pivot axis angle]
;;    (let [v (vector-subtract v pivot)
;;          v (vector-rotate v axis angle)]
;;      (vector-add v pivot))))

;; (defn vector-dot-product [[x1 y1 z1] [x2 y2 z2]]
;;   (+ (* x1 x2) (* y1 y2) (* z1 z2)))

;; (defn vector-angle
;;   ([a b]
;;    (let [v (/ (vector-dot-product a b) (* (vector-length a) (vector-length b)))]
;;      (cond
;;        (>= v 1) 0
;;        (<= v -1) 180
;;        :else (acos v))))
;;   ([a b axis]
;;    (let [value (vector-angle a b)
;;          cross-axis (vector-cross-product a b)]
;;      (if (= (vector-length cross-axis) 0.0)
;;        value
;;        (let [cross-axis (vector-normalize cross-axis)
;;              axis (vector-normalize axis)
;;              sign (vector-dot-product cross-axis axis)]
;;          (* sign value))))))  

;; (defn vector-scalar-projection [a b]
;;   (vector-dot-product a (vector-normalize b)))

;; (defn vector-orthogonal [vector]
;;   (let [temp (if (< (vector-angle vector [1 0 0]) 20)
;;                [0.0 0.0 1.0]
;;                [1.0 0.0 0.0])]
;;     (vector-cross-product vector temp)))

;; (defn distance [x1 y1 x2 y2]
;;   (let [dx (- x1 x2)
;;         dy (- y1 y2)]
;;     (Math/sqrt (+ (* dx dx) (* dy dy)))))
