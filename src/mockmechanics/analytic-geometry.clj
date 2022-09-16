(ns mockmechanics.core
  (:require [mockmechanics.library.util :as util]
            [mockmechanics.library.vector :as vector]))

(defn line-get-point [[pl vl] k]
  (vector/add pl (vector/multiply vl k)))

(defn line-plane-intersection [line plane]
  (let [[pl vl] line
        [p0 p1 p2] plane
        v1 (vector/subtract p1 p0)
        v2 (vector/subtract p2 p0)
        v (vector/subtract p0 pl)
        n (vector/cross-product v1 v2)
        dp (vector/dot-product vl n)]
    (if (= dp 0.0)
      nil
      (let [d (/ (vector/dot-product v n) dp)]
        (line-get-point line d)))))

(defn cylinder-plane-intersection [axis radius plane]
  (let [[axis-point axis-dir] axis
        [a b c] plane
        v1 (vector/subtract b a)
        v2 (vector/subtract c a)
        vn (vector/normalize (vector/cross-product v1 v2))
        center-point (line-plane-intersection axis plane)
        minor-dir (vector/normalize (vector/cross-product vn axis-dir))
        minor-point (line-get-point [center-point minor-dir] radius)
        radial-dir (vector/normalize (vector/cross-product minor-dir axis-dir))
        major-point-above (line-get-point [axis-point radial-dir] radius)
        major-point (line-plane-intersection [major-point-above axis-dir] plane)]
    [center-point major-point minor-point]))

(defn sphere-plane-intersection [ps rs [a b c]]
  (let [v1 (vector/subtract b a)
        v2 (vector/subtract c a)
        vn (vector/normalize (vector/cross-product v2 v1))
        k (/ (vector/dot-product (vector/subtract a ps) vn)
             (util/pow (vector/length vn) 2))]
    (if (> (util/abs k) rs)
      nil
      (let [pc (vector/add ps (vector/multiply vn k))
            rc (util/sqrt (- (* rs rs) (* k k)))]
        [pc rc]))))

(defn get-affine-coordinates [v1 v2 vp]
  (let [a (vector/dot-product vp v1)
        b (vector/dot-product v1 v1)
        c (vector/dot-product v1 v2)
        d (vector/dot-product vp v2)
        e (vector/dot-product v2 v2)
        cb (/ c b)
        t (/ (- d  (* a cb)) (- e (* c cb)))
        s (/ (- a (* t c))  b)]
    [s t]))

(defn point-plane-distance [point plane]
  (let [[a b c] plane
        v1 (vector/subtract c a)
        v2 (vector/subtract b a)
        normal (vector/cross-product v1 v2)
        line [point normal]
        projected-point (line-plane-intersection line plane)
        to-point (vector/subtract point projected-point)
        sign (if (pos? (vector/dot-product normal to-point))
               1
               -1)]
    (* sign (vector/length to-point))))

(defn point-above-plane? [point plane]
  (let [[a b c] plane
        v1 (vector/subtract b a)
        v2 (vector/subtract c a)
        normal (vector/cross-product v1 v2)
        line [point normal]
        projected-point (line-plane-intersection line plane)
        to-point (vector/subtract point projected-point)]
    (> (vector/dot-product normal to-point) 0.0)))

(defn line-plane-distance [line plane]
  (let [[pl vl] line
        [p0 p1 p2] plane
        v1 (vector/subtract p1 p0)
        v2 (vector/subtract p2 p0)
        v (vector/subtract p0 pl)
        n (vector/cross-product v1 v2)
        dp (vector/dot-product vl n)]
    (if (= dp 0.0)
      nil
      (/ (vector/dot-product v n) dp))))

(defn line-triangle-distance [[pl vl] [p0 p1 p2]]
  (let [v1 (vector/subtract p1 p0)
        v2 (vector/subtract p2 p0)
        v (vector/subtract p0 pl)
        n (vector/cross-product v1 v2)
        den (vector/dot-product vl n)]
    (if (or (util/float= den 0.0)
            (pos? (vector/dot-product n vl)))
      nil
      (let [dist (/ (vector/dot-product v n) den)
            point (line-get-point [pl vl] dist)
            vp (vector/subtract point p0)
            [s t] (get-affine-coordinates v1 v2 vp)]
        (if (and (>= dist 0)
                 (>= s 0.0)
                 (<= s 1.0)
                 (>= t 0.0)
                 (<= t 1.0)
                 (<= (+ s t) 1.0))
          dist
          nil)))))

(defn point-line-coordinate [point [pl vl]]
  (let [vl (vector/normalize vl)
        point-n (vector/subtract point pl)]
    (vector/scalar-projection point-n vl)))

(defn point-line-projection [point line]
  (line-get-point line (point-line-coordinate point line)))

(defn point-line-distance [point line]
  (let [pl (point-line-projection point line)
        v (vector/subtract point pl)]
    (vector/length v)))

(defn sphere-line-intersection [[sp r] [lp v]]
  (let [w (vector/subtract lp sp)
        a (vector/dot-product v v)
        b (* 2 (vector/dot-product v w))
        c (- (vector/dot-product w w) (* r r))
        delta (- (* b b) (* 4 a c))
        t-fn (fn [d]
               (/ (- d b) (* 2 a)))]
    (cond
      (< delta 0.0) []
      (= delta 0.0) [(t-fn 0)]
      :else (let [sd (util/sqrt delta)]
              [(t-fn (- sd)) (t-fn sd)]))))

(defn line-line-closest-point [l1 l2]
  (let [[p1 v1] l1
        [p2 v2] l2
        p1v1 (vector/dot-product p1 v1)
        p1v2 (vector/dot-product p1 v2)
        p2v1 (vector/dot-product p2 v1)
        p2v2 (vector/dot-product p2 v2)
        v1v1 (vector/dot-product v1 v1)
        v1v2 (vector/dot-product v1 v2)
        v2v2 (vector/dot-product v2 v2)]
    (/ (+ p1v2 (- p2v2) (* (- p2v1 p1v1) (/ v2v2 v1v2)))
       (/ (- (* v1v1 v2v2) (* v1v2 v1v2)) v1v2))))

(defn point-inside-triangle [point triangle]
  (if (util/float= (point-plane-distance point triangle) 0.0)
    (let [[p0 p1 p2] triangle
          v1 (vector/subtract p1 p0)
          v2 (vector/subtract p2 p0)
          vp (vector/subtract point p0)
          [s t] (get-affine-coordinates v1 v2 vp)]
      (and (>= s 0.0)
           (<= s 1.0)
           (>= t 0.0)
           (<= t 1.0)
           (<= (+ s t) 1.0)))
    false))

(defn get-normalized-plane-point [plane point grain]
  (let [[a b c] plane
        v1 (vector/subtract b a)
        v2 (vector/subtract c a)
        s (point-line-coordinate point [a v1])
        t (point-line-coordinate point [a v2])
        s (* grain (util/round (/ s grain)))
        t (* grain (util/round (/ t grain)))]
    (reduce vector/add [a
                        (vector/multiply v1 s)
                        (vector/multiply v2 t)])))

(defn line->plane [[point direction]]
  (let [direction (vector/normalize direction)
        v (if (util/float= (util/abs (vector/dot-product direction [1 0 0])) 1.0)
            [0 1 0]
            [1 0 0])
        v1 (vector/cross-product direction v)
        v2 (vector/cross-product v1 direction)
        p1 (vector/add point v1)
        p2 (vector/add point v2)]
    [point p1 p2]))

(defn point-triangle-projection [point triangle]
  (let [[a b c] triangle
        v1 (vector/subtract c a)
        v2 (vector/subtract b a)
        normal (vector/cross-product v1 v2)
        line [point normal]
        projected-point (line-plane-intersection line triangle)
        to-point (vector/subtract point projected-point)]
    (if (neg? (vector/dot-product normal to-point))
      nil
      (let [d (vector/add (vector/add a v1) v2)]
        (if (or (point-inside-triangle projected-point [a b c])
                (point-inside-triangle projected-point [b c d]))
          projected-point
          nil)))))

(defn point-plane-projection [point plane]
  (let [[a b c] plane
        v1 (vector/subtract c a)
        v2 (vector/subtract b a)
        normal (vector/cross-product v1 v2)
        line [point normal]
        projected-point (line-plane-intersection line plane)
        to-point (vector/subtract point projected-point)]
    (if (neg? (vector/dot-product normal to-point))
      nil
      projected-point)))
