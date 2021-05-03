
(import android.opengl.Matrix)

(defn get-identity-matrix []
  (let [matrix (into-array Float/TYPE (range 16))]
    (Matrix/setIdentityM matrix 0)
    matrix))

(defn get-rotation-matrix [angle x y z]
  (let [matrix (into-array Float/TYPE (range 16))]
    (Matrix/setRotateM matrix 0 angle x y z)
    matrix))

(defn get-translation-matrix [x y z]
  (let [matrix (get-identity-matrix)]
    (Matrix/translateM matrix 0 x y z)
    matrix))

(defn get-scale-matrix [x y z]
  (let [matrix (get-identity-matrix)]
    (Matrix/scaleM matrix 0 x y z)
    matrix))

(defn get-look-at-matrix [[eye-x eye-y eye-z]
                          [look-x look-y look-z]
                          [up-x up-y up-z]]
  (let [matrix (get-identity-matrix)]
    (Matrix/setLookAtM matrix 0
                       eye-x eye-y eye-z
                       look-x look-y look-z
                       up-x up-y up-z)
    matrix))

(defn multiply-matrices [a b]
  (let [vector-args (vector? a)
        a (if vector-args (into-array Float/TYPE a) a)
        b (if vector-args (into-array Float/TYPE b) b)
        matrix (into-array Float/TYPE (range 16))]
    (Matrix/multiplyMM matrix 0 a 0 b 0)
    (if vector-args
      (into [] matrix)
      matrix)))

(defn multiply-matrix-vector [m v]
  (let [vector (into-array Float/TYPE (range 4))]
    (Matrix/multiplyMV vector 0 m 0 v 0)
    vector))

(defn get-frustum-matrix [left right bottom top near far]
  (let [matrix (into-array Float/TYPE (range 16))]
    (Matrix/frustumM matrix 0 left right bottom top near far)
    matrix))

(defn get-perspective-matrix [fovy aspect near far]
  (let [matrix (into-array Float/TYPE (range 16))]
    (Matrix/perspectiveM matrix 0 fovy aspect near far)
    matrix))

(defn get-orthographic-matrix [left right bottom top near far]
  (let [matrix (into-array Float/TYPE (range 16))]
    (Matrix/orthoM matrix 0 left right bottom top near far)
    matrix))

(defn get-inverse-matrix [m]
  (let [matrix (into-array Float/TYPE (range 16))]
    (Matrix/invertM matrix 0 m 0)
    matrix))

(defn get-transpose-matrix [m]
  (let [matrix (into-array Float/TYPE (range 16))]
    (Matrix/transposeM matrix 0 m 0)
    matrix))

(defn print-matrix [matrix]
  (let [vec (into [] matrix)]
    (dotimes [i 4]
      (dotimes [j 4]
        (let [value (max (nth vec (+ i (* j 4))))
              value (if (< (abs value) 0.001) 0.0 value)]
          (print value " ")))
      (println))))

(defn apply-matrix [matrix point]
  (let [vector (into-array Float/TYPE (conj (into [] point) 1))]
    (vec (butlast (multiply-matrix-vector matrix vector)))))
