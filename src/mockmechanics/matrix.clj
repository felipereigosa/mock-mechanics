(import mockmechanics.java.Matrix)

(defn get-identity-matrix []
  (float-array [1 0 0 0
                0 1 0 0
                0 0 1 0
                0 0 0 1]))

(defn get-rotation-matrix [angle x y z]
  (let [matrix (get-identity-matrix)]
    (Matrix/setRotateM matrix angle x y z)
    matrix))

(defn get-scale-matrix [x y z]
  (float-array [x 0 0 0
                0 y 0 0
                0 0 z 0
                0 0 0 1]))

(defn get-look-at-matrix [[eye-x eye-y eye-z]
                          [look-x look-y look-z]
                          [up-x up-y up-z]]
  (let [matrix (get-identity-matrix)]
    (Matrix/setLookAtM matrix
                       eye-x eye-y eye-z
                       look-x look-y look-z
                       up-x up-y up-z)
    matrix))

(defn multiply-matrices [a b]
  (let [vector-args (vector? a)
        a (if vector-args (float-array a) a)
        b (if vector-args (float-array b) b)
        matrix (get-identity-matrix)]
    (Matrix/multiplyMM matrix a b)
    (if vector-args
      (vec matrix)
      matrix)))

(defn multiply-matrix-vector [m v]
  (let [vector (float-array (repeat 4 0))]
    (Matrix/multiplyMV vector m v)
    vector))

(defn get-perspective-matrix [fovy aspect near far]
  (let [matrix (get-identity-matrix)]
    (Matrix/perspectiveM matrix fovy aspect near far)
    matrix))

(defn get-inverse-matrix [m]
  (let [matrix (get-identity-matrix)]
    (Matrix/invertM matrix m)
    matrix))

(defn get-transpose-matrix [m]
  (let [matrix (get-identity-matrix)]
    (Matrix/transposeM matrix m)
    matrix))
