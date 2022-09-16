(ns mockmechanics.core
  (:require [mockmechanics.library.matrix :as matrix]
            [clojure.string :refer [join]]))

(defn draw-animated-mesh! [world mesh transform]
  (let [num-vertices (/ (.capacity (:vertices-buffer mesh)) 3)
        program (get-in world [:programs (:program mesh)])
        program-index (:index program)
        attributes (:attributes program)
        uniforms (:uniforms program)
        model-matrix (matrix/multiply
                       (apply matrix/get-scale (:scale mesh))
                       (get-transform-matrix transform))
        view-matrix (:view-matrix world)
        projection-matrix (:projection-matrix world)
        mv-matrix (matrix/multiply model-matrix view-matrix)
        mvp-matrix (matrix/multiply mv-matrix projection-matrix)
        itmv-matrix (matrix/get-transpose (matrix/get-inverse mv-matrix))]

    (GL20/glUseProgram program-index)
    (GL20/glUniformMatrix4fv (:itmv-matrix uniforms) false
                             (get-float-buffer itmv-matrix))
    (GL20/glUniformMatrix4fv (:mvp-matrix uniforms) false
                             (get-float-buffer mvp-matrix))

    (GL20/glVertexAttribPointer (:position attributes) 3 GL11/GL_FLOAT
                                false 0 (:vertices-buffer mesh))
    (GL20/glEnableVertexAttribArray (:position attributes))

    (GL20/glVertexAttribPointer (:normal attributes) 3 GL11/GL_FLOAT
                                false 0 (:normals-buffer mesh))
    (GL20/glEnableVertexAttribArray (:normal attributes))

    (GL20/glVertexAttribPointer (:color attributes) 4 GL11/GL_FLOAT
                                false 0 (:colors-buffer mesh))
    (GL20/glEnableVertexAttribArray (:color attributes))

    (GL20/glVertexAttribPointer (:weights attributes) 4 GL11/GL_FLOAT
                                false 0 (:weights-buffer mesh))
    (GL20/glEnableVertexAttribArray (:weights attributes))

    (GL20/glVertexAttribPointer (:bone-indices attributes) 4 GL11/GL_FLOAT
                                false 0 (:bone-indices-buffer mesh))
    (GL20/glEnableVertexAttribArray (:bone-indices attributes))

    (GL20/glUniformMatrix4fv (:bone-matrices uniforms)
                             false
                             (get-float-buffer
                               (nth (:bone-matrices mesh)
                                    (round (:index mesh)))))

    (GL20/glUniformMatrix4fv (:inverse-bind-pose-matrices uniforms)
                             false
                             (get-float-buffer
                               (:inverse-bind-pose-matrices mesh)))

    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 num-vertices)))

(defn blender->opengl [matrix]
  (let [[m00 m10 m20 m30
         m01 m11 m21 m31
         m02 m12 m22 m32
         m03 m13 m23 m33] matrix]
    [m00 m02 (- m01) m03
     m10 m12 (- m11) m13
     m20 m22 (- m21) m23
     m30 m32 (- m31) m33]))

(defn change-extension [filename extension]
  (-> filename
      (subs 0 (.lastIndexOf filename "."))
      (str "." extension)))

(defn read-floats [lines n]
  (vec (partition n (read-string (str "[" (join " " lines) "]")))))

(defn match-order [coll order]
  (let [indices (zipmap order (range))
        f #(get indices (subs (first %) 2))]
    (sort #(< (f %1) (f %2)) coll)))

(defn parse-animation [filename order]
  (with-open [reader (clojure.java.io/reader filename)]
    (let [lines (doall (line-seq reader))
          lines (remove (fn [line]
                          (or (empty? line)
                              (.startsWith line ";;")))
                        lines)
          num-bones (read-string (subs (first lines) 12))
          num-vertices (read-string (subs (second lines) 15))
          lines (nthrest lines 3)
          n (+ (* 2 num-vertices) (count order))
          weight-lines (take n lines)
          weight-groups (create-groups #(.startsWith % ">") weight-lines)
          weight-groups (match-order weight-groups order)
          [weights bone-indices] (reduce (fn [result item]
                                           (let [item (rest item)
                                                 n (/ (count item) 2)
                                                 [a b] result]
                                             [(concat a (take n item))
                                              (concat b (drop n item))]))
                                         [() ()]
                                         weight-groups)
          weights (read-floats weights 4)
          bone-indices (read-floats bone-indices 4)
          lines (nthrest lines (inc n))
          inverse-bind-pose-matrices (read-floats (take (* num-bones 4) lines) 16)
          lines (nthrest lines (+ (* num-bones 4) 1))
          bone-matrices (read-floats lines 16)]
      {:num-bones num-bones
       :num-frames (/ (count bone-matrices) num-bones)
       :weights weights
       :bone-indices bone-indices
       :inverse-bind-pose-matrices inverse-bind-pose-matrices
       :bone-matrices bone-matrices})))

(defn create-animated-mesh-helper [vertices position rotation
                                   scale colors normals
                                   weights bone-indices]
  (let [scale (if (vector? scale)
                scale
                (vec (repeat 3 scale)))
        vertices (vec (flatten vertices))
        normals (if (empty? normals)
                  (vec (compute-normals vertices))
                  (vec (flatten normals)))
        colors (vec (flatten colors))
        weights (vec (flatten weights))
        bone-indices (vec (flatten bone-indices))]
    {:vertices vertices
     :vertices-buffer (get-float-buffer vertices)
     :normals-buffer (get-float-buffer normals)
     :weights-buffer (get-float-buffer weights)
     :bone-indices-buffer (get-float-buffer bone-indices)
     :transform (make-transform position rotation)
     :scale scale
     :colors-buffer (get-float-buffer colors)
     :draw-fn draw-animated-mesh!
     :program :animated}))

(defn invert-matrix [m]
  (vec (matrix/get-inverse (float-array m))))

(defn create-animated-mesh [filename position rotation scale]
  (with-open [reader (clojure.java.io/reader filename)]
    (let [materials (parse-materials (change-extension filename "mtl"))
          lines (filter (fn [line]
                          (or (.startsWith line "o")
                              (.startsWith line "v")
                              (.startsWith line "vn")
                              (.startsWith line "vt")
                              (.startsWith line "f")
                              (.startsWith line "usemtl")))
                        (line-seq reader))
          names (map (fn [n]
                       (let [index (.lastIndexOf n "_")]
                         (if (neg? index)
                           (subs n 2)
                           (subs n 2 index))))
                     (filter #(.startsWith % "o ") lines))
          animation (parse-animation (change-extension filename "anim")
                                     names)
          v (map parse-line (filter #(.startsWith % "v ") lines))
          n (map parse-line (filter #(.startsWith % "vn") lines))
          t (map parse-line (filter #(.startsWith % "vt") lines))
          faces (mapcat parse-line-with-slashes
                        (filter #(.startsWith % "f") lines))
          vertices (use-indices v (map first faces))
          normals (use-indices n (map last faces))
          colors (create-colors lines materials)
          weights (use-indices (:weights animation) (map first faces))
          bone-indices (use-indices (:bone-indices animation) (map first faces))
          mesh (create-animated-mesh-helper vertices position rotation scale
                                            colors normals
                                            weights bone-indices)
          convert-matrices (fn [f matrices]
                             (float-array (flatten
                                            (map f matrices))))]
      (-> mesh
          (assoc-in [:index] 0)
          (assoc-in [:num-frames] (:num-frames animation))
          (assoc-in [:inverse-bind-pose-matrices]
                    (convert-matrices (comp invert-matrix blender->opengl)
                                      (:inverse-bind-pose-matrices animation)))
          (assoc-in [:bone-matrices]
                    (vec (map (partial convert-matrices blender->opengl)
                              (partition (:num-bones animation)
                                         (:bone-matrices animation)))))))))
