

(ns temp.core)

(require '[clojure.set :refer [difference union map-invert]])
(require '[clojure.string :refer [split]])

(import org.lwjgl.glfw.GLFW)
(import org.lwjgl.system.MemoryUtil)
(import org.lwjgl.opengl.GL)
(import org.lwjgl.opengl.GL11)
(import org.lwjgl.opengl.GL12)
(import org.lwjgl.opengl.GL13)
(import org.lwjgl.opengl.GL20)
(import org.lwjgl.opengl.GL30)
(import org.lwjgl.glfw.GLFWCursorPosCallback)
(import org.lwjgl.glfw.GLFWMouseButtonCallback)
(import org.lwjgl.glfw.GLFWKeyCallback)
(import org.lwjgl.glfw.GLFWScrollCallback)
(import java.nio.FloatBuffer)
(import java.nio.IntBuffer)
(import java.nio.ByteBuffer)
(import java.nio.ByteOrder)
(import java.awt.image.BufferedImage)
(import javax.imageio.ImageIO)
(import java.io.File)
(import java.awt.Color)
(import java.awt.geom.Ellipse2D$Double)
(import java.awt.RenderingHints)
(import java.awt.Font)
(import java.awt.Polygon)
(import java.awt.geom.AffineTransform)
(import java.awt.AlphaComposite)
(import javax.vecmath.Vector3f)
(import javax.vecmath.Quat4f)
(import javax.vecmath.AxisAngle4f)
(import com.bulletphysics.linearmath.Transform)
(import java.net.ServerSocket)
(import java.net.Socket)
(import java.io.BufferedReader)
(import java.io.InputStreamReader)
(import javax.vecmath.Matrix4f)

(load "util")
(load "vector")
(load "world")
(load "matrix")
(load "analytic-geometry")
(load "xml")
(load "svg")
(load "physics")

(defn create-world! [])
(defn draw-world! [world])
(defn update-world [world elapsed] world)
(defn key-pressed [world event] world)
(defn key-released [world event] world)
(defn mouse-pressed [world event] world)
(defn mouse-moved [world event] world)
(defn mouse-released [world event] world)
(defn mouse-scrolled [world event] world)

(def to-run (atom nil))

(defn run-pending! []
  (swap! to-run (fn [tr]
                  (when (not (nil? tr))
                    (tr))
                  nil)))

(defn create-key-handler! [window]
  (let [key-handler (proxy [GLFWKeyCallback] []
                      (invoke [window key scancode action mods]
                        (cond
                          (= action GLFW/GLFW_PRESS)
                          (try
                            (swap! world (fn [w] (key-pressed w {:code key})))
                            (catch Exception e))

                          (= action GLFW/GLFW_RELEASE)
                          (try
                            (swap! world (fn [w] (key-released w {:code key})))
                            (catch Exception e)))))]

    (GLFW/glfwSetKeyCallback window key-handler)
    key-handler))

(def window-width 685)
(def window-height 715)
(def mouse-x (atom 0))
(def mouse-y (atom 0))
(def mouse-button (atom nil))

(defn get-button-name [value]
  (get {0 :left
        1 :right
        2 :middle} value))

(defn create-mouse-handler! [window]
  (let [mouse-handler (proxy [GLFWMouseButtonCallback] []
                        (invoke [window button action mods]
                          (let [event {:x @mouse-x
                                       :y @mouse-y
                                       :button (get-button-name button)}]
                            (reset! mouse-button (get-button-name button))
                            (cond
                              (= action GLFW/GLFW_PRESS)
                              (try
                                (swap! world (fn [w] (mouse-pressed w event)))
                                (catch Exception e))

                              (= action GLFW/GLFW_RELEASE)
                              (try
                                (swap! world (fn [w] (mouse-released w event)))
                                (catch Exception e))))))]

    (GLFW/glfwSetMouseButtonCallback window mouse-handler)
    mouse-handler))

(defn create-mouse-motion-handler! [window]
  (let [mouse-motion-handler (proxy [GLFWCursorPosCallback] []
                               (invoke [window x y]
                                 (reset! mouse-x x)
                                 (reset! mouse-y y)
                                 (try
                                   (swap! world
                                          (fn [w]
                                            (mouse-moved w {:x x :y y
                                                            :button @mouse-button})))
                                   (catch Exception e))))]

    (GLFW/glfwSetCursorPosCallback window mouse-motion-handler)
    mouse-motion-handler))

(defn create-mouse-scroll-handler! [window]
  (let [mouse-scroll-handler (proxy [GLFWScrollCallback] []
                               (invoke [window x y]
                                 (try
                                   (swap! world
                                          (fn [w]
                                            (mouse-scrolled w {:x x :y y})))
                                   (catch Exception e))))]

    (GLFW/glfwSetScrollCallback window mouse-scroll-handler)
    mouse-scroll-handler))

(defn loop! [window]
  (try
    (create-world!)
    (catch Exception e))
  (while true
    (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT
                          GL11/GL_DEPTH_BUFFER_BIT))
    (try
      (run-pending!)
      (catch Exception e))
    (try
      (draw-world! @world)
      (catch Exception e))
    (try
      (swap! world (fn [w] (update-world w 16)))
      (catch Exception e))

    (GLFW/glfwSwapBuffers window)
    (GLFW/glfwPollEvents)))

(defn window-init! []
  (.start (new Thread (proxy [Runnable] []
                        (run []
    (GLFW/glfwInit)
    (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
    (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_FALSE)
    (GLFW/glfwWindowHint GLFW/GLFW_SAMPLES 8)

    (let [window (GLFW/glfwCreateWindow window-width
                                        window-height
                                        "Window" MemoryUtil/NULL MemoryUtil/NULL)
          key-handler (create-key-handler! window)
          mouse-handler (create-mouse-handler! window)
          mouse-motion-handler (create-mouse-motion-handler! window)
          mouse-scroll-handler (create-mouse-scroll-handler! window)]

      (GLFW/glfwMakeContextCurrent window)
      (GLFW/glfwSwapInterval 1)
      (GLFW/glfwShowWindow window)

      (GL/createCapabilities)

      (GL11/glViewport 0 0 window-width window-height)
      (GL11/glClearColor 0.0 0.0 0.0 0.0)

      (GL11/glEnable GL11/GL_BLEND)
      (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)

      (GL11/glEnable GL11/GL_DEPTH_TEST)

      (loop! window)

      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwTerminate)))))))

;;-------------------------------------------------------------------------------;;
;; opengl

(defmacro gl-thread [form] ;;############################## forms
  `(reset! to-run (fn [] ~form)))

(def out (atom nil))

;; error with changing buffer #############################################
(defn gl-println [& forms]
  (binding [*out* @out]
    (apply println forms)))

(defn check-shader [shader]
  (let [status (GL20/glGetShaderi shader GL20/GL_COMPILE_STATUS)]
    (when (= status 0)
      (gl-println (GL20/glGetShaderInfoLog shader))
      (GL20/glDeleteShader shader))))

(defn check-program [program]
  (let [status (GL20/glGetProgrami program GL20/GL_LINK_STATUS)]
    (when (= status 0)
      (gl-println (GL20/glGetProgramInfoLog program))
      (GL20/glDeleteProgram program))))

(defn load-shader [filename type]
  (let [shader (GL20/glCreateShader (if (= type :fragment)
                                      GL20/GL_FRAGMENT_SHADER
                                      GL20/GL_VERTEX_SHADER))
        source (slurp filename)]
    (GL20/glShaderSource shader source)
    (GL20/glCompileShader shader)
    (check-shader shader)
    shader))

(defn compile-program [vertex-filename fragment-filename]
  (let [vertex-shader (load-shader vertex-filename :vertex)
        fragment-shader (load-shader fragment-filename :fragment)
        program (GL20/glCreateProgram)]

    (GL20/glAttachShader program vertex-shader)
    (GL20/glAttachShader program fragment-shader)
    (GL20/glLinkProgram program)
    (check-program program)
    program))

(defn get-float-buffer [seq]
  (let [array (if (vector? seq)
                (into-array Float/TYPE seq)
                seq)
        bb (ByteBuffer/allocateDirect (* (count array) 4))]
    (.order bb (ByteOrder/nativeOrder))
    (let [fb (.asFloatBuffer bb)]
      (.put fb array)
      (.position fb 0)
      fb)))

(defn get-short-buffer [vec]
  (let [array (into-array Short/TYPE vec)
        bb (ByteBuffer/allocateDirect (* (count vec) 4))]
    (.order bb (ByteOrder/nativeOrder))
    (let [fb (.asShortBuffer bb)]
      (.put fb array)
      (.position fb 0)
      fb)))

(defn get-int-buffer [vec]
  (let [array (into-array Integer/TYPE vec)
        bb (ByteBuffer/allocateDirect (* (count vec) 4))]
    (.order bb (ByteOrder/nativeOrder))
    (let [fb (.asIntBuffer bb)]
      (.put fb array)
      (.position fb 0)
      fb)))

(defn get-byte-buffer [vec]
  (let [array (into-array Byte/TYPE vec)
        bb (ByteBuffer/allocateDirect (* (count vec) 4))]
    (.order bb (ByteOrder/nativeOrder))
    (.put bb array)
    (.position bb 0)
    bb))

(defn buffer->vector [buffer]
  (.position buffer 0)
  (let [vec (into [] (map (fn [i] (.get buffer)) (range (.capacity buffer))))]
    (.position buffer 0)
    vec))

(defn print-program-info [index]
  (gl-thread
   (do
     (gl-println "--------------------")
     (gl-println "program index =" index)
     (gl-println "active attributes ="
                 (GL20/glGetProgrami index GL20/GL_ACTIVE_ATTRIBUTES)))))

(defn make-int-buffer [size]
  (let [bb (ByteBuffer/allocateDirect size)]
    (.order bb (ByteOrder/nativeOrder))
    (.asIntBuffer bb)))

(defn get-attribute-names [index]
  (let [num-active (GL20/glGetProgrami index GL20/GL_ACTIVE_ATTRIBUTES)]
    (map (fn [i]
           (let [size (make-int-buffer 100)
                 type (make-int-buffer 100)]
             (GL20/glGetActiveAttrib index i size type))) (range num-active))))

(defn get-uniform-names [index]
  (let [num-active (GL20/glGetProgrami index GL20/GL_ACTIVE_UNIFORMS)]
    (map (fn [i]
           (let [size (make-int-buffer 100)
                 type (make-int-buffer 100)]
             (GL20/glGetActiveUniform index i size type))) (range num-active))))

(defn location-name->keyword [name]
  (keyword (apply str (clojure.string/replace name #"_" "-"))))

(defn create-program [base-name]
  (let [v-name (str "resources/" base-name "-vert.glsl")
        f-name (str "resources/" base-name "-frag.glsl")
        index (compile-program v-name f-name)
        attribute-names (get-attribute-names index)
        uniform-names (get-uniform-names index)
        attributes (apply merge
                          (map (fn [name]
                                 {(location-name->keyword name)
                                  (GL20/glGetAttribLocation index name)})
                               attribute-names))
        uniforms (apply merge
                        (map (fn [name]
                               {(location-name->keyword name)
                                (GL20/glGetUniformLocation index name)})
                             uniform-names))]

    {:index index
     :uniforms uniforms
     :attributes attributes}))

;;-------------------------------------------------------------------------------;;
;; textures

(defn new-image [width height]
  (new BufferedImage
       width
       height
       (. BufferedImage TYPE_INT_ARGB)))

(defn open-image [filename]
  (ImageIO/read (new File filename)))

(defn get-image-width [image]
  (.getWidth image))

(defn get-image-height [image]
  (.getHeight image))

(defn get-image-graphics [image]
  (let [g (.getGraphics image)]
    (.setRenderingHint g
                       RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    g))

(defn clear
  ([image]
   (let [g (get-image-graphics image)
         w (get-image-width image)
         h (get-image-height image)]
     (.setComposite g (AlphaComposite/getInstance AlphaComposite/CLEAR))
     (.fillRect g 0 0 w h)
     (.setComposite g (AlphaComposite/getInstance AlphaComposite/SRC_OVER))))
  ([image color]
   (let [g (get-image-graphics image)
         w (get-image-width image)
         h (get-image-height image)]
     (.setColor g (get-color color))
     (.fillRect g 0 0 w h))))

(defn draw-pixel [image color x y]
  (when (and
         (<= 0 x (dec (get-image-width image)))
         (<= 0 y (dec (get-image-height image))))
    (.setRGB image (int x) (int y) (.getRGB (get-color color))))
  image)

(defn get-pixel [image x y]
  (new Color (.getRGB image x y)))

(defn fill-rect [image color x y w h]
  (let [g (get-image-graphics image)
        hw (/ w 2)
        hh (/ h 2)]
    (.setColor g (get-color color))
    (.fillRect g (- x hw) (- y hh) w h)))

(defn draw-rect [image color x y w h]
  (let [g (get-image-graphics image)
        hw (/ w 2)
        hh (/ h 2)]
    (.setColor g (get-color color))
    (.drawRect g (- x hw) (- y hh) w h)))

(defn fill-circle [image color x y r]
  (let [g (get-image-graphics image)]
    (.setColor g (get-color color))
    (.fill g (new Ellipse2D$Double (- x r) (- y r) (* 2 r) (* 2 r)))))

(defn draw-circle [image color x y r]
  (let [g (get-image-graphics image)]
    (.setColor g (get-color color))
    (.draw g (new Ellipse2D$Double (- x r) (- y r) (* 2 r) (* 2 r)))))

(defn draw-text [image color text x y size]
  (let [g (get-image-graphics image)]
    (.setFont g (new Font "Dialog" Font/PLAIN size))
    (.setColor g (get-color color))
    (.drawString g text x y)))

(defn draw-ellipse [image color rect]
  (let [g (get-image-graphics image)]
    (.setColor g (get-color color))
    (let [w (* (:w rect) 1.0)
          h (* (:h rect) 1.0)
          ellipse (new Ellipse2D$Double
                       (- (/ w 2))
                       (- (/ h 2))
                       w h)
          angle (to-radians (:angle rect))
          ellipse (.createTransformedShape
                   (AffineTransform/getRotateInstance angle)
                   ellipse)
          ellipse (.createTransformedShape
                   (AffineTransform/getTranslateInstance (:x rect) (:y rect))
                   ellipse)]
      (.draw g ellipse)
      (fill-circle image color (:x rect) (:y rect) 2))))

(defn draw-line [image color x1 y1 x2 y2]
  (let [g (get-image-graphics image)]
    (.setColor g (get-color color))
    (.drawLine g x1 y1 x2 y2)))

(defn fill-polygon [image color points]
  (let [g (get-image-graphics image)]
    (.setColor g (get-color color))
    (let [polygon (new Polygon)]
      (doseq [[x y] points]
        (.addPoint polygon x y))
      (.fillPolygon g polygon))))

(defn draw-polygon [image color points]
  (let [g (get-image-graphics image)]
    (.setColor g (get-color color))
    (let [polygon (new Polygon)]
      (doseq [[x y] points]
        (.addPoint polygon x y))
      (.drawPolygon g polygon))))

(defn draw-image [image image2 x y & corner]
  (let [g (get-image-graphics image)
        w (get-image-width image2)
        h (get-image-height image2)
        x (if (first corner) x (- x (/ w 2)))
        y (if (first corner) y (- y (/ h 2)))]
    (gl-println corner)
    (.drawImage g image2 (int x) (int y) nil)))

(def pixels (make-array Integer/TYPE (* window-width window-height)))
(def bb (ByteBuffer/allocateDirect (* window-width window-height 4)))

;;########################################################################
(defn image->buffer [image]
  (let [;; w (.getWidth image)
        ;; h (.getHeight image)
        ;; pixels (make-array Integer/TYPE (* w h))
        ;; bb (ByteBuffer/allocateDirect (* w h 4))
        ]
    (.getRGB image 0 0 window-width window-height pixels 0 window-width)
    (let [ib (.asIntBuffer bb)]
      (.put ib pixels)
      bb)))

(defn reset-texture [mesh]
  (let [id (:texture-id mesh)
        image (:image mesh)
        width (get-image-width image)
        height (get-image-height image)
        buffer (image->buffer image)
        ]

    (GL13/glActiveTexture GL13/GL_TEXTURE0)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D id)

    (GL11/glTexSubImage2D GL11/GL_TEXTURE_2D 0 0 0 width height
                       GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE buffer)
    mesh))

(defn set-texture [mesh]
  (let [id (:texture-id mesh)
        image (:image mesh)
        width (get-image-width image)
        height (get-image-height image)
        buffer (image->buffer image)]
    (GL13/glActiveTexture GL13/GL_TEXTURE0)

    (GL11/glBindTexture GL11/GL_TEXTURE_2D id)
    (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGBA width height 0
                       GL11/GL_RGBA GL11/GL_UNSIGNED_BYTE buffer)
    (GL30/glGenerateMipmap GL11/GL_TEXTURE_2D)

    (GL11/glTexParameteri GL11/GL_TEXTURE_2D
                          GL11/GL_TEXTURE_WRAP_S GL12/GL_CLAMP_TO_EDGE)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D
                          GL11/GL_TEXTURE_WRAP_T GL12/GL_CLAMP_TO_EDGE)
    ;; (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER
    ;;                       GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER
                          GL11/GL_LINEAR_MIPMAP_LINEAR)
    mesh))

;;-------------------------------------------------------------------------------;;
;; meshes

(defn axis-angle->quaternion [[ax ay az] angle]
  (let [quat (new Quat4f)]
    (.set quat (new AxisAngle4f ax ay az (to-radians angle)))
    quat))

(defn quaternion->axis-angle [quaternion]
  (let [axis-angle (new AxisAngle4f)]
    (.set axis-angle quaternion)
    [(.-x axis-angle) (.-y axis-angle) (.-z axis-angle) (to-degrees (.-angle axis-angle))]))

(defn make-transform [[x y z] [ax ay az angle]]
  (let [transform (new Transform)
        orientation (new Quat4f)]
    (.set (.origin transform) (new Vector3f x y z))
    (.set orientation (axis-angle->quaternion [ax ay az] angle))
    (.setRotation transform orientation)
    transform))

(defn get-transform-position [transform]
  (let [vec (.-origin transform)]
    [(.-x vec) (.-y vec) (.-z vec)]))

(defn get-transform-rotation [transform]
  (let [rotation (new Quat4f)]
    (quaternion->axis-angle (.getRotation transform rotation))))

(declare println!)

(defn print-transform [transform]
  (println! [(get-transform-position transform)
             (get-transform-rotation transform)]))

(defn get-transform-matrix [transform]
  (let [matrix (into-array Float/TYPE (range 16))]
    (.getOpenGLMatrix transform matrix)
    matrix))

(defn get-rotation-component [transform]
  (let [rotation (get-transform-rotation transform)]
    (make-transform [0 0 0] rotation)))

(declare matrix->transform)

(defn get-inverse-transform [transform]
  (let [m (get-transform-matrix transform)
        im (get-inverse-matrix m)]
    (matrix->transform im)))

(defn set-mesh-position [mesh position]
  (assoc-in mesh [:transform] (make-transform position [1 0 0 0])))

(defn get-mesh-position [mesh]
  (get-transform-position (:transform mesh)))

(defn get-mesh-rotation [mesh]
  (get-transform-rotation (:transform mesh)))

(defn set-mesh-color [mesh color]
  (let [color (get-color color)
        r (/ (get-red color) 255)
        g (/ (get-green color) 255)
        b (/ (get-blue color) 255)]
    (assoc-in mesh [:color] [r g b 1.0])))

(defn compute-normals [vertices]
  (flatten (map (fn [[a b c]]
                  (let [v1 (vector-subtract b a)
                        v2 (vector-subtract c a)
                        v3 (vector-cross-product v1 v2)
                        nv3 (vector-normalize v3)]
                    (list nv3 nv3 nv3)))
                (partition 3 (partition 3 vertices)))))

(defn is-image? [object]
  (instance? java.awt.image.BufferedImage object))

(defn join-keywords [& keywords]
  (keyword (apply str (interpose "-" (map (fn [k]
                                            (subs (str k) 1)) keywords)))))

(defn quaternion-from-normal [normal]
  (let [normal (vector-normalize normal)
        axis (vector-cross-product [0 1 0] normal)
        angle (vector-angle normal [0 1 0])]
    (if (= axis [0.0 0.0 0.0])
      [0 1 0 0]
      (conj axis angle))))

(defn point-mesh-towards [mesh direction]
  (let [position (get-mesh-position mesh)
        rotation (quaternion-from-normal direction)
        transform (make-transform position rotation)]
  (assoc-in mesh [:transform] transform)))

(defn get-cube-vertices []
  (let [corners [[-0.5 0.5 0.5] [0.5 0.5 0.5] [-0.5 -0.5 0.5] [0.5 -0.5 0.5]
                 [-0.5 0.5 -0.5] [0.5 0.5 -0.5] [-0.5 -0.5 -0.5] [0.5 -0.5 -0.5]]
        indices [2 3 0   3 1 0   4 7 6   4 5 7
                 2 0 6   6 0 4   7 1 3   5 1 7
                 0 1 5   0 5 4   7 3 2   6 7 2]]
    (into [] (flatten (map (fn [index]
                             (nth corners index)) indices)))))

(defn get-cube-texture-coordinates []
  (let [a1 (/ 1 3)
        a2 (* 2 a1)
        a3 (* 3 a1)
        b1 0.5
        b2 1.0]
    [ 0 b1   a1 b1    0  0   a1 b1   a1  0    0  0
     a2  0   a1 b1   a2 b1   a2  0   a1  0   a1 b1
     a3 b1   a3  0   a2 b1   a2 b1   a3  0   a2  0
     a1 b2    0 b1    0 b2   a1 b1    0 b1   a1 b2
     a1 b2   a2 b2   a2 b1   a1 b2   a2 b1   a1 b1
     a3 b2   a3 b1   a2 b1   a2 b2   a3 b2   a2 b1]))

(defn get-plane-vertices []
  [-0.5 -0.5 0   0.5 -0.5 0
   0.5 0.5 0    -0.5 -0.5 0
   0.5 0.5 0    -0.5 0.5 0])

(defn get-plane-texture-coordinates []
  [0 1   1 1   1 0
   0 1   1 0   0 0])

(defn get-circle-vertices [r y divisions]
  (let [angles (map (fn [i]
                      (* i (/ 360 divisions)))
                    (range divisions))
        vertices (map (fn [angle]
                        [(* r (cos angle)) y (* r (sin angle))])
                      angles)]
    vertices))

(defn get-cone-vertices []
  (let [divisions 40
        bottom-circle (get-circle-vertices 1 0 divisions)
        top-point [0 1 0]
        top-triangles (map (fn [i]
                             [top-point
                              (nth bottom-circle (mod (inc i) divisions))
                              (nth bottom-circle i)])
                          (range divisions))
        bottom-point [0 0 0]
        bottom-triangles (map (fn [i]
                                [(nth bottom-circle i)
                                 (nth bottom-circle (mod (inc i) divisions))
                                 bottom-point])
                          (range divisions))]
    (into [] (flatten (conj top-triangles
                            bottom-triangles)))))

(defn get-cone-texture-coordinates []
  ;;######################################################
  [])

(defn get-sphere-vertices [level]
  (letfn [(rec [[a b c] l revert]
            (if (= l 0)
              (list [a b c])
              (let [mab (vector-multiply (vector-normalize (vector-add a b)) 1)
                    mbc (vector-multiply (vector-normalize (vector-add b c)) 1)
                    mca (vector-multiply (vector-normalize (vector-add c a)) 1)
                    t1 (if revert [mca mab a] [a mab mca])
                    t2 (if revert [mbc b mab] [mab b mbc])
                    t3 (if revert [c mbc mca] [mca mbc c])
                    t4 (if revert [mca mbc mab] [mab mbc mca])
                    ]
                (concat
                 (rec t1 (dec l) false)
                 (rec t2 (dec l) false)
                 (rec t3 (dec l) false)
                 (rec t4 (dec l) false)))))]
    (let [q0 (rec [[0 0 1] [1 0 0] [0 1 0]] level false)
          q1 (rec [[0 0 1] [(- 1) 0 0] [0 1 0]] level true)
          q2 (rec [[0 0 (- 1)] [1 0 0] [0 1 0]] level true)
          q3 (rec [[0 0 (- 1)] [(- 1) 0 0] [0 1 0]] level false)
          q4 (rec [[0 0 1] [1 0 0] [0 (- 1) 0]] level true)
          q5 (rec [[0 0 1] [(- 1) 0 0] [0 (- 1) 0]] level false)
          q6 (rec [[0 0 (- 1)] [1 0 0] [0 (- 1) 0]] level false)
          q7 (rec [[0 0 (- 1)] [(- 1) 0 0] [0 (- 1) 0]] level true)]
      (into [] (flatten (concat q0 q1 q2 q3 q4 q5 q6 q7))))))

(defn get-sphere-texture-coordinates []
  ;;######################################################
  [])

(defn parse-line [line]
  (map read-string (into [] (.split (.trim (.substring line 1)) " "))))

(defn get-model-vertices [filename]
  (letfn [(parse-line [line]
            (map read-string (into [] (.split (.trim (.substring line 1)) " "))))]
    (with-open [reader (clojure.java.io/reader filename)]
      (let [lines (doall (line-seq reader))
            vertices (into [] (filter (comp not nil?) (map (fn [line]
                                                             (if (= (first line) \v)
                                                               (parse-line line)))
                                                           lines)))
            faces (flatten (filter (comp not nil?) (map (fn [line]
                                                          (if (= (first line) \f)
                                                            (parse-line line)))
                                                        lines)))]
        (into [] (mapcat (fn [f]
                           (nth vertices (dec f)))
                         faces))))))

(defn get-model-texture-coordinates []
  ;;######################################################
  [])

(defn get-cylinder-vertices []
  (let [divisions 40
        top-circle (get-circle-vertices 1 0.5 divisions)
        bottom-circle (get-circle-vertices 1 -0.5 divisions)
        up-triangles (map (fn [i]
                            [(nth bottom-circle (mod (inc i) divisions))
                             (nth bottom-circle i)
                             (nth top-circle i)])
                          (range divisions))
        down-triangles (map (fn [i]
                              [(nth top-circle i)
                               (nth top-circle (mod (inc i) divisions))
                               (nth bottom-circle (mod (inc i) divisions))])
                            (range divisions))
        top-point [0 0.5 0]
        top-triangles (map (fn [i]
                             [(nth top-circle (mod (inc i) divisions))
                              (nth top-circle i)
                              top-point])
                          (range divisions))
        bottom-point [0 -0.5 0]
        bottom-triangles (map (fn [i]
                                [(nth bottom-circle i)
                                 (nth bottom-circle (mod (inc i) divisions))
                                 bottom-point])
                          (range divisions))]
    (into [] (flatten (conj up-triangles
                            down-triangles
                            top-triangles
                            bottom-triangles)))))

(defn get-cylinder-texture-coordinates []
  ;;###############################################
  )

(defn get-rotated-vertices [cross-vertices ring-divisions]
  (let [cross-divisions (count cross-vertices)

        ring-vertices (map (fn [i]
                             (let [angle (* i (/ 360 ring-divisions))]
                               (map (fn [v]
                                      (vector-rotate v [0 1 0] angle))
                                    cross-vertices)))
                           (range ring-divisions))

        up-indices (map (fn [i]
                          (let [j (mod (inc i) cross-divisions)]
                            (list i j j)))
                        (range cross-divisions))

        down-indices (map (fn [i]
                            (let [j (mod (inc i) cross-divisions)]
                              (list j i i)))
                          (range cross-divisions))

        up-vertices (map (fn [i]
                           (let [j (mod (inc i) ring-divisions)
                                 i-ring (nth ring-vertices i)
                                 j-ring (nth ring-vertices j)]
                             (map (fn [[a b c]]
                                    (list (nth j-ring c)
                                          (nth i-ring b)
                                          (nth i-ring a)))
                                  up-indices)))
                         (range ring-divisions))

        down-vertices (map (fn [i]
                             (let [j (mod (inc i) ring-divisions)
                                   i-ring (nth ring-vertices i)
                                   j-ring (nth ring-vertices j)]
                               (map (fn [[a b c]]
                                      (list (nth i-ring c)
                                            (nth j-ring b)
                                            (nth j-ring a)))
                                    down-indices)))
                           (range ring-divisions))]
    (into [] (flatten (conj down-vertices up-vertices)))))

(defn get-torus-vertices [inner-radius outer-radius]
  (let [ring-divisions 50
        cross-divisions 50
        cross-radius (/ (- outer-radius inner-radius) 2)
        cross-vertices (get-circle-vertices cross-radius 0 cross-divisions)
        ring-radius (+ inner-radius cross-radius)
        cross-vertices (map (fn [v]
                              (vector-add [ring-radius 0 0] (vector-rotate v [1 0 0] -90)))
                            cross-vertices)]
    (get-rotated-vertices cross-vertices ring-divisions)))

(defn get-torus-texture-coordinates []
  []) ;;##################################################

(defn get-disk-vertices [inner-radius outer-radius height]
  (let [border-divisions 50
        ring-divisions 50
        border-radius (/ height 2)
        border-vertices (get-circle-vertices border-radius 0 border-divisions)
        border-vertices (take (inc (/ border-divisions 2)) border-vertices)

        border-vertices (map (fn [v]
                               (vector-rotate v [1 0 0] -90))
                             border-vertices)

        border-vertices (map (fn [v]
                               (vector-rotate v [0 0 1] 90))
                             border-vertices)

        inner-border-vertices (map (fn [v]
                                     (vector-add v [(+ inner-radius border-radius) 0 0]))
                                   border-vertices)

        outer-border-vertices (map (fn [v]
                                     (vector-rotate v [0 0 1] 180))
                                   border-vertices)

        outer-border-vertices (map (fn [v]
                                     (vector-add v [(- outer-radius border-radius) 0 0]))
                                   outer-border-vertices)

        cross-vertices (into [] (flatten (cons inner-border-vertices outer-border-vertices)))
        cross-vertices (partition 3 cross-vertices)]
    (get-rotated-vertices cross-vertices ring-divisions)))

(defn get-disk-texture-coordinates [] ;;#####################################
  [])

(defn draw-lighted-mesh! [world mesh transform]
  (let [num-vertices (/ (.capacity (:vertices-buffer mesh)) 3)
        program (get-in world [:programs (:program mesh)])
        program-index (:index program)
        attributes (:attributes program)
        uniforms (:uniforms program)
        model-matrix (multiply-matrices
                      (apply get-scale-matrix (:scale mesh))
                      (get-transform-matrix transform))
        view-matrix (:view-matrix world)
        projection-matrix (:projection-matrix world)
        mv-matrix (multiply-matrices model-matrix view-matrix)
        mvp-matrix (multiply-matrices mv-matrix projection-matrix)
        itmv-matrix (get-transpose-matrix (get-inverse-matrix mv-matrix))]

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

    (if-let [[r g b a] (:color mesh)]
      (GL20/glUniform4f (:material-color uniforms) r g b a)
      (do
        (GL20/glVertexAttribPointer (:texture-coordinates attributes) 2 GL11/GL_FLOAT
                                    false 0 (:texture-coordinates-buffer mesh))
        (GL20/glEnableVertexAttribArray (:texture-coordinates attributes))
        (GL13/glActiveTexture GL13/GL_TEXTURE0)
        (GL11/glBindTexture GL11/GL_TEXTURE_2D (:texture-id mesh))
        (GL20/glUniform1i (:texture-diffuse uniforms) 0)))

    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 num-vertices)))

(defn draw-colored-mesh! [world mesh transform]
  (let [num-vertices (/ (.capacity (:vertices-buffer mesh)) 3)
        program (get-in world [:programs (:program mesh)])
        program-index (:index program)
        attributes (:attributes program)
        uniforms (:uniforms program)
        model-matrix (multiply-matrices
                      (apply get-scale-matrix (:scale mesh))
                      (get-transform-matrix transform))
        view-matrix (:view-matrix world)
        projection-matrix (:projection-matrix world)
        mv-matrix (multiply-matrices model-matrix view-matrix)
        mvp-matrix (multiply-matrices mv-matrix projection-matrix)
        itmv-matrix (get-transpose-matrix (get-inverse-matrix mv-matrix))]
    
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

    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 num-vertices)))

(defn create-mesh [vertices position rotation scale skin tex-coords]
  (let [base-mesh {:vertices (into-array Double/TYPE (map double vertices))
                   :vertices-buffer (get-float-buffer vertices)
                   :normals-buffer (get-float-buffer (into [] (compute-normals vertices)))
                   :transform (make-transform position rotation)
                   :draw-fn draw-lighted-mesh!
                   :scale scale}]
    (if (not (is-image? skin))
      (let [color (get-color skin)
            r (/ (get-red color) 255.0)
            g (/ (get-green color) 255.0)
            b (/ (get-blue color) 255.0)]
        (-> base-mesh
            (assoc-in [:color] [r g b 1.0])
            (assoc-in [:program] :flat)))
      (let [texture-id (GL11/glGenTextures)]
        (-> base-mesh
            (assoc-in [:image] skin)
            (assoc-in [:program] :textured)
            (assoc-in [:texture-coordinates-buffer] (get-float-buffer tex-coords))
            (assoc-in [:texture-id] texture-id)
            (set-texture))))))

(defn create-cube-mesh [position rotation scale skin]
  (create-mesh (get-cube-vertices)
                   position rotation scale skin (get-cube-texture-coordinates)))

(defn create-plane-mesh [position rotation scale skin]
  (create-mesh (get-plane-vertices)
                   position rotation scale skin (get-plane-texture-coordinates)))

(defn create-cone-mesh [position rotation scale skin]
  (create-mesh (get-cone-vertices)
               position rotation scale skin (get-cone-texture-coordinates)))

(defn create-sphere-mesh [position rotation scale skin]
  (create-mesh (get-sphere-vertices 4)
               position rotation scale skin (get-sphere-texture-coordinates)))

(defn create-model-mesh [filename position rotation scale skin]
  (create-mesh (get-model-vertices filename)
               position rotation scale skin (get-model-texture-coordinates)))

(defn create-cylinder-mesh [position rotation scale skin]
  (create-mesh (get-cylinder-vertices)
               position rotation scale skin (get-cylinder-texture-coordinates)))

(defn create-torus-mesh [inner-radius outer-radius position rotation scale skin]
  (let [mesh (create-mesh (get-torus-vertices inner-radius outer-radius)
                          position rotation scale skin (get-torus-texture-coordinates))]
    (-> mesh
        (assoc-in [:inner-radius] inner-radius) ;;#####################################
        (assoc-in [:outer-radius] outer-radius))))

(defn create-disk-mesh [inner-radius outer-radius height position rotation scale skin]
  (let [mesh (create-mesh (get-disk-vertices inner-radius outer-radius height)
                          position rotation scale skin (get-disk-texture-coordinates))]
    (-> mesh
        (assoc-in [:inner-radius] inner-radius) ;;##########################################
        (assoc-in [:outer-radius] outer-radius)
        (assoc-in [:height] height))))

(defn draw-lines! [world mesh transform]
  (let [num-vertices (/ (.capacity (:vertices-buffer mesh)) 3)
        [r g b a] (:color mesh)
        program (get-in world [:programs (:program mesh)])
        program-index (:index program)
        attributes (:attributes program)
        uniforms (:uniforms program)
        model-matrix (multiply-matrices
                      (apply get-scale-matrix (:scale mesh))
                      (get-transform-matrix transform))
        view-matrix (:view-matrix world)
        projection-matrix (:projection-matrix world)
        mv-matrix (multiply-matrices model-matrix view-matrix)
        mvp-matrix (multiply-matrices mv-matrix projection-matrix)]

    (GL20/glUseProgram program-index)
    (GL20/glUniformMatrix4fv (:mvp-matrix uniforms) false
                             (get-float-buffer mvp-matrix))

    (GL20/glVertexAttribPointer (:position attributes) 3 GL11/GL_FLOAT
                                false 0 (:vertices-buffer mesh))
    (GL20/glEnableVertexAttribArray (:position attributes))

    (GL20/glUniform4f (:material-color uniforms) r g b a)
    (GL11/glDrawArrays GL11/GL_LINES 0 num-vertices)))

(defn create-line-mesh [a b color]
  (let [vertices (into [] (concat a b))
        color (get-color color)
        r (/ (get-red color) 255)
        g (/ (get-green color) 255)
        b (/ (get-blue color) 255)
        line {:vertices-buffer (get-float-buffer vertices)
              :color [r g b 1.0]
              :transform (make-transform [0.0 0.0 0.0] [0 1 0 0])
              :program :basic
              :scale [1 1 1]
              :draw-fn draw-lines!}]
    line))

(defn create-circle-mesh [center normal radius color]
  (let [vertices (into [] (flatten
                           (rotate-list
                            (mapcat (fn [x]
                                      [x x])
                                    (get-circle-vertices radius 0.0 40)))))
        color (get-color color)
        r (/ (get-red color) 255)
        g (/ (get-green color) 255)
        b (/ (get-blue color) 255)
        circle {:vertices-buffer (get-float-buffer vertices)
                :color [r g b 1.0]
                :transform (make-transform center
                                           (quaternion-from-normal normal))
                :program :basic
                :scale [1 1 1]
                :draw-fn draw-lines!}]
    circle))

(defn create-path-mesh [vertices color]
  (let [vertices (interleave vertices (rest vertices))
        vertices (into [] (flatten vertices))
        color (get-color color)
        r (/ (get-red color) 255)
        g (/ (get-green color) 255)
        b (/ (get-blue color) 255)
        path {:vertices-buffer (get-float-buffer vertices)
              :color [r g b 1.0]
              :transform (make-transform [0 0 0] [1 0 0 0])
              :program :basic
              :scale [1 1 1]
              :draw-fn draw-lines!}]
    path))

(defn draw-mesh! [world mesh]
  (let [draw-fn (:draw-fn mesh)]
    (draw-fn world mesh (:transform mesh))))

(defn get-grid-vertices [num-cells cell-size]
  (let [hw (/ (* cell-size num-cells) 2)
        seq (map (fn [val]
                   (- (* val cell-size) hw))
                 (range (inc num-cells)))
        min (first seq)
        max (last seq)
        z-parallel (mapcat (fn [x]
                             [x 0 min x 0 max])
                           seq)
        x-parallel (mapcat (fn [z]
                             [min 0 z max 0 z])
                           seq)]
    (into [] (concat z-parallel x-parallel))))

(defn create-grid-mesh [world num-cells size]
  (let [vertices (get-grid-vertices num-cells size)
        color (get-color :black)
        r (/ (get-red color) 255)
        g (/ (get-green color) 255)
        b (/ (get-blue color) 255)
        grid {:vertices-buffer (get-float-buffer vertices)
              :color [r g b 1.0]
              :transform (make-transform [0.0 0.0 0.0] [0 1 0 0])
              :program :basic
              :scale [1 1 1]
              :draw-fn draw-lines!}]
    (assoc-in world [:background-meshes :grid] grid)))

(defn create-vector-mesh [world position direction color base-name
                          & {:keys [width tip] :or {width 0.1 tip 1}}]
  (let [direction (vector-multiply direction (- 1 (/ tip (vector-length direction))))
        p1 (vector-add position (vector-multiply direction 0.5))
        p2 (vector-add position direction)
        rotation (quaternion-from-normal direction)
        axis-name (join-keywords base-name :axis)
        tip-name (join-keywords base-name :tip)]
    (-> world
        (assoc-in [:background-meshes axis-name]
                  (create-cube-mesh p1 rotation
                                    [width (vector-length direction) width] color))
        (assoc-in [:background-meshes tip-name]
                  (create-cone-mesh p2 rotation [(/ tip 3) tip (/ tip 3)] color)))))

(defn create-axis-mesh [world size thickness]
  (-> world
      (create-vector-mesh [0 0 0] [size 0 0] :red :x-axis)
      (create-vector-mesh [0 0 0] [0 size 0] :green :y-axis)
      (create-vector-mesh [0 0 0] [0 0 size] :blue :z-axis)))

(defn draw-ortho-mesh! [world mesh]
  (let [num-vertices (/ (.capacity (:vertices-buffer mesh)) 3)
        program (get-in world [:programs :ortho])
        program-index (:index program)
        attributes (:attributes program)
        uniforms (:uniforms program)]

    (GL20/glUseProgram program-index)

    (GL20/glVertexAttribPointer (:position attributes) 3 GL11/GL_FLOAT
                                false 0 (:vertices-buffer mesh))
    (GL20/glEnableVertexAttribArray (:position attributes))

    (GL20/glVertexAttribPointer (:texture-coordinates attributes) 2 GL11/GL_FLOAT
                                false 0 (:texture-coordinates-buffer mesh))
    (GL20/glEnableVertexAttribArray (:texture-coordinates attributes))

    (GL13/glActiveTexture GL13/GL_TEXTURE0)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D (:texture-id mesh))
    (GL20/glUniform1i (:texture-diffuse uniforms) 0)

    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 num-vertices)))

(defn create-ortho-mesh []
  (let [image (new-image window-width window-height)
        vertices [-1 -1  0   1 -1  0   -1  1  0
                   1 -1  0   1  1  0   -1  1  0]
        texture-coordinates [0 1   1 1   0 0
                             1 1   1 0   0 0]
        texture-id (GL11/glGenTextures)
        mesh {:vertices-buffer (get-float-buffer vertices)
              :image image
              :texture-coordinates-buffer (get-float-buffer texture-coordinates)
              :texture-id texture-id}]
    (set-texture mesh)))

(defn create-wireframe-mesh [vertices position rotation scale color-name]
  (let [color (get-color color-name)
        r (/ (get-red color) 255)
        g (/ (get-green color) 255)
        b (/ (get-blue color) 255)]
    {:vertices-buffer (get-float-buffer vertices)
     :color [r g b 1.0]
     :transform (make-transform position rotation)
     :program :basic
     :scale scale
     :draw-fn draw-lines!}))

(defn transform->matrix [transform]
  (into [] (get-transform-matrix transform)))

(defn matrix->transform [matrix]
  (let [matrix4f (apply (fn [m00 m10 m20 m30
                             m01 m11 m21 m31
                             m02 m12 m22 m32
                             m03 m13 m23 m33]
                          (new Matrix4f
                               m00 m01 m02 m03
                               m10 m11 m12 m13
                               m20 m21 m22 m23
                               m30 m31 m32 m33))
                        matrix)]
    (new Transform matrix4f)))

(defn combine-transforms [a b]
  (let [ma (transform->matrix a)
        mb (transform->matrix b)
        m (multiply-matrices ma mb)]
    (matrix->transform m)))

(defn remove-transform [a b]
  (let [ma (transform->matrix a)
        mb (transform->matrix b)
        imb (get-inverse-matrix (into-array Float/TYPE mb))
        m (multiply-matrices ma imb)]
    (matrix->transform m)))

(defn translate-transform [transform displacement]
  (combine-transforms transform (make-transform displacement [1 0 0 0])))

(defn rotate-transform [transform axis-angle]
  (combine-transforms (make-transform [0 0 0] axis-angle) transform))

(defn apply-transform [transform point]
  (let [matrix (get-transform-matrix transform)
        vector (into-array Float/TYPE (conj (into [] point) 1))]
    (into [] (butlast (multiply-matrix-vector matrix vector)))))

(defn apply-rotation [transform point]
  (let [rotation (get-transform-rotation transform)
        rotation-transform (make-transform [0 0 0] rotation)]
    (apply-transform rotation-transform point)))

;;-------------------------------------------------------------------------------;;
;; output

(def output (atom ""))

(defn clear!
  ([]
   (let [mesh (get-in @world [:output])]
     (clear (:image mesh))
    (gl-thread (reset-texture mesh))))
  ([color]
   (let [mesh (get-in @world [:output])]
     (clear (:image mesh) color)
    (gl-thread (reset-texture mesh)))))

(defn draw-pixel! [color x y]
  (let [mesh (get-in @world [:output])]
    (draw-pixel (:image mesh) color x y)
    (gl-thread (reset-texture mesh))))

(defn fill-rect! [color x y w h]
  (let [mesh (get-in @world [:output])]
    (fill-rect (:image mesh) color x y w h)
    (gl-thread (reset-texture mesh))))

(defn draw-rect! [color x y w h]
  (let [mesh (get-in @world [:output])]
    (draw-rect (:image mesh) color x y w h)
    (gl-thread (reset-texture mesh))))

(defn fill-circle! [color x y r]
  (let [mesh (get-in @world [:output])]
    (fill-circle (:image mesh) color x y r)
    (gl-thread (reset-texture mesh))))

(defn draw-circle! [color x y r]
  (let [mesh (get-in @world [:output])]
    (draw-circle (:image mesh) color x y r)
    (gl-thread (reset-texture mesh))))

(defn draw-text! [color text x y size]
  (let [mesh (get-in @world [:output])]
    (draw-text (:image mesh) color text x y size)
    (gl-thread (reset-texture mesh))))

(defn draw-ellipse! [color rect]
  (let [mesh (get-in @world [:output])]
    (draw-ellipse (:image mesh) color rect)
    (gl-thread (reset-texture mesh))))

(defn draw-line! [color x1 y1 x2 y2]
  (let [mesh (get-in @world [:output])]
    (draw-line (:image mesh) color x1 y1 x2 y2)
    (gl-thread (reset-texture mesh))))

(defn fill-polygon! [color points]
  (let [mesh (get-in @world [:output])]
    (fill-polygon (:image mesh) color points)
    (gl-thread (reset-texture mesh))))

(defn draw-polygon! [color points]
  (let [mesh (get-in @world [:output])]
    (draw-polygon (:image mesh) color points)
    (gl-thread (reset-texture mesh))))

(defn draw-image! [image2 x y & corner]
  (let [mesh (get-in @world [:output])]
    (apply draw-image (:image mesh) image2 x y corner)
    (gl-thread (reset-texture mesh))
    nil))

(defn clear-top! []
  (let [mesh (get-in @world [:output])
        image (:image mesh)
        g (get-image-graphics image)
        w (get-image-width image)
        h (get-image-height image)]
    (.setComposite g (AlphaComposite/getInstance AlphaComposite/CLEAR))
    (.fillRect g 0 0 w (- h 100))
    (.setComposite g (AlphaComposite/getInstance AlphaComposite/SRC_OVER))
    (gl-thread (reset-texture mesh)))
  nil)

(defn draw-output! []
  (let [lines (split @output #"\n")
        last-lines (take-last 5 lines)
        hw (/ window-width 2)
        hh (/ window-height 2)]
    (fill-rect! :dark-gray hw (- window-height 50) window-width 100)
    (dotimes [i (count last-lines)]
      (draw-text! :green (nth last-lines i)
                  15 (+ (* i 15) (- window-height 80)) 14))))

(defn println! [& args]
  (apply gl-println args)
  (let [line (apply print-str (conj (into [] args) "\n"))
        truncated-output (apply str (take-last 1024 @output))]
    (swap! output (fn [output]
                    (str truncated-output line))))
  (draw-output!)
  nil)

(defn clear-output! []
  (reset! output "")
  (draw-output!)
  nil)

;;-------------------------------------------------------------------------------;;
;; camera

(defn compute-camera [world]
  (let [camera (:camera world)
        pivot (:pivot camera)
        eye (-> (:vector camera)
                (vector-multiply (:distance camera))
                (vector-rotate [1 0 0] (- (:x-angle camera)))
                (vector-rotate [0 1 0] (- (:y-angle camera)))
                (vector-add pivot))]
    (-> world
        (assoc-in [:view-matrix] (get-look-at-matrix eye pivot [0 1 0]))
        (assoc-in [:camera :eye] eye))))

(defn rotate-camera [world dx dy]
  (let [x-speed 0.4
        y-speed 0.4
        camera (-> (:camera world)
                   (update-in [:x-angle]
                              (fn [angle]
                                (within (+ angle (* dy y-speed)) -89 89)))
                   (update-in [:y-angle] (fn [angle] (+ angle (* dx y-speed)))))]
    (-> world
        (assoc-in [:camera] camera)
        (compute-camera))))

(declare unproject-point)

(defn pan-camera [world x1 y1 x2 y2]
  (let [l1 (unproject-point world [x1 y1])
        l2 (unproject-point world [x2 y2])
        plane [[0 0 0] [0 0 1] [1 0 0]]
        p1 (line-plane-intersection l1 plane)
        p2 (line-plane-intersection l2 plane)
        d (vector-subtract p1 p2)]
    (-> world
        (update-in [:camera :pivot] (fn [pivot]
                                      (vector-add pivot d)))
        (compute-camera))))

(defn create-camera [world vector distance x-angle y-angle]
  (-> world
    (assoc-in [:camera] {:vector vector
                         :distance distance
                         :x-angle x-angle
                         :y-angle y-angle
                         :pivot [0 0 0]})
    (compute-camera)))

(defn reset-camera! []
  (update-thing! [] (slots create-camera _ [0 0 1] 50 25 -45)))

(defn get-camera-plane [world point]
  (let [camera (:camera world)
        to-camera (vector-subtract (:eye camera) (:pivot camera))
        x-axis (vector-cross-product [0 1 0] to-camera)
        y-axis (vector-cross-product x-axis to-camera)
        p1 point
        p2 (vector-add point x-axis)
        p3 (vector-add point y-axis)]
    [p1 p2 p3]))

(defn zoom-camera [world amount]
  (-> world
      (update-in [:camera :distance] #(* % amount))
      (compute-camera)))

(defn get-ground-camera-point [world x y offset]
  (let [plane (:move-plane world)
        line (unproject-point world [x y])
        ground-plane [[0 offset 0] [1 offset 0] [0 offset 1]]
        p1 (line-plane-intersection line plane)
        p2 (line-plane-intersection line ground-plane)]
    (if (> (second p1) (second p2))
      p1
      p2)))

;;-------------------------------------------------------------------------------;;

(def redraw-flag (atom true))
(declare draw-2d!)
(declare draw-3d!)

(defn draw-world! [world]
  (try
    (draw-3d! world)
    (catch Exception e))

  (when @redraw-flag
    (clear-top!)
    (try
      (draw-2d! world)
      (catch Exception e))
    (reset! redraw-flag false))

  ;; (draw-close-snap-points! world)

  (GL11/glViewport 0 0 window-width window-height)
  (draw-ortho-mesh! world (:output world))
  )

(defn redraw [world]
  (reset! redraw-flag true)
  world)

(defn redraw! []
  (reset! redraw-flag true))

(defn -main [& args]
  (window-init!)
  (reset! out *out*))

(defn reset-world! []
  (gl-thread
   (do
     (create-world!)
     (redraw!))))

;;-------------------------------------------------------------------------------;;
;; debug shapes

(defn create-debug-meshes! []
  (set-thing! [:meshes :red-point]
              (create-cube-mesh [0 -1 0] [1 0 0 0] [0.1 0.1 0.1] :red))
  (set-thing! [:meshes :green-point]
              (create-cube-mesh [0.1 -1 0] [1 0 0 0] [0.1 0.1 0.1] :green))
  (set-thing! [:meshes :blue-point]
              (create-cube-mesh [0.2 -1 0] [1 0 0 0] [0.1 0.1 0.1] :blue))
  (set-thing! [:meshes :yellow-point]
              (create-cube-mesh [0.3 -1 0] [1 0 0 0] [0.1 0.1 0.1] :yellow))

  (set-thing! [:meshes :red-line]
              (create-line-mesh [0 0 0] [1 -1 1] :red))
  (set-thing! [:meshes :green-line]
              (create-line-mesh [0 0 0] [1.1 -1 1] :green))
  (set-thing! [:meshes :blue-line]
              (create-line-mesh [0 0 0] [1.2 -1 1] :blue))
  (set-thing! [:meshes :yellow-line]
              (create-line-mesh [0 0 0] [1.3 -1 1] :yellow)))

(defn set-point [world color position]
  (let [name (keyword (subs (str color "-point") 1))]
    (update-in world [:meshes name] #(set-mesh-position % position))))

(defn set-line [world color a b]
  (let [name (keyword (subs (str color "-line") 1))
        vertices (into [] (concat a b))]              
    (assoc-in world [:meshes name :vertices-buffer]
              (get-float-buffer vertices))))

(defn set-point! [color position]
  (update-thing! [] #(set-point % color position)))

(defn set-line! [color a b]
  (update-thing! [] #(set-line % color a b)))

(defn hide-debug-shapes! []
  (set-point! :red [0 -1 0])
  (set-point! :green [0 -1 0])
  (set-point! :blue [0 -1 0])
  (set-point! :yellow [0 -1 0])
  
  (set-line! :red [0 0 0] [0 -1 0])
  (set-line! :green [0 0 0] [0 -1 0])
  (set-line! :blue [0 0 0] [0 -1 0])
  (set-line! :yellow [0 0 0] [0 -1 0]))

;;-------------------------------------------------------------------------------;;
;; camera manipulation

(defn mouse-rotate [world event]
  (let [[x y] (:last-point world)
        dx (- (:x event) x)
        dy (- (:y event) y)]
    (-> world
        (rotate-camera dx dy)
        (assoc-in [:last-point] [(:x event) (:y event)]))))

(defn mouse-pan [world event]
  (let [[x1 y1] (:last-point world)
        x2 (:x event)
        y2 (:y event)]
    (-> world
        (pan-camera x1 y1 x2 y2)
        (assoc-in [:last-point] [x2 y2]))))

(defn mouse-scrolled [world event]
  (let [amount (+ 1 (* (:y event) -0.05))]
    (zoom-camera world amount)))

;;-------------------------------------------------------------------------------;;
;; begin

;;-------------------------------------------------------------------------------;;
;; debug

(defn get-part-with-color [world color]
  (first (find-if (fn [[name part]]
                    (= (:color part) color))
                  (:parts world))))

(defn create-graph [world part-name]
  (if (nil? (get-in world [:parts part-name]))
    world
    (assoc-in world [:wave-editor :functions part-name] [[0 0] [1 0]])))

(defn create-graph! [color]
  (let [part-name (get-part-with-color @world color)]
    (update-thing! [] #(create-graph % part-name))
    (sleep 100)
    (reset! redraw-flag true)))

(defn get-tail-transform [track]
  (let [track-transform (:transform track)
        y-offset (* -0.5 (second (:scale track)))]
    (combine-transforms
     (make-transform [0 y-offset 0] [1 0 0 0])
     track-transform)))

(defn inserting? [world]
  (or (get-in world [:keys :b :active])
      (get-in world [:keys :t :active])))

(defn draw-track-cap? [world track]
  (let [non-wagon-child (find-if (fn [[child-name _]]
                                   (let [child (get-in world [:parts child-name])]
                                     (nil? (:loop-fn child))))
                                 (:children track))]
    (or (inserting? world)
        (not-nil? non-wagon-child)
        false ;; if there are extra connections #######################
        )))

(defn draw-track! [world track]
  (let [info (get-in world [:info :track])
        tail (-> (:tail info)
                 (assoc-in [:transform] (get-tail-transform track))
                 (assoc-in [:scale] (:scale track))
                 (set-mesh-color (:color track)))]
    (draw-mesh! world tail)

    (if (draw-track-cap? world track)
      (let [track-transform (:transform track)
            model (if (inserting? world)
                    (:model-big info)
                    (:model info))
            head (-> model
                     (assoc-in [:transform] track-transform)
                     (set-mesh-color (:color track)))]
        (draw-mesh! world head)))))

(defn draw-part! [world part]
  (if (= (:type part) :track)
    (draw-track! world part)
    (let [info (get-in world [:info (:type part)])
          transform (:transform part)
          mesh (-> (:model info)
                   (assoc-in [:transform] transform)
                   (assoc-in [:scale] (:scale part))
                   (set-mesh-color (:color part)))]
      (draw-mesh! world mesh))))

;;-------------------------------------------------------------------------------;;
;; parts

(defn get-part-position [world name]
  (let [transform (get-in world [:parts name :transform])]
    (get-transform-position transform)))

(defn get-parts-with-type [parts type]
  (filter (fn [part]
            (= (get-in parts [part :type]) type))
          (keys parts)))

(defn get-part-type [world name]
  (get-in world [:parts name :type]))

(defn create-info []
  {:block {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [0.12 0.12 0.12] :white) ;;; ##### 0.12??
           :points [[0.5 0 0] [-0.5 0 0]
                    [0 0.5 0]
                    [0 0 0.5] [0 0 -0.5]]
           :offset 0.25
           }

   :track {:model (create-sphere-mesh [0 0 0] [1 0 0 0]
                                      [0.1 0.1 0.1] :white)

           :model-big (create-sphere-mesh [0 0 0] [1 0 0 0]
                                          [0.12 0.12 0.12] :white)
           
           :tail (create-cylinder-mesh [0 0 0] [1 0 0 0]
                                       [1 1 1] :white)

           :collision-box (create-cube-mesh [0 0 0] [1 0 0 0]
                                            [0.5 0.5 0.5] :white)
           
           :points [[0.12 0 0] [-0.12 0 0]
                    [0 0.12 0]
                    [0 0 0.12] [0 0 -0.12]
                    ]
           :offset 1
           }
   })

(defn create-part [type color]
  (let [scale (if (= type :block)
                [0.5 0.5 0.5]
                [0.1 1 0.1])]
    {:type type
     :color color
     :transform (make-transform [0 0.5 0] [0 1 0 0])
     :value 0.0
     :scale scale}))

(defn get-parent-part [world child-name]
  (if (in? child-name (keys (:ground-children world)))
    :ground
    (find-if (fn [name]
               (let [parent (get-in world [:parts name])]
                 (in? child-name (keys (:children parent)))))
             (keys (:parts world)))))

(declare get-part-at)

(defn set-pivot [world x y]
  (let [pos (if-let [part-name (get-part-at world x y)]
              (get-part-position world part-name)
              (let [line (unproject-point world [x y])
                    ground-plane [[0 0 0] [1 0 0] [0 0 1]]]
                (line-plane-intersection line ground-plane)))]
    (compute-camera (assoc-in world [:camera :pivot] pos))))

(defn make-spec [position rotation point]
  (let [rotation-transform (make-transform [0 0 0] rotation)
        transform (make-transform position rotation)
        p (apply-transform transform point)
        normal-table {[ 1  0  0] [0 0 1 -90]
                      [-1  0  0] [0 0 1 90]
                      [ 0  1  0] [1 0 0 0]
                      [ 0 -1  0] [1 0 0 180]
                      [ 0  0  1] [1 0 0 90]
                      [ 0  0 -1] [1 0 0 -90]}
        extra-rotation (get normal-table (map round (vector-normalize point)))
        final-rotation (get-transform-rotation
                        (combine-transforms
                         (make-transform [0 0 0] extra-rotation)
                         (make-transform [0 0 0] rotation)))]
    [p final-rotation]))

(defn get-snap-specs [world]
  (let [;; grid-specs (vec (map (fn [[a b]]
        ;;                        {:position [(- a 5.5) 0 (- b 5.5)]
        ;;                         :rotation [1 0 0 0]
        ;;                         :local [(- a 5.5) 0 (- b 5.5)]
        ;;                         :part :ground})
        ;;                      (create-combinations (range 12) (range 12))))
        grid-specs [{:position [0.25 0 0.25]
                     :rotation [1 0 0 0]
                     :part :ground}
                    ]
        face-specs
        (vec
         (remove-nil
          (mapcat (fn [[name part]]
                    (let [part (get-in world [:parts name])
                          transform (:transform part)
                          position (get-transform-position transform)
                          rotation (get-transform-rotation transform)
                          points (get-in world [:info (:type part) :points])
                          [sa sb sc] (if (= (:type part) :track)
                                       [1 1 1]
                                       (:scale part))
                          points (map (fn [[a b c]]
                                        [(* sa a) (* sb b) (* sc c)])
                                      points)]
                      (map (fn [p]
                             (let [[pos rot] (make-spec position rotation p)]
                               {:position pos
                                :rotation rot
                                :part name}))
                           points)))
                  (:parts world))))]
    (vec (concat grid-specs face-specs))))

(defn get-closest-snap-point [world x y snap-specs]
  (let [line (unproject-point world [x y])
        close-points (filter (fn [spec]
                               (< (point-line-distance (:position spec) line) 0.2))
                             snap-specs)
        eye (get-in world [:camera :eye])]
    (first (sort-by (fn [spec]
                      (distance (:position spec) eye)) close-points))))

;;-------------------------------------------------------------------------------;;
;; collision

(defn unproject-point [world [x y]]
  (let [dx (dec (/ x (/ window-width 2)))
        dy (- (dec (/ y (/ window-height 2))))
        p-matrix (:projection-matrix world)
        v-matrix (:view-matrix world)
        matrix (multiply-matrices v-matrix p-matrix)
        inverse-matrix (get-inverse-matrix matrix)
        p-2d-a (into-array Float/TYPE [dx dy -1.0 1.0])
        p-3d-a (into [] (multiply-matrix-vector inverse-matrix p-2d-a))
        p-3d-a (map (slots / _ (nth p-3d-a 3)) p-3d-a)
        p-3d-a (into [] (butlast p-3d-a))

        p-2d-b (into-array Float/TYPE [dx dy 0.0 1.0])
        p-3d-b (into [] (multiply-matrix-vector inverse-matrix p-2d-b))
        p-3d-b (map (slots / _ (nth p-3d-b 3)) p-3d-b)
        p-3d-b (into [] (butlast p-3d-b))]
    [p-3d-a (vector-normalize (vector-subtract p-3d-b p-3d-a))]))

(defn distance-comparator [a b]
  (cond
    (nil? a) false
    (nil? b) true
    (and (nil? a) (nil? b)) a
    :else (< a b)))

(defn get-mesh-triangles [mesh transform scale]
  (let [vertices (partition 3 (into [] (:vertices mesh)))
        matrix (multiply-matrices
                (apply get-scale-matrix scale)
                (get-transform-matrix transform))
        vertices (map (fn [[x y z]]
                        (let [vertex (into-array Float/TYPE [x y z 1])]
                          (butlast (into [] (multiply-matrix-vector
                                             matrix vertex)))))
                      vertices)]
    (partition 3 vertices)))

(defn get-mesh-collision [mesh transform scale line]
  (let [triangles (get-mesh-triangles mesh transform scale)
        measured-triangles (map (fn [i]
                                  {:d (line-triangle-distance
                                       line (nth triangles i))
                                   :i i})
                                (range (count triangles)))
        collision (first (sort-by :d distance-comparator measured-triangles))]
    (if (nil? (:d collision))
      nil
      [(:i collision) (:d collision) (line-get-point line (:d collision))])))

(defn get-block-collision [block info line]
  (let [mesh (:model info)
        transform (:transform block)
        scale (:scale block)]
    (get-mesh-collision mesh transform scale line)))

(defn get-track-collision [track info line]
  (let [mesh (:collision-box info)
        transform (get-tail-transform track)
        [sx sy sz] (:scale track)
        scale [(* 1.6 sx) sy (* 1.6 sz)]]
    (get-mesh-collision mesh transform scale line)))

(defn get-part-collision [world px py]
  (let [line (unproject-point world [px py])
        distances (map (fn [[name part]]
                         (let [type (:type part)
                               info (get-in world [:info type])
                               [i d p] (if (= type :block)
                                         (get-block-collision part info line)
                                         (get-track-collision part info line))]
                           (if (nil? d)
                             nil
                             {:part-name name
                              :distance d
                              :point p
                              :index i})))
                       (:parts world))]
    (first (sort-by :distance (remove-nil distances)))))

(defn get-part-at [world px py]
  (:part-name (get-part-collision world px py)))

;;-------------------------------------------------------------------------------;;
;; track loop

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

(defn compute-loop-function [loop]
  (let [lengths (map (fn [a b]
                       (vector-length (vector-subtract a b)))
                     loop (rest loop))
        total-length (reduce + lengths)
        ts (map #(/ % total-length) (accumulate lengths))]
    (map vector ts loop)))

(defn set-wagon-loop [world wagon-name track-name]
  (assoc-in world [:parts wagon-name :loop-fn]
            (compute-loop-function (get-track-loop world track-name))))

;;-------------------------------------------------------------------------------;;
;; mechanical tree

(declare compute-subtree-transforms)
(declare get-function-value)

(defn compute-children-transforms [world part-name transform key]
  (reduce (fn [w [child-name relative-transform]]
            (let [new-transform (combine-transforms
                                 relative-transform transform)]
              (compute-subtree-transforms w child-name new-transform key)))
          world
          (get-in world [key part-name :children])))

(defn block-compute-subtree-transforms [world name transform key]
  (let [block (get-in world [:parts name])]
    (if (nil? (:loop-fn block))
      (-> world
          (assoc-in [key name :transform] transform)
          (compute-children-transforms name transform key))
      (let [rotation (get-transform-rotation transform)
            loop-fn (map (fn [[t v]]
                           [t (apply-transform transform v)])
                         (:loop-fn block))
            value (within (:value block) 0.0 1.0)
            position (get-function-value loop-fn value vector-interpolate)
            transform (make-transform position rotation)]
        (-> world
            (assoc-in [key name :transform] transform)
            (compute-children-transforms name transform key))))))

(defn track-compute-subtree-transforms [world name transform key]
  (let [track (get-in world [:parts name])
        angle (map-between-ranges (:value track) 0.0 1.0 0.0 360.0)
        angle-transform (make-transform [0 0 0] [0 1 0 angle])
        transform (combine-transforms angle-transform transform)
        world (assoc-in world [key name :transform] transform)]
    (reduce (fn [w [child-name relative-transform]]
              (let [new-transform (combine-transforms
                                   relative-transform transform)]
                (compute-subtree-transforms w child-name new-transform key)))
            world
            (get-in world [key name :children]))))  

(defn compute-subtree-transforms [world name transform key]
  (case (get-in world [:parts name :type])
    :block (block-compute-subtree-transforms world name transform key)
    :track (track-compute-subtree-transforms world name transform key)
    world))

(defn compute-transforms [world key] 
  (reduce (fn [w [name relative-transform]]
            (compute-subtree-transforms w name relative-transform key))
          world
          (:ground-children world)))

(defn detach-child [world child-name]
  (let [parent-name (get-parent-part world child-name)]
    (cond
      (= parent-name :ground)
      (dissoc-in world [:ground-children child-name])

      (nil? parent-name) world
      
      :else
      (dissoc-in world [:parts parent-name :children child-name]))))

(defn create-relative-transform [world child-name parent-name]
  (let [child (get-in world [:parts child-name])]
    (if (= parent-name :ground)
      (assoc-in world [:ground-children child-name] (:transform child))
      (let [parent (get-in world [:parts parent-name])
            child-transform (:transform child)
            parent-transform (:transform parent)
            final-transform (remove-transform child-transform parent-transform)]
        (assoc-in world [:parts parent-name :children child-name]
                  final-transform)))))

;;-------------------------------------------------------------------------------;;
;; wave editor

(defn add-graph [world x y]
  (if-let [part-name (get-part-at world x y)]
    (assoc-in world [:wave-editor :functions part-name] [[0 0] [1 0]])
    world))

(defn global->editor-coordinates [wave-editor [t v]]
  (let [{:keys [x y w h]} wave-editor
        hw (/ w 2)
        hh (/ h 2)
        x1 (- x hw)
        x2 (+ x hw)
        y1 (- y hh)
        y2 (+ y hh)]
    [(map-between-ranges t 0 1 (+ x1 7) (- x2 7))
     (map-between-ranges v 0 1 (- y2 7) (+ y1 7))]))

(defn editor-coordinates->global [wave-editor [px py]]
  (let [{:keys [x y w h]} wave-editor
        hw (/ w 2)
        hh (/ h 2)
        x1 (- x hw)
        x2 (+ x hw)
        y1 (- y hh)
        y2 (+ y hh)]
    [(within (map-between-ranges px (+ x1 7) (- x2 7) 0 1) 0 1)
     (within (map-between-ranges py (- y2 7) (+ y1 7) 0 1) 0 1)]))

(defn get-function-point-at [wave-editor t v]
  (let [named-points (mapcat (fn [[name points]]
                               (map (fn [point index]
                                      [name index point])
                                    points (range (count points))))
                             (:functions wave-editor))
        named-point (find-if (fn [[name index point]]
                               (< (distance point [t v]) 0.05))
                             named-points)]
    (if (nil? named-point)
      nil
      (vec (take 2 named-point)))))

(defn point-between-points? [p p1 p2]
  (let [v (vector-subtract p2 p1)
        line [p1 v]
        l (vector-length v)]
    (and
     (< (point-line-distance p line) 0.04)
     (< (distance p p1) l)
     (< (distance p p2) l))))

(defn add-function-point [wave-editor t v]
  (let [functions (:functions wave-editor)
        named-points (mapcat (fn [[name points]]
                               (map (fn [point index]
                                      [name index point])
                                    (butlast points) (range (count points))))
                             functions)
        [name index _] (find-if (fn [[name index point-before]]
                                  (let [function (get-in functions [name])
                                        point-after (nth function (inc index))]
                                    (point-between-points? [t v]
                                                           point-before
                                                           point-after)))
                                named-points)]

    (if (not-nil? name)
      (update-in wave-editor [:functions name]
                 (fn [function]
                   (vector-insert function [t v] (inc index))))
      wave-editor)))

(defn remove-function-point [wave-editor point]
  (if-let [[name index] point]
    (let [function (get-in wave-editor [:functions name])]
      (if (< 0 index (dec (count function)))
        (update-in wave-editor [:functions name] #(vector-remove % index))
        wave-editor))
    wave-editor))

(defn get-function-value [function t interpolator]
  (let [pairs (map vector function (rest function))
        pair (find-if (fn [[[t0 & _] [t1 & _]]]
                        (<= t0 t t1))
                      pairs)
        t0 (first (first pair))
        t1 (first (second pair))
        s (map-between-ranges t t0 t1 0 1)
        v0 (second (first pair))
        v1 (second (second pair))]
    (interpolator v0 v1 s)))

(defn run-wave [world name]
  (let [function (get-in world [:wave-editor :functions name])
        part (get-in world [:parts name])
        linear-interpolator (fn [a b t]
                              (+ (* a (- 1.0 t)) (* b t)))
        new-value (float (get-function-value
                          function (:time world) linear-interpolator))]
    (assoc-in world [:parts name :value] new-value)))

(defn run-waves [world]
  (reduce (fn [w name]
            (run-wave w name))
          world
          (keys (get-in world [:wave-editor :functions]))))

(defn get-function-at [wave-editor t v]
  ;;#########################################################
  :track4741
  )

(defn editor-mouse-pressed [world event]
  (let [x (:x event)
        y (:y event)
        [t v] (editor-coordinates->global (:wave-editor world) [x y])
        moving-point (get-function-point-at (:wave-editor world) t v)]
    (if (= (:button event) :right)
      (update-in world [:wave-editor] #(remove-function-point % moving-point))
      (let [elapsed (- (get-current-time) (:last-time world))
            world (assoc-in world [:last-time] (get-current-time))]
        (if (< elapsed 200)
          (update-in world [:wave-editor] #(add-function-point % t v))
          (if (nil? moving-point)
            (if-let [function-name (get-function-at (:wave-editor world) t v)]
              (let [function (get-in world [:wave-editor
                                            :functions function-name])]
                ;;############################################################
                (println! function)
                world
                )
              world)
            (-> world
                (assoc-in [:moving-function-point] moving-point)
                (assoc-in [:editor-last-point] [x y]))))))))

(defn editor-mouse-moved [world event]
  (reset! redraw-flag true)
  (let [x (:x event)
        y (:y event)
        coords (editor-coordinates->global (:wave-editor world) [x y])]
    (if-let [[function-name index] (:moving-function-point world)]
      (let [function (get-in world [:wave-editor :functions function-name])
            coords (cond
                     (= index 0)
                     (assoc-in coords [0] 0.0)

                     (= index (dec (count function)))
                     (assoc-in coords [0] 1.0)

                     :else
                     coords)]
        (assoc-in world [:wave-editor :functions function-name index] coords))
      ;; (-> world
      ;;     (assoc-in [:time] (first coords))
      ;;     (run-waves)
      ;;     (compute-transforms))
      world
      )))

(defn editor-mouse-released [world event]
  (-> world
      (dissoc-in [:editor-last-point])
      (dissoc-in [:moving-function-point])))

(defn draw-function! [wave-editor function-name color]
  (let [{:keys [x y w h]} wave-editor
        function (get-in wave-editor [:functions function-name])]
    (doseq [i (range 1 (count function))]
      (let [[x1 y1] (global->editor-coordinates
                     wave-editor (nth function (dec i)))
            [x2 y2] (global->editor-coordinates
                     wave-editor (nth function i))]
        (draw-line! color x1 y1 x2 y2)
        (fill-circle! color x2 y2 7)

        (if (= i 1)
          (fill-circle! color x1 y1 7))))))

(defn draw-wave-editor! [world]
  (let [wave-editor (:wave-editor world)
        {:keys [x y w h]} wave-editor]
    (fill-rect! :black x y w h)
    (draw-rect! :dark-gray x y (- w 14) (- h 14))

    (doseq [function-name (keys (get-in wave-editor [:functions]))]
      (let [color (get-in world [:parts function-name :color])]
        (draw-function! (:wave-editor world) function-name color)))

    (let [hw (/ w 2)
          hh (/ h 2)
          x1 (- x hw -7)
          x2 (+ x hw -7)
          y1 (- y hh -7)
          y2 (+ y hh -7)

          lx (map-between-ranges (:time world) 0 1 x1 x2)]
      (draw-line! :purple lx y1 lx y2)
      )))

;;-------------------------------------------------------------------------------;;
;; menus

(defn create-menu [filename x y w h]
  (let [document (read-xml filename)
        image (if (= w -1)
                (parse-svg-from-map-with-height document h)
                (parse-svg-from-map-with-width document w))
        menu {:x x
              :y y
              :w (get-image-width image)
              :h (get-image-height image)
              :image image}
        regions (get-absolute-svg-regions document menu)]
    (assoc-in menu [:regions] regions)))


(defn get-button-at [menus x y]
  (let [buttons (mapcat (fn [[name menu]]
                          (map (fn [region]
                                 [name (first region) (second region)])
                               (:regions menu)))
                        menus)
        button (find-if (fn [[menu-name button-name box]]
                          (inside-box? box x y))
                        buttons)]
    (if (nil? button)
      nil
      (vec (take 2 button)))))

(defn menu-pressed [world event]
  (let [{:keys [x y]} event
        [menu-name button-name] (get-button-at (:menus world) x y)]
    (cond
      (= menu-name :colors)
      (assoc-in world [:current-color] button-name)

      (= menu-name :actions)
      (cond
        (= button-name :playpause)
        (update-in world [:paused] not)

        (= button-name :delete) ;;########################################
        (let [world (update-in world [:show-editor] not)]
          (reset! redraw-flag true)
          world)

        :else world)

      :else world)))

(defn get-current-button [world]
  (case (:mode world)
    :insert [:tools (:mode-part world)]
    :cable [:tools :cable]
    :color [:colors (:mode-color world)]
    :move [:actions :move]
    :adjust [:actions :adjust]
    :delete [:actions :delete]))

;;-------------------------------------------------------------------------------;;
;; color mode

(declare get-cable-at)

(defn set-color [world x y]
  (if-let [part-name (get-part-at world x y)]
    (assoc-in world [:parts part-name :color] (:current-color world))
    world))

;;-------------------------------------------------------------------------------;;
;; insert mode

(defn spec->transform [offset spec parent]
  (let [final-rotation (:rotation spec)
        rotation-transform (make-transform [0 0 0] final-rotation)
        offset [0 offset 0]
        offset (apply-transform rotation-transform offset)
        final-point (if (or (nil? parent)
                            (= (:type parent) :block))
                      (:position spec)
                      (get-transform-position (:transform parent)))
        final-point (vector-add final-point offset)]
    (make-transform final-point final-rotation)))

(defn insert-part [world type color x y]
  (if-let [spec (get-closest-snap-point world x y (:snap-specs world))]
    (let [part (create-part type color)
          name (gen-keyword type)
          offset (get-in world [:info (:type part) :offset])
          parent-name (:part spec)
          parent (get-in world [:parts parent-name])
          transform (spec->transform offset spec parent)]
      (-> world
          (assoc-in [:parts name] part)
          (assoc-in [:parts name :transform] transform)
          (create-relative-transform name parent-name)))
    world))

(defn insert-wagon [world color x y]
  (let [part-name (get-part-at world x y)]
    (if (and (not-nil? part-name)
             (= (get-in world [:parts part-name :type]) :track))
      (let [part (create-part :block color)
            name (gen-keyword :wagon)
            transform (get-in world [:parts part-name :transform])]
        (-> world
            (assoc-in [:parts name] part)
            (assoc-in [:parts name :transform] transform)
            (assoc-in [:parts part-name :children name]
                      (make-transform [0 0 0] [1 0 0 0]))
            (set-wagon-loop name part-name)
            (compute-transforms :parts)))
      world)))

;;-------------------------------------------------------------------------------;;
;; delete mode

(defn mesh-to-from [mesh start end]
  (let [v (vector-subtract end start)
        length (vector-length v)]
    (if (float-equals? length 0.0)
      (set-mesh-position mesh [-1000 0 0])
      (let [position (vector-multiply (vector-add end start) 0.5)
            rotation [1 0 0 0]]
        (-> mesh
            (assoc-in [:transform] (make-transform position rotation))
            (point-mesh-towards v)
            (assoc-in [:scale 1] length))))))

(defn actualize-points [world points]
  (map (fn [p]
         (if (map? p)
           (let [{:keys [local part]} p]
             (if (= part :ground)
               local
               (let [part (get-in world [:parts part])
                     transform (:transform part)]
                 (apply-transform transform local))))
           p))
       points))

(defn segment-collision? [mesh start end line]
  (let [mesh (mesh-to-from mesh start end)
        transform (:transform mesh)]
    (get-mesh-collision mesh transform line)))
                      
(defn cable-collision? [world cable line]
  (let [points (actualize-points world (:points cable))]
    (some (fn [[a b]]
            (segment-collision? (:bounding-box world)
                                a b line))
          (map vector points (rest points)))))

(defn get-cable-at [world x y]
  (let [line (unproject-point world [x y])]
    (first (find-if (fn [[name cable]]
                      (cable-collision? world cable line))
                    (:cables world)))))

(defn delete-part [world x y]
  (if-let [part-name (get-part-at world x y)]
    (-> world
        (detach-child part-name)
        (dissoc-in [:wave-editor :functions part-name])
        (dissoc-in [:parts part-name]))
    (if-let [cable-name (get-cable-at world x y)]
      (dissoc-in world [:cables cable-name])
      world)))

;;-------------------------------------------------------------------------------;;
;; scale mode

(defn set-value-0-transform [world part-name]
  (let [parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        relative-transform (get-in parent [:children part-name])
        transform (combine-transforms relative-transform parent-transform)]
    (assoc-in world [:parts part-name :transform] transform)))

(defn set-block-size [world block-name original-scale
                      original-center increase]
  (let [block (get-in world [:parts block-name])
        new-scale (map (fn [a b]
                     (if (zero? b)
                       a
                       (abs b)))
                       original-scale increase)
        scale-change (vector-subtract new-scale original-scale)
        value (if (some neg? increase)
                -0.5
                0.5)
        part-rotation (get-transform-rotation (:transform block))
        rotation-transform (make-transform [0 0 0] part-rotation)
        offset (apply-transform rotation-transform
                                (vector-multiply scale-change value))
        new-center (vector-add original-center offset)        
        new-transform (make-transform new-center part-rotation)]
    (-> world
        (assoc-in [:parts block-name :scale] (map abs new-scale))
        (assoc-in [:parts block-name :transform] new-transform))))

(do
1

(defn scale-block-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            vertices (get-in world [:info :block :model :vertices])
            triangles (partition 3 (partition 3 vertices))
            [a b c] (nth triangles index)
            v1 (vector-subtract b a)
            v2 (vector-subtract c a)
            normal (vector-cross-product v1 v2)
            rotation-transform (get-rotation-component (:transform part))
            v (apply-transform rotation-transform normal)
            scale (:scale part)
            center (get-transform-position (:transform part))]
        (-> world
            (assoc-in [:adjusted-block] part-name)
            (assoc-in [:adjust-line] [point (vector-normalize v)])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)
            (assoc-in [:normal] normal)))
      world)))

(defn scale-block-moved [world event]
  (if-let [block-name (:adjusted-block world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          grain-size 0.2
          d (* grain-size (round (/ d grain-size)))
          scale (:original-scale world)
          center (:original-center world)
          normal (:normal world)
          l (within (+ d (abs (reduce + (map * normal scale)))) 0.1 10)
          increase-vector (map * (:normal world) [l l l])]
      (println! "scale:" (round (/ l grain-size)))
      (-> world
          (set-block-size block-name scale center increase-vector)
          (assoc-in [:increase-vector] increase-vector)))
    world))

(defn scale-block-released [world event]
  (if-let [block-name (:adjusted-block world)]
    (let [parent-name (get-parent-part world block-name)
          scale (:original-scale world)
          increase-vector (:increase-vector world)
          world (-> world
                    (assoc-in [:parts block-name :scale] scale)
                    (set-value-0-transform block-name))
          center (get-part-position world block-name)]
      (-> world
          (set-block-size block-name scale center increase-vector)
          (create-relative-transform block-name parent-name)
          (dissoc-in [:adjusted-block])))
    world))
)


(defn set-track-size [world track-name original-scale original-center height]
  (let [track (get-in world [:parts track-name])
        new-scale (assoc original-scale 1 height)
        scale-change (vector-subtract new-scale original-scale)
        part-rotation (get-transform-rotation (:transform track))
        rotation-transform (make-transform [0 0 0] part-rotation)
        new-center (->> scale-change
                        (apply-transform rotation-transform)
                        (vector-add original-center))
        new-transform (make-transform new-center part-rotation)]
    (-> world
        (assoc-in [:parts track-name :scale] new-scale)
        (assoc-in [:parts track-name :transform] new-transform))))

(defn scale-track-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point _]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            scale (:scale part)
            transform (:transform part)
            center (get-transform-position transform)
            rotation-transform (get-rotation-component transform)
            v (apply-transform rotation-transform [0 1 0])]
        (-> world
            (assoc-in [:adjusted-track] part-name)
            (assoc-in [:adjust-line] [point (vector-normalize v)])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)))
      world)))

(defn scale-track-moved [world event]
  (if-let [track-name (:adjusted-track world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          grain-size 0.1
          d (* grain-size (round (/ d grain-size)))
          scale (:original-scale world)
          center (:original-center world)
          normal (second adjust-line)
          l (within (+ (apply max scale) d) grain-size 10)]
      (println! "scale:" (round (/ l grain-size)))
      (-> world
          (set-track-size track-name scale center l)
          (assoc-in [:track-length] l)
          ))
      world))

(defn scale-track-released [world event]
  (if-let [track-name (:adjusted-track world)]
    (let [parent-name (get-parent-part world track-name)
          scale (:original-scale world)
          world (-> world
                    (assoc-in [:parts track-name :scale] scale)
                    (set-value-0-transform track-name))
          center (get-part-position world track-name)
          track-length (:track-length world)]
      (-> world
          (set-track-size track-name scale center track-length)
          (create-relative-transform track-name parent-name)
          (dissoc-in [:adjusted-track])))
    world))

(defn scale-mode-pressed [world event]
  (if-let [part-name (get-part-at world (:x event) (:y event))]
    (let [type (get-in world [:parts part-name :type])
          world (assoc-in world [:scale-type] type)]
      (case type
        :block (scale-block-pressed world event)
        :track (scale-track-pressed world event)
        world))
    world))

(defn scale-mode-moved [world event]
  (case (:scale-type world)
    :block (scale-block-moved world event)
    :track (scale-track-moved world event)
    world))

(defn scale-mode-released [world event]
  (let [world (case (:scale-type world)
                :block (scale-block-released world event)
                :track (scale-track-released world event)
                world)]
    (dissoc-in world [:scale-type])))

;;-------------------------------------------------------------------------------;;
;; move mode

(defn move-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [w (set-value-0-transform world part-name)
            part (get-in w [:parts part-name])
            v1 (apply-rotation (:transform part) [1 0 0])
            v2 (apply-rotation (:transform part) [0 0 1])
            plane [point (vector-add point v1) (vector-add point v2)]
            part-position (get-part-position world part-name)
            offset (vector-subtract part-position point)]
        (-> world
            (assoc-in [:moving-part] part-name)
            (assoc-in [:plane] plane)
            (assoc-in [:offset] offset)
            (assoc-in [:original-position] part-position)))
      world)))

(defn move-mode-moved [world event]
  (if-let [part-name (:moving-part world)]
    (let [line (unproject-point world [(:x event) (:y event)])
          touch-point (line-plane-intersection line (:plane world))
          position (vector-add touch-point (:offset world))
          [a b c] (:plane world)
          v1 (vector-subtract b a)
          v2 (vector-subtract c a)
          origin (:original-position world)
          s (point-line-coordinate position [origin v1])
          t (point-line-coordinate position [origin v2])
          grain-size 0.1
          s (* grain-size (round (/ s grain-size)))
          t (* grain-size (round (/ t grain-size)))
          snapped-position (reduce vector-add [origin
                                               (vector-multiply v1 s)
                                               (vector-multiply v2 t)])
          part (get-in world [:parts part-name])
          rotation (get-transform-rotation (:transform part))
          transform (make-transform snapped-position rotation)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (assoc-in [:snapped-position] snapped-position)))          
    world))

(defn move-mode-released [world event]
  (if-let [part-name (:moving-part world)]
    (let [parent-name (get-parent-part world part-name)
          world (set-value-0-transform world part-name)
          part (get-in world [:parts part-name])
          rotation (get-transform-rotation (:transform part))
          snapped-position (:snapped-position world)
          transform (make-transform snapped-position rotation)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (create-relative-transform part-name parent-name)
          (dissoc-in [:moving-part])))
    world))

;;-------------------------------------------------------------------------------;;
;; actions

(defn get-key-name [keys code]
  (first (find-if (fn [[name info]]
                    (= (:code info) code))
                  keys)))

(defn key-pressed [world event]
  (if-let [key (get-key-name (:keys world) (:code event))]
    (let [world (assoc-in world [:keys key :active] true)]
      (if (in? key [:b :t])
        (assoc-in world [:show-cursor] true)
        world))
    world))

(defn key-released [world event]
  (if-let [key (get-key-name (:keys world) (:code event))]
    (let [world (assoc-in world [:keys key :active] false)]
      (if (in? key [:b :t])
        (assoc-in world [:show-cursor] false)
        world))
    world))

(defn action-mouse-pressed [world event]
  (let [keys (:keys world)
        x (:x event)
        y (:y event)]
    (cond
      (get-in keys [:b :active])
      (insert-part world :block :white x y)

      (get-in keys [:t :active])
      (insert-part world :track :red x y)

      (get-in keys [:w :active])
      (insert-wagon world :yellow x y)

      (get-in keys [:period :active])
      (set-pivot world x y)

      (get-in keys [:s :active])
      (scale-mode-pressed world event)

      (get-in keys [:c :active])
      (set-color world x y)

      (get-in keys [:d :active])
      (delete-part world x y)

      (get-in keys [:g :active])
      (add-graph world x y)

      (get-in keys [:m :active])
      (move-mode-pressed world event)

      :else
      world)))

(defn action-mouse-moved [world event]
  (let [keys (:keys world)
        x (:x event)
        y (:y event)]
    (cond
      (get-in keys [:s :active])
      (scale-mode-moved world event)

      (get-in keys [:m :active])
      (move-mode-moved world event)

      :else
      world)))

(defn action-mouse-released [world event]
  (let [keys (:keys world)
        x (:x event)
        y (:y event) world (cond
                             (get-in keys [:s :active])
                             (scale-mode-released world event)

                             (get-in keys [:m :active])
                             (move-mode-released world event)

                             :else
                             world)]
    (compute-transforms world :parts)))

;;-------------------------------------------------------------------------------;;
;; miscellaneous

(defn create-sphere [world position]
  (let [body (create-sphere-body
              (:sphere-radius world) 1.0
              (make-transform position [1 0 0 0]))]
    (add-body-to-planet (:planet world) body)
    (update-in world [:spheres] (partial cons body))))

(defn draw-spheres! [world]
  (let [mesh (:sphere-mesh world)]
    (doseq [body (:spheres world)]
      (let [transform (get-body-transform body)
            mesh (assoc-in mesh [:transform] transform)]
        (draw-mesh! world mesh)))))

(defn update-move-plane [world]
  (assoc-in world [:move-plane]
            (get-camera-plane world (get-in world [:camera :pivot]))))

(defn move-cursor [world event]
  (let [x (:x event)
        y (:y event)
        snap-spec (get-closest-snap-point world x y (:snap-specs world))
        [snap-point snap-rotation color snapped]
        (if (nil? snap-spec)
          [(get-ground-camera-point world x y 0) [0 1 0 0] :black false]
          [(:position snap-spec) (:rotation snap-spec) :yellow true])
        transform (make-transform snap-point snap-rotation)]
    (-> world
        (update-in [:cursor] (fn [cursor]
                               (-> cursor
                                   (assoc-in [:transform] transform)
                                   (set-mesh-color color))))
        (assoc-in [:cursor-snapped] snapped))))

(defn create-menus! []
  (set-thing! [:menus :tools]
              (create-menu "resources/tools.svg" 40 153 80 -1))
  (set-thing! [:menus :actions]
              (create-menu "resources/actions.svg" (- 685 40) 152 80 -1))
  (set-thing! [:menus :colors]
              (create-menu "resources/colors.svg" 480 20 -1 40))

  (let [w (get-thing! [:menus :actions :regions :playpause :w])]
    (set-thing! [:pause-image]
                (parse-svg-with-width "resources/pause.svg" (inc w))))
  )

;;-------------------------------------------------------------------------------;;

(do
1

(defn create-world! []
  (set-thing! [] {})
  (set-thing! [:programs :basic] (create-program "basic"))
  (set-thing! [:programs :flat] (create-program "flat"))
  (set-thing! [:programs :textured] (create-program "textured"))
  (set-thing! [:programs :ortho] (create-program "ortho"))
  (set-thing! [:programs :colored] (create-program "colored"))

  (GL11/glEnable GL11/GL_SCISSOR_TEST)
  (GL11/glClearColor 0 0.5 0.8 0)
  (GL11/glEnable GL11/GL_CULL_FACE)
  (GL11/glCullFace GL11/GL_BACK)

  (set-thing! [:projection-matrix] (get-perspective-matrix
                                    10 (/ window-width window-height) 3 1000))

  (update-thing! [] (slots create-camera _ [0 0 1] 40 25 -35))
  (set-thing! [:camera :pivot] [0 0 0])
  (update-thing! [] compute-camera)
  (update-thing! [] #(create-grid-mesh % 24 0.5))
  (set-thing! [:output] (create-ortho-mesh))
  (create-debug-meshes!)
  (clear-output!)
  
  ;;-------------------------------------------------;;

  (set-thing! [:planet] (create-planet!))
  (create-ground! (:planet @world))
  (set-thing! [:sphere-radius] 0.4)

  (let [r (:sphere-radius @world)]
    (set-thing! [:sphere-mesh] (create-sphere-mesh [0 0 0] [1 0 0 0]
                                                   [r r r] :blue)))

  (set-thing! [:meshes :ground] (create-cube-mesh
                                 [0 -0.25 0] [1 0 0 0] [12 0.5 12]
                                 (make-color 40 40 40)))

  (set-thing! [:info] (create-info))

  (set-thing! [:ground-children] {})

  (set-thing! [:wave-editor] {:x 343
                              :y 540
                              :w 685
                              :h 150
                              :functions {}})

  (set-thing! [:time] 0.0)
  (set-thing! [:last-time] 0.0)
  (set-thing! [:paused] true)

  (set-thing! [:spheres] [])
  ;; (update-thing! [] #(create-sphere % [-1.5 0.4 2]))
  ;; (update-thing! [] #(create-sphere % [-2 0.4 0]))

  (create-menus!)

  (set-thing! [:mode] :insert)
  (set-thing! [:mode-part] :block)

  (set-thing! [:snap-specs] (get-snap-specs @world))
  (update-thing! [] update-move-plane)

  (set-thing! [:cursor]
              (create-cone-mesh [0 -5 0] [1 0 0 0] [0.05 0.1 0.05] :black))

  (set-thing! [:cable-mesh] (create-cylinder-mesh [0 0 0] [1 0 0 0]
                                                  [0.03 1 0.03] :blue))

  (set-thing! [:pulley-mesh] (create-cube-mesh [0 1 0] [1 0 0 0]
                                                 [0.15 0.15 0.15] :black))

  (set-thing! [:anchor-mesh] (create-sphere-mesh [0 0 0] [1 0 0 0]
                                               [0.1 0.1 0.1] :black))
  (set-thing! [:new-cable] nil)

  (set-thing! [:bounding-box] (create-cube-mesh [0 0 0] [1 0 0 0]
                                                [0.2 1 0.2] :red))

  (set-thing! [:keys] {:esq {:code 256 :active false}
                       :shift {:code 340 :active false}
                       :period {:code 46 :active false} ;; center
                       :b {:code 66 :active false} ;; block
                       :t {:code 84 :active false} ;; track
                       :s {:code 83 :active false} ;; scale
                       :c {:code 67 :active false} ;; color
                       :d {:code 68 :active false} ;; delete
                       :m {:code 77 :active false} ;; move
                       :g {:code 71 :active false} ;; add graph
                       :w {:code 87 :active false} ;; wagon                       
                       :p {:code 80 :active false} ;; place                       
                       })

  (set-thing! [:current-color] :red)

  (set-thing! [:show-cursor] false)
  (set-thing! [:show-editor] false)
  )
(reset-world!)
)

(defn update-world [world elapsed]
  (if (:paused world)
    world
    (do
      (step-simulation! (:planet world) elapsed)
      (-> world
          (update-in [:time] (fn [v]
                               (if (:paused world)
                                 v
                                 (mod (+ v 0.005) 1))))
          (run-waves)
          (compute-transforms :parts)))))

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))

  (doseq [mesh (vals (:meshes world))]
    (draw-mesh! world mesh))

  (doseq [part (vals (:parts world))]
    (draw-part! world part))

  (draw-spheres! world)
    
  (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
      
  (if (:show-cursor world)
    (draw-mesh! world (:cursor world)))
  )

(defn draw-color-indicator! [world]
  (let [{:keys [x y w h]} (get-in world [:menus :colors
                                         :regions (:current-color world)])]
    (dotimes [i 3]
      (draw-rect! :black x y (- w i 1) (- h i 1)))))

(do
1
(defn draw-2d! [world]
  (if (:show-editor world)
    (draw-wave-editor! world))
  
  (doseq [menu (vals (:menus world))]
    (draw-image! (:image menu) (:x menu) (:y menu)))

  (draw-color-indicator! world)

  (when (not (:paused world))
    (let [{:keys [x y w h]} (get-in world [:menus :actions :regions :playpause])]
      (draw-image! (:pause-image world) x y)))
  )
(reset! redraw-flag true))

(defn mouse-pressed [world event]
  (let [x (:x event)
        y (:y event)
        tools (get-in world [:menus :tools])
        actions (get-in world [:menus :actions])
        colors (get-in world [:menus :colors])
        wave-editor (:wave-editor world)]
    (cond
      (and (inside-box? wave-editor x y)
           (:show-editor world))
      (editor-mouse-pressed world event)

      (or
       (inside-box? tools x y)
       (inside-box? actions x y)
       (inside-box? colors x y))
      (menu-pressed world event)

      (in? (:button event) [:middle :right])
      (assoc-in world [:last-point] [(:x event) (:y event)])

      :else
      (if (:paused world)
        (action-mouse-pressed world event)
        world))))

(defn mouse-moved [world event]
  (cond
    (not-nil? (:last-point world))
    (cond
      (= (:button event) :right) (mouse-rotate world event)
      (= (:button event) :middle) (mouse-pan world event)
      :else world)

    (not-nil? (:editor-last-point world))
    (editor-mouse-moved world event)

    :else
    (if (:paused world)
      (-> world
          (move-cursor event)
          (action-mouse-moved event))
      world)))

(defn mouse-released [world event]
  (let [world (cond
                (not-nil? (:editor-last-point world))
                (editor-mouse-released world event)

                (not-nil? (:last-point world))
                (-> world
                    (dissoc-in [:last-point])
                    (update-move-plane))

                :else
                (if (:paused world)
                  (action-mouse-released world event)
                  world))]
    (draw-2d! world)
    (assoc-in world [:snap-specs] (get-snap-specs world))))

(save-world! [:parts :ground-children])
