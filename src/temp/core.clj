
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
  (let [v-name (str "res/" base-name "-vert.glsl")
        f-name (str "res/" base-name "-frag.glsl")
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

(defn get-transform-matrix [transform]
  (let [matrix (into-array Float/TYPE (range 16))]
    (.getOpenGLMatrix transform matrix)
    matrix))

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

(defn create-mesh [vertices position rotation scale skin tex-coords]
  (let [base-mesh {:vertices (into-array Double/TYPE (map double vertices))
                   :vertices-buffer (get-float-buffer vertices)
                   :normals-buffer (get-float-buffer (into [] (compute-normals vertices)))
                   :transform (make-transform position rotation)
                   :draw-fn draw-lighted-mesh!
                   :scale scale}]
    (if (not (is-image? skin))
      (let [color (get-color skin)
            r (/ (get-red color) 255)
            g (/ (get-green color) 255)
            b (/ (get-blue color) 255)]
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
        last-lines (take-last 6 lines)
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
  (let [x-speed 0.3
        y-speed 0.3
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

;; (defn place-pivot! [object-name]
;;   (let [world @world]
;;     (if-let [object (get-in world [:objects object-name])]
;;       (let [body (:body object)
;;             transform (get-body-transform body)]
;;         (set-thing! [:camera :pivot] (get-transform-position transform))
;;         (update-thing! [] (slots compute-camera _))))))

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
;; begin

;;-------------------------------------------------------------------------------;;
;; collision

(defn get-mesh-triangles [mesh transform]
  (let [vertices (partition 3 (into [] (:vertices mesh)))
        matrix (multiply-matrices
                (apply get-scale-matrix (:scale mesh))
                (get-transform-matrix transform))
        vertices (map (fn [[x y z]]
                        (let [vertex (into-array Float/TYPE [x y z 1])]
                          (butlast (into [] (multiply-matrix-vector
                                             matrix vertex)))))
                      vertices)]
    (partition 3 vertices)))

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

(defn get-mesh-collision [mesh transform line]
  (let [triangles (get-mesh-triangles mesh transform)
        measured-triangles (map (fn [i]
                                  {:d (line-triangle-distance
                                       line (nth triangles i))
                                   :i i})
                                (range (count triangles)))
        collision (first (sort-by :d distance-comparator measured-triangles))]
    (if (nil? (:d collision))
      nil
      [(:i collision) (:d collision) (line-get-point line (:d collision))])))

(defn get-part-collision [world px py]
  (let [line (unproject-point world [px py])
        distances (map (fn [[name part]]
                         (let [type (:type part)
                               info (get-in world [:info type])
                               mesh (or (:collision-box info)
                                        (:model info))
                               transform (:transform part)
                               [_ d p] (get-mesh-collision mesh transform line)]
                           (if (nil? d)
                             nil                             
                             {:part-name name
                              :distance d
                              :point p})))
                       (:parts world))]
    (first (sort-by :distance (remove-nil distances)))))

(defn get-part-at [world px py]
  (:part-name (get-part-collision world px py)))

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
;; track loop

(defn get-parts-with-type [parts type]
  (filter (fn [part]
            (= (get-in parts [part :type]) type))
          (keys parts)))

(defn tracks-connected? [parts t0-name t1-name]
  ;;###################################################### two start loop bug
  (let [t0 (get-in parts [t0-name])
        t1 (get-in parts [t1-name])]
    (or
     (in? t1-name (keys (:children t0)))
     (in? t0-name (keys (:children t1))))))

(defn get-neighbours [parts track-names track-name]
  (filter (fn [other-track-name]
            (and
             (not (= other-track-name track-name))
             (tracks-connected? parts other-track-name track-name)))
          track-names))

(defn grow-loop [parts track-names loop color]
  (let [start (first loop)
        end (last loop)
        get-next (fn [tip]
                   (first
                    (filter (fn [track-name]
                              (let [track (get-in parts [track-name])]
                                (and
                                 (= (:color track) color)
                                 (not (in? track-name loop)))))
                            (get-neighbours parts track-names tip))))
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
        (recur parts track-names new-loop color)))))

(defn get-track-loop [parts t0-name]
  (let [track-names (get-parts-with-type parts :track)
        t0 (get-in parts [t0-name])
        loop-color (:color t0)]
    (grow-loop parts track-names [t0-name] loop-color)))

(defn set-wagon-loop [world wagon-name track-name]
  (let [type (get-in world [:parts wagon-name :type])]
    (if (= type :wagon)
      (let [loop (get-track-loop (:parts world) track-name)]
        (-> world
            (assoc-in [:parts wagon-name :base] track-name)
            (assoc-in [:parts wagon-name :loop] loop)))
      world)))

(defn interpolate-points [p1 p2 t]
  (vector-add
   (vector-multiply p2 t)
   (vector-multiply p1 (- 1.0 t))))

(defn get-position-in-track [parts info track-name loop t]
  (let [track (get-in parts [track-name])
        transform (:transform track)
        pairs (map vector loop (rest loop))
        before-track (first (find-if (fn [[a b]]
                                       (= b track-name))
                                     pairs))
        after-track (second (find-if (fn [[a b]]
                                       (= a track-name))
                                     pairs))
        epsilon 0.001]
  (cond
    (= track-name (first loop))
    (let [after-point (get-shared-point parts info track-name after-track)
          p1 (apply-transform transform [0 -0.5 0])
          p2 (apply-transform transform [0 0.5 0])
          before-point (if (< (distance p1 after-point) epsilon)
                         p2 p1)]
      (interpolate-points before-point after-point t))

    (= track-name (last loop))
    (let [before-point (get-shared-point parts info before-track track-name)
          p1 (apply-transform transform [0 -0.5 0])
          p2 (apply-transform transform [0 0.5 0])
          after-point (if (< (distance p1 before-point) epsilon)
                         p2 p1)]
      (interpolate-points before-point after-point t))
    
    :else
    (let [before-point (get-shared-point parts info before-track track-name)
          after-point (get-shared-point parts info track-name after-track)]
      (interpolate-points before-point after-point t)))))

(defn get-position-in-loop [parts info loop t]
  (let [num-tracks (count loop)
        track-number (within (int (map-between-ranges
                                   t 0.0 1.0 0 num-tracks))
                             0 (dec num-tracks))
        track-size (/ 1.0 num-tracks)
        local-t (if (< (abs (- t 1.0)) 0.00001)
                  1.0
                  (/ (mod t track-size) track-size))
        track-name (nth loop track-number)]
    (get-position-in-track parts info track-name loop local-t)))

;;-------------------------------------------------------------------------------;;
;; mechanical tree

(declare compute-subtree-transforms)

(defn get-shared-point [parts info p1-name p2-name]
  (let [p1 (get-in parts [p1-name])
        p1-transform (:transform p1)
        p1-points (map #(apply-transform p1-transform %)
                       (get-in info [(:type p1) :points]))
        p2 (get-in parts [p2-name])
        p2-transform (:transform p2)
        p2-points (map #(apply-transform p2-transform %)
                       (get-in info [(:type p2) :points]))
        epsilon 0.001]
    (find-if (comp not nil?)
             (mapcat (fn [a]
                       (map (fn [b]
                              (if (< (distance a b) epsilon)
                                a
                                nil))
                            p2-points))
                     p1-points))))

(defn compute-children-transforms [parts info part-name transform]
  (let [part (get-in parts [part-name])] 
    (reduce (fn [ps [child-name relative-transform]]
              (let [new-transform (combine-transforms
                                   relative-transform transform)]
                (compute-subtree-transforms ps info child-name new-transform)))
            parts
            (:children part))))

(defn block-compute-subtree-transforms [parts info name transform]
  (compute-children-transforms
   (assoc-in parts [name :transform] transform)
   info name transform))

(defn axle-compute-subtree-transforms [parts info name transform]
  (let [axle (get-in parts [name])
        angle (:angle axle)
        angle-transform (make-transform [0 0 0] [0 1 0 angle])
        transform (combine-transforms angle-transform transform)
        parts (assoc-in parts [name :transform] transform)]
    (if-let [[child-name relative-transform] (first (:children axle))]
      (let [new-transform (combine-transforms
                           relative-transform transform)]
        (compute-subtree-transforms parts info child-name new-transform))
      parts)))

(defn wagon-compute-subtree-transforms [parts info name transform]
  (let [wagon (get-in parts [name])]
    (if (nil? (:base wagon))
      parts
      (let [transform (get-in parts [(:base wagon) :transform])
            rotation (get-transform-rotation transform)
            position (get-position-in-loop parts info (:loop wagon) (:t wagon))
            transform (make-transform position rotation)
            parts (assoc-in parts [name :transform] transform)]
        (compute-children-transforms parts info name transform)))))

(defn compute-subtree-transforms [parts info name transform]
  (let [part (get-in parts [name])]
    (case (:type part)
      :block (block-compute-subtree-transforms parts info name transform)
      :track (block-compute-subtree-transforms parts info name transform)
      :axle (axle-compute-subtree-transforms parts info name transform)
      :wagon (wagon-compute-subtree-transforms parts info name transform)
      parts)))

(defn compute-transforms [world]
  (assoc-in world [:parts]
            (reduce (fn [ps [name relative-transform]]
                      (compute-subtree-transforms ps (:info world)
                                                  name relative-transform))
                    (:parts world)
                    (:ground-children world))))

;;-------------------------------------------------------------------------------;;
;; placing

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
        ;;                        [[(- a 5.5) 0 (- b 5.5)] [1 0 0 0] :ground])
        ;;                      (create-combinations (range 12) (range 12))))

        grid-specs [[[0.5 0 0.5] [1 0 0 0] :ground]]
        moving-part (:moving-part world)
        face-specs
        (vec
         (remove-nil
          (mapcat (fn [[name part]]
                    (if (= name moving-part)
                      nil
                      (let [part (get-in world [:parts name])
                            transform (:transform part)
                            position (get-transform-position transform)
                            rotation (get-transform-rotation transform)
                            points (get-in world [:info (:type part) :points])]
                        (map (fn [p]
                               (conj (make-spec position rotation p) name))
                             points))))
                  (:parts world))))]
    (vec (concat grid-specs face-specs))))

(defn mouse-pressed [world event]
  (if-let [moving-part (get-part-at world (:x event) (:y event))]
    (let [part (get-in world [:parts moving-part])
          transform (:transform part)
          rotation (get-transform-rotation transform)
          position (get-transform-position transform)]
      (-> world
          (assoc-in [:moving-part] moving-part)
          (assoc-in [:start-rotation] rotation)
          (assoc-in [:parts moving-part :position] position)
          (assoc-in [:parts moving-part :rotation] rotation)
          ((fn [w]
             (assoc-in w [:snap-specs] (get-snap-specs w))))
          (assoc-in [:plane] (get-camera-plane world position))))
    (assoc-in world [:last-point] [(:x event) (:y event)])))

(defn mouse-place [world event]
  (let [moving-part (:moving-part world)
        line (unproject-point world [(:x event) (:y event)])
        snap-specs (:snap-specs world)
        close-points (filter (fn [[p _ _]]
                               (< (point-line-distance p line) 0.2))
                             snap-specs)
        eye (get-in world [:camera :eye])
        snap-spec (first (sort-by (fn [[p _ _]]
                                    (distance p eye)) close-points))]
    (if (nil? snap-spec)
      (let [plane (:plane world)
            plane-point (line-plane-intersection line plane)
            transform (make-transform plane-point (:start-rotation world))]
        (-> world
            (assoc-in [:parts moving-part :transform] transform)
            (assoc-in [:parts moving-part :snapped] false)))
      (let [[final-point final-rotation static-part] snap-spec
            rotation-transform (make-transform [0 0 0] final-rotation)
            part (get-in world [:parts moving-part])
            [_ sy _] (get-in world [:info (:type part) :scale])
            offset [0 (* sy 0.5) 0]
            offset (apply-transform rotation-transform offset)
            final-point (vector-add final-point offset)
            final-transform (make-transform final-point final-rotation)]
        (-> world
            (assoc-in [:parts moving-part :transform] final-transform)
            (assoc-in [:parts moving-part :snapped] true)
            (assoc-in [:static-part] static-part))))))

(defn mouse-moved [world event]
  (cond
    (not-nil? (:moving-part world)) (mouse-place world event)

    (not-nil? (:last-point world))
    (cond
      (= (:button event) :left) (mouse-rotate world event)
      (= (:button event) :right) (mouse-pan world event)
      :else world)

    :else world))

(defn update-parent [world child-name parent-name]
  (let [child (get-in world [:parts child-name])]
    (if (= parent-name :ground)
      (assoc-in world [:ground-children child-name] (:transform child))
      (let [parent (get-in world [:parts parent-name])
            child-transform (:transform child)
            parent-transform (:transform parent)
            relative-transform (remove-transform child-transform
                                                 parent-transform)]
        (assoc-in world [:parts parent-name :children child-name]
                  relative-transform)))))

(defn mouse-released [world event]
  (if-let [moving-part (:moving-part world)]
    (let [snapped (get-in world [:parts moving-part :snapped])
          world (-> world
                    (dissoc-in [:moving-part])
                    (dissoc-in [:snap-specs]))]
      (if snapped
        (-> world
            (set-wagon-loop moving-part (:static-part world))
            (update-parent moving-part (:static-part world))
            (compute-transforms))
        world))
    (dissoc-in world [:last-point])))

;;-------------------------------------------------------------------------------;;

(defn create-info []
  {:block {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [1 1 1] :white)
           :points [[0.5 0 0] [-0.5 0 0]
                    [0 0.5 0] [0 -0.5 0]
                    [0 0 0.5] [0 0 -0.5]]
           :scale [1 1 1]
           }

   :axle {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                       [0.2 1 0.2] :white)
          :points [[0 0.5 0] [0 -0.5 0]]
          :scale [0.2 1 0.2]
          }

   :track {:model (create-cylinder-mesh [0 0 0] [1 0 0 0]
                                        [0.1 1 0.1] :white)
           :points [[0 0.5 0] [0 -0.5 0]
                    [-0.5 0 0] [0.5 0 0]
                    [0 0 -0.5] [0 0 0.5]
                    ]
           :scale [0.1 1 0.1]
           }

   :wagon {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [0.5 0.5 0.5] :white)
           :points [[0.25 0 0] [-0.25 0 0]
                    [0 0.25 0] [0 -0.25 0]
                    [0 0 0.25] [0 0 -0.25]]
           :scale [1 1 1]
           }

   ;; :pulley {:model (create-cone-mesh [0 0 0] [1 0 0 0]
   ;;                                   [0.3 0.3 0.3] :white)
   ;;          :points [[0 -0.15 0]]
   ;;          :scale [0.3 0.3 0.3]
   ;;          }

   ;; :anchor {:model (create-cube-mesh [0 0 0] [1 0 0 0]
   ;;                                   [0.2 0.2 0.2] :white)
   ;;          :points [[0 -0.1 0]]
   ;;          :scale [0.2 0.2 0.2]
   ;;          }

   ;; :cable {:model (create-cylinder-mesh [0 0 0] [1 0 0 0]
   ;;                                      [0.2 1 0.2] :white)
   ;;         :thickness 0.03}

   ;; :spring {:model (create-model-mesh "res/spring.obj" [0 0 0]
   ;;                                    [1 0 0 0] [1 1 1] :white)
   ;;          :points [[0 0.5 0] [0 -0.5 0]]
   ;;          :scale [0.2 1 0.2]
   ;;          :collision-box (create-cylinder-mesh [0 0 0] [1 0 0 0]
   ;;                                               [0.35 1 0.35] :white)
   ;;        }
   })

(defn make-part! [type color]
  (let [x (- (rand-int 10) 5)
        z (- (rand-int 10) 5)
        part {:type type
              :color color
              :transform (make-transform [x 0.5 z] [0 1 0 0])}
        part (cond
               (= type :axle) (assoc-in part [:angle] 0)
               (= type :wagon) (assoc-in part [:t] 0.0)
               :else part)]
    (set-thing! [:parts (gen-keyword type)] part)))

(defn draw-part! [world part]
  (let [info (get-in world [:info (:type part)])
        transform (:transform part)
        mesh (-> (:model info)
                 (assoc-in [:transform] transform)
                 (set-mesh-color (:color part)))]
    (draw-mesh! world mesh)))

(do
1

(defn create-world! []
  (set-thing! [] {})
  (set-thing! [:programs :basic] (create-program "basic"))
  (set-thing! [:programs :flat] (create-program "flat"))
  (set-thing! [:programs :textured] (create-program "textured"))
  (set-thing! [:programs :ortho] (create-program "ortho"))

  (GL11/glEnable GL11/GL_SCISSOR_TEST)
  (GL11/glClearColor 0 0.5 0.8 0)

  (set-thing! [:projection-matrix] (get-perspective-matrix
                                    10 (/ window-width window-height) 3 1000))

  (update-thing! [] (slots create-camera _ [0 0 1] 60 25 -35))
  (update-thing! [] #(create-grid-mesh % 12 1))
  (set-thing! [:output] (create-ortho-mesh))
  (clear-output!)

  ;;-------------------------------------------------;;

  (set-thing! [:meshes :ground] (create-cube-mesh
                                 [0 -0.25 0] [1 0 0 0] [12 0.5 12]
                                 (make-color 40 40 40)))

  
  (let [info (create-info)]
    (set-thing! [:info] info)

    (set-thing! [:ground-children] {:axle (make-transform [0 0.5 0] [1 0 0 0])})

    (set-thing! [:parts :axle] {:type :axle
                                :color :green
                                :angle 54
                                :transform (make-transform [0 0 0] [0 1 0 0])
                                :children {:block (make-transform [0 1 0] [0 1 0 0])}
                                })

    (set-thing! [:parts :block] {:type :block
                                 :color :white
                                 :transform (make-transform [0 0 0] [0 1 0 0])
                                 :children {:track0 (make-transform [1 0 0]
                                                                    [0 0 1 90])}
                                 })

    (set-thing! [:parts :track0] {:type :track
                                  :color :white
                                  :transform (make-transform [0 0 0] [0 1 0 0])
                                  :children {:track1 (make-transform [0 0 1]
                                                                     [1 0 0 90])}
                                  
                                  })

    (set-thing! [:parts :track1] {:type :track
                                  :color :red
                                  :transform (make-transform [0 0 0] [0 1 0 0])
                                  :children {:track2 (make-transform [0 1 0]
                                                                     [1 0 0 0])
                                             ;; :wagon (make-transform [0 0 1]
                                             ;;                        [1 0 0 0])
                                             }
                                  })

    (set-thing! [:parts :track2] {:type :track
                                  :color :red
                                  :transform (make-transform [0 0 0] [0 0 0 0])
                                  })

    (set-thing! [:parts :wagon] {:type :wagon
                                 :color :yellow
                                 :t 0
                                 :transform (make-transform [2 0 0] [0 1 0 0])
                                 ;; :children {:axle2 (make-transform [0 0 -0.5]
                                 ;;                                   [1 0 0 90])}
                                 })

    ;; (set-thing! [:parts :axle2] {:type :axle
    ;;                              :color :purple
    ;;                              :angle 0.0
    ;;                              :transform (make-transform [0 0 0] [0 1 0 0])
    ;;                              })

;;-------------------------------------------------------------------------------;;

    ;; (set-thing! [:ground-children] {:track0 (make-transform [0 0.5 0] [1 0 0 0])})
    
    ;; (set-thing! [:parts :track0] {:type :track
    ;;                               :color :white
    ;;                               :transform (make-transform [0 0 0] [1 0 0 0])
    ;;                               :children {:track1 (make-transform [0 1 0]
    ;;                                                                  [1 0 0 0])}
    ;;                               })

    ;; (set-thing! [:parts :track1] {:type :track
    ;;                               :color :red
    ;;                               :transform (make-transform [0 0 0] [1 0 0 0])
    ;;                               :children {:track2 (make-transform [0 1 0]
    ;;                                                                  [1 0 0 0])}
    ;;                               })

    ;; (set-thing! [:parts :track2] {:type :track
    ;;                               :color :red
    ;;                               :transform (make-transform [0 0 0] [1 0 0 0])
    ;;                               :children {:track3 (make-transform [0 1 0]
    ;;                                                                 [0 0 1 180])
    ;;                                          :wagon (make-transform [0 0 0]
    ;;                                                                 [1 0 0 0])}
    ;;                               })

    ;; (set-thing! [:parts :track3] {:type :track
    ;;                               :color :red
    ;;                               :transform (make-transform [0 0 0] [1 0 0 0])
    ;;                               :children {:track4 (make-transform [0 -1 0]
    ;;                                                                  [0 0 1 180])}
    ;;                               })

    ;; (set-thing! [:parts :track4] {:type :track
    ;;                               :color :red
    ;;                               :transform (make-transform [0 0 0] [1 0 0 0])
    ;;                               })

    ;; (set-thing! [:parts :wagon] {:type :wagon
    ;;                              :color :yellow
    ;;                              ;; :base :track1
    ;;                              ;; :loop [:track1 :track2 :track3 :track4]
    ;;                              :t 0
    ;;                              :transform (make-transform [3 0 0] [0 1 0 0])
    ;;                              })

    (update-thing! [] compute-transforms)
    )
  )

(reset-world!)
)

(defn update-world [world elapsed]
  (-> world
      (update-in [:parts :axle :angle] (fn [v] (mod (+ v 0.5) 360)))
      (update-in [:parts :wagon :t] (fn [v] (mod (+ v 0.005) 1)))
      (compute-transforms)
      )
  )

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))

  (doseq [mesh (vals (:meshes world))]
    (draw-mesh! world mesh))

  (doseq [part (vals (:parts world))]
    (draw-part! world part))
  )

(reset-world!)
(make-part! :wagon :purple)

;; (do
;;   (set-thing! [:parts :wagon :t] 0)
;;   (update-thing! [] compute-transforms))


;; (println! (get-thing! [:parts :wagon :loop]))

;; (set-thing! [:parts :wagon :loop] [:track2 :track6733 :track6749 :track6765 :track6781 :track6797 :track6813 :track1])


(make-part! :axle :green)
