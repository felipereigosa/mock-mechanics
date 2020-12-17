
(ns temp.core (:gen-class))

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
(import org.lwjgl.glfw.GLFWWindowSizeCallback)
(import org.lwjgl.glfw.GLFWWindowFocusCallback)
(import org.lwjgl.glfw.GLFWWindowMaximizeCallback)                       
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
(import com.bulletphysics.linearmath.Transform)
(import java.net.ServerSocket)
(import java.net.Socket)
(import java.io.BufferedReader)
(import java.io.InputStreamReader)
(import java.nio.ByteBuffer)
(import java.nio.ByteOrder)
(import org.lwjgl.opengl.GL11)
(import org.lwjgl.opengl.GL13)
(import org.lwjgl.opengl.GL20)
(import java.nio.FloatBuffer)
(import java.nio.IntBuffer)
(import java.nio.ByteBuffer)
(import java.nio.ByteOrder)

(declare draw-2d!)
(declare draw-3d!)
(declare draw-ortho-mesh!)
(declare create-ortho-mesh)
(declare redraw)

(defn create-world [])
(defn draw-world! [world])
(defn update-world [world elapsed] world)
(defn key-pressed [world event] world)
(defn key-released [world event] world)
(defn mouse-pressed [world event] world)
(defn mouse-moved [world event] world)
(defn mouse-released [world event] world)
(defn mouse-scrolled [world event] world)
(defn window-changed [world event] world)
(defn window-focused [world focused] world)

(def window-width (atom 800))
(def window-height (atom 600))
(def time-since-update (atom 0))

(defn recompute-viewport [world width height]
  (let [projection-matrix (get-perspective-matrix
                           10 (/ width height) 3 1000)
        world (-> world
                  (assoc-in [:projection-matrix] projection-matrix)
                  (assoc-in [:ortho-mesh] (create-ortho-mesh width height))
                  (assoc-in [:moving-ortho-mesh] (create-ortho-mesh width height))
                  (assoc-in [:window-width] width)
                  (assoc-in [:window-height] height))]
    (GL11/glViewport 0 0 width height)
    (reset! window-width width)
    (reset! window-height height)    
    (redraw world)))

(def to-run (atom nil))

(defn run-pending! []
  (swap! to-run (fn [tr]
                  (when (not (nil? tr))
                    (tr))
                  nil)))

(def mouse-x (atom 0))
(def mouse-y (atom 0))
(def mouse-button (atom nil))

(defn get-button-name [value]
  (get {0 :left
        1 :right
        2 :middle} value))

(defn create-key-handler! [window]
  (let [key-handler (proxy [GLFWKeyCallback] []
                      (invoke [window key scancode action mods]
                        (cond
                          (= action GLFW/GLFW_PRESS)
                          (try
                            (swap! world (fn [w] (key-pressed w {:code key})))
                            (reset! time-since-update 0)
                            (catch Exception e))

                          (= action GLFW/GLFW_RELEASE)
                          (try
                            (swap! world (fn [w] (key-released w {:code key})))
                            (reset! time-since-update 0)
                            (catch Exception e)))))]

    (GLFW/glfwSetKeyCallback window key-handler)
    key-handler))

(defn create-mouse-handler! [window]
  (let [mouse-handler (proxy [GLFWMouseButtonCallback] []
                        (invoke [window button action mods]
                          (let [event {:x @mouse-x
                                       :y @mouse-y
                                       :button (get-button-name button)}]
                            (cond
                              (= action GLFW/GLFW_PRESS)
                              (try
                                (reset! mouse-button (get-button-name button))
                                (swap! world (fn [w] (mouse-pressed w event)))
                                (reset! time-since-update 0)
                                (catch Exception e))

                              (= action GLFW/GLFW_RELEASE)
                              (try
                                (swap! world (fn [w] (mouse-released w event)))
                                (reset! mouse-button nil)
                                (reset! time-since-update 0)
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
                                   (when (or @mouse-button
                                             (and (= (:mode @world) :add)
                                                  (= (:add-type @world) :track)))
                                     (reset! time-since-update 0))
                                   (catch Exception e))))]
    (GLFW/glfwSetCursorPosCallback window mouse-motion-handler)
    mouse-motion-handler))

(defn create-mouse-scroll-handler! [window]
  (let [mouse-scroll-handler (proxy [GLFWScrollCallback] []
                               (invoke [window x y]
                                 (try
                                   (swap! world
                                          (fn [w]
                                            (mouse-scrolled w {:x @mouse-x
                                                               :y @mouse-y
                                                               :amount y})))
                                   (reset! time-since-update 0)
                                   (catch Exception e))))]

    (GLFW/glfwSetScrollCallback window mouse-scroll-handler)
    mouse-scroll-handler))

(defn create-window-size-handler! [window]
  (let [handler (proxy [GLFWWindowSizeCallback] []
                  (invoke [window width height]
                    (try
                      (swap! world
                             (fn [w]
                               (if (empty? w)
                                 w
                                 (window-changed w {:width width
                                                    :height height}))))
                      (reset! time-since-update 0)
                      (catch Exception e))))]
    (GLFW/glfwSetWindowSizeCallback window handler)
    handler))

(defn create-window-focus-handler! [window]
  (let [handler (proxy [GLFWWindowFocusCallback] []
                  (invoke [window focused]
                    (try
                      (swap! world
                             (fn [w]
                               (window-focused w focused)))
                      (catch Exception e))))]
    (GLFW/glfwSetWindowFocusCallback window handler)
    handler))

(def last-time (atom (get-current-time)))

(defn chip-active? [world chip-name]
  (let [elapsed 16 ;;#########################################
        dt (float (/ elapsed 1000))
        chip (get-in world [:parts chip-name])]
    (and
     (not (empty? (:functions chip)))
     (not (>= (:time chip) (+ (:final-time chip)) dt)))))

(declare get-parts-with-type)

(defn any-chip-active? [world]
  (let [chip-names (get-parts-with-type (:parts world) :chip)]
    (some #(chip-active? world %) chip-names)))

(declare motherboard-activation-count)
(declare spheres-moving?)

(defn update-and-draw! [window]
  (try
    (run-pending!)
    (catch Exception e))
  
  (if (or (< @time-since-update 200)
          ;; (not (empty? (:spheres @world)))
          ;; (:forces-active? @world)
          (spheres-moving? @world)
          (not (nil? (:mouse-force @world)))
          )
    (let [current-time (get-current-time)
          elapsed (within (- current-time @last-time) 0 40)]
      ;; (println "update" (rand)) ;;#######################
      (reset! last-time current-time)
      (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT
                            GL11/GL_DEPTH_BUFFER_BIT))
      (try
        (draw-world! @world)
        (catch Exception e))
      (try
        (swap! world (fn [w] (update-world w elapsed)))
        (catch Exception e))

      (GLFW/glfwSwapBuffers window)

      (swap! time-since-update #(+ elapsed %))

      (if (and (in? (:mode @world) [:simulation :graph :motherboard :property])
               (or
                (any-chip-active? @world)
                (> @motherboard-activation-count 0)))
        (reset! time-since-update 0))
      )
    (sleep 5))
  
  (GLFW/glfwPollEvents)
  )

(defn loop! [window]
  (try
    (reset! world (create-world))
    (catch Exception e))

  (reset! time-since-update 0)
  
  (while (not (GLFW/glfwWindowShouldClose window))
    (update-and-draw! window)))

(def the-window (atom nil))

(defn window-init! []
  (.start (new Thread (proxy [Runnable] []
                        (run []
    (GLFW/glfwInit)
    (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
    (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
    (GLFW/glfwWindowHint GLFW/GLFW_SAMPLES 8)
    (GLFW/glfwWindowHint GLFW/GLFW_MAXIMIZED GLFW/GLFW_FALSE)

    (let [width @window-width
          height @window-height
          window (GLFW/glfwCreateWindow width height "-"
                                        MemoryUtil/NULL MemoryUtil/NULL)]

      (reset! the-window window)
      (create-key-handler! window)
      (create-mouse-handler! window)
      (create-mouse-motion-handler! window)
      (create-mouse-scroll-handler! window)
      (create-window-size-handler! window)
      (create-window-focus-handler! window)
          
      (GLFW/glfwMakeContextCurrent window)
      (GLFW/glfwSwapInterval 1)
      (GLFW/glfwShowWindow window)

      (GL/createCapabilities)

      (GL11/glViewport 0 0 width height)
      (GL11/glClearColor 0.0 0.0 0.0 0.0)

      (GL11/glEnable GL11/GL_BLEND)
      (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)

      (GL11/glEnable GL11/GL_DEPTH_TEST)

      (loop! window)

      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwTerminate)      
      ))))))

(defn set-title! [text]
  (GLFW/glfwSetWindowTitle @the-window text))

(defn set-window-size! [width height]
  (GLFW/glfwSetWindowSize @the-window width height))

(declare place-elements)
(declare create-input-indicator)

(defn set-recording! []
  (GLFW/glfwSetWindowSize @the-window 1280 720)
  (update-thing!
   []
   (fn [world]
     (-> world
         (assoc-in [:show-hints] false)
         (assoc-in [:num-lines] 1)
         (create-input-indicator 670)
         (place-elements)
         (redraw)))))

(defmacro gl-thread [form]
  `(reset! to-run (fn [] ~form)))

(def out (atom nil))

(defn gl-println [& forms]
  (binding [*out* @out]
    (apply clojure.core/println forms)))

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

(defn get-text-width! [text size]
  (let [image (get-in @world [:ortho-mesh :image])
        g (get-image-graphics image)
        font (new Font "Dialog" Font/PLAIN size)]
    (.stringWidth (.getFontMetrics g font) text)))

(defn draw-text [image color text x y size]
  (let [g (get-image-graphics image)]
    (.setFont g (new Font "Dialog" Font/PLAIN size))
    (.setColor g (get-color color))
    (.drawString g text (int x) (int y))))

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
    (.drawImage g image2 (int x) (int y) nil)))

(defn image->buffer [image]
  (let [w (.getWidth image)
        h (.getHeight image)
        pixels (make-array Integer/TYPE (* w h))
        bb (ByteBuffer/allocateDirect (* w h 4))]
    (.getRGB image 0 0 w h pixels 0 w)
    (let [ib (.asIntBuffer bb)]
      (.put ib pixels)
      bb)))

(defn reset-texture [mesh]
  (let [id (:texture-id mesh)
        image (:image mesh)
        width (get-image-width image)
        height (get-image-height image)
        buffer (image->buffer image)]
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
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER
                          GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER
                          GL11/GL_NEAREST)
    mesh))

(defn clear!
  ([]
   (let [mesh (get-in @world [:ortho-mesh])]
     (clear (:image mesh))
    (gl-thread (reset-texture mesh))))
  ([color]
   (let [mesh (get-in @world [:ortho-mesh])]
     (clear (:image mesh) color)
    (gl-thread (reset-texture mesh)))))

(defn draw-pixel! [color x y]
  (let [mesh (get-in @world [:ortho-mesh])]
    (draw-pixel (:image mesh) color x y)
    (gl-thread (reset-texture mesh))))

(defn fill-rect! [color x y w h]
  (let [mesh (get-in @world [:ortho-mesh])]
    (fill-rect (:image mesh) color x y w h)
    (gl-thread (reset-texture mesh))))

(defn draw-rect! [color x y w h]
  (let [mesh (get-in @world [:ortho-mesh])]
    (draw-rect (:image mesh) color x y w h)
    (gl-thread (reset-texture mesh))))

(defn fill-circle! [color x y r]
  (let [mesh (get-in @world [:ortho-mesh])]
    (fill-circle (:image mesh) color x y r)
    (gl-thread (reset-texture mesh))))

(defn draw-circle! [color x y r]
  (let [mesh (get-in @world [:ortho-mesh])]
    (draw-circle (:image mesh) color x y r)
    (gl-thread (reset-texture mesh))))

(defn draw-text! [color text x y size]
  (let [mesh (get-in @world [:ortho-mesh])]
    (draw-text (:image mesh) color text x y size)
    (gl-thread (reset-texture mesh))))

(defn draw-text-in-box! [text color size box]
  (let [w (* 0.5 (get-text-width! text size))]
    (draw-text! color text (- (:x box) w) (+ (:y box) 5) size)))

(defn draw-ellipse! [color rect]
  (let [mesh (get-in @world [:ortho-mesh])]
    (draw-ellipse (:image mesh) color rect)
    (gl-thread (reset-texture mesh))))

(defn draw-line! [color x1 y1 x2 y2]
  (let [mesh (get-in @world [:ortho-mesh])]
    (draw-line (:image mesh) color x1 y1 x2 y2)
    (gl-thread (reset-texture mesh))))

(defn fill-polygon! [color points]
  (let [mesh (get-in @world [:ortho-mesh])]
    (fill-polygon (:image mesh) color points)
    (gl-thread (reset-texture mesh))))

(defn draw-polygon! [color points]
  (let [mesh (get-in @world [:ortho-mesh])]
    (draw-polygon (:image mesh) color points)
    (gl-thread (reset-texture mesh))))

(defn draw-image! [image2 x y & corner]
  (let [mesh (get-in @world [:ortho-mesh])]
    (apply draw-image (:image mesh) image2 x y corner)
    (gl-thread (reset-texture mesh))
    nil))

(def redraw-flag (atom true))

(defn draw-world! [world]
  (try
    (draw-3d! world)
    (catch Exception e))

  (when @redraw-flag
    (try
      (draw-2d! world)
      (catch Exception e))
    (reset! redraw-flag false))

  (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
  (draw-ortho-mesh! world (:ortho-mesh world))
  )

(defn redraw! []
  (reset! time-since-update 0)
  (reset! redraw-flag true))
  
(defn redraw [world]
  (reset! time-since-update 0)
  (reset! redraw-flag true)
  world)

(defn -main [& args]
  (window-init!)
  (reset! out *out*)
  nil
  )

(defn create-base-world []
  (GL/createCapabilities)
  (GL11/glClearColor 0 0.5 0.8 0)
  (GL11/glEnable GL11/GL_CULL_FACE)
  (GL11/glCullFace GL11/GL_BACK)

  (-> {}
      (assoc-in [:programs :basic] (create-program "basic"))
      (assoc-in [:programs :flat] (create-program "flat"))
      (assoc-in [:programs :textured] (create-program "textured"))
      (assoc-in [:programs :ortho] (create-program "ortho"))
      (assoc-in [:programs :colored] (create-program "colored"))
      (recompute-viewport @window-width @window-height)
      (#(create-camera % [0 0 1] 40 25 -35))
      (compute-camera)
      ))

(defn reset-world! []
  (gl-thread
   (try
     (reset! world (create-world))
     (catch Exception e))))

(defn set-mesh-position [mesh position]
  (let [transform (:transform mesh)
        rotation (get-transform-rotation transform)]
    (assoc-in mesh [:transform] (make-transform position rotation))))

(defn set-mesh-rotation [mesh rotation]
  (let [transform (:transform mesh)
        position (get-transform-position transform)]
    (assoc-in mesh [:transform] (make-transform position rotation))))

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

(defn quaternion-from-normal [normal]
  (let [normal (vector-normalize normal)]
    (cond
      (vector= normal [0 1 0]) [0 1 0 0]
      (vector= normal [0 -1 0]) [1 0 0 180]
      :else (let [axis (vector-cross-product [0 1 0] normal)
                  angle (vector-angle normal [0 1 0])]
              (conj axis angle)))))

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

(defn get-circle-vertices [r y divisions]
  (let [angles (map (fn [i]
                      (* i (/ 360 divisions)))
                    (range divisions))
        vertices (map (fn [angle]
                        [(* r (cos angle)) y (* r (sin angle))])
                      angles)]
    vertices))

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

(defn draw-textured-mesh! [world mesh transform]
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

    (GL20/glVertexAttribPointer (:texture-coordinates attributes) 2 GL11/GL_FLOAT
                                false 0 (:texture-coordinates-buffer mesh))
    (GL20/glEnableVertexAttribArray (:texture-coordinates attributes))

    (GL13/glActiveTexture GL13/GL_TEXTURE0)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D (:texture-id mesh))
    (GL20/glUniform1i (:texture-diffuse uniforms) 0)

    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 num-vertices)))

(defn create-mesh [vertices position rotation
                   scale skin tex-coords normals]
  (let [scale (if (vector? scale)
                scale
                (vec (repeat 3 scale)))
        vertices (vec (flatten vertices))
        normals (if (empty? normals)
                  (vec (compute-normals vertices))
                  (vec (flatten normals)))
        base-mesh {:vertices vertices
                   :vertices-buffer (get-float-buffer vertices)
                   :normals normals
                   :normals-buffer (get-float-buffer normals)
                   :transform (make-transform position rotation)
                   :scale scale}]
    (cond
      (string? skin)
      (let [texture-id (GL11/glGenTextures)
            tex-coords (vec (flatten tex-coords))]
        (-> base-mesh
            (assoc-in [:draw-fn] draw-textured-mesh!)
            (assoc-in [:program] :textured)
            (assoc-in [:image] (open-image skin))
            (assoc-in [:texture-coordinates] tex-coords)
            (assoc-in [:texture-coordinates-buffer]
                      (get-float-buffer tex-coords))
            (assoc-in [:texture-id] texture-id)
            (set-texture)))

      (sequential? skin)
      (let [colors (vec (flatten skin))]
        (-> base-mesh
            (assoc-in [:colors] colors)
            (assoc-in [:colors-buffer] (get-float-buffer colors))
            (assoc-in [:draw-fn] draw-colored-mesh!)
            (assoc-in [:program] :colored)))

      :else
      (let [color (get-color skin)
            r (/ (get-red color) 255.0)
            g (/ (get-green color) 255.0)
            b (/ (get-blue color) 255.0)]
        (-> base-mesh
            (assoc-in [:color] [r g b 1.0])
            (assoc-in [:draw-fn] draw-lighted-mesh!)
            (assoc-in [:program] :flat))))))

(defn create-cube-mesh [position rotation scale skin]
  (create-mesh (get-cube-vertices)
               position rotation scale skin
               (get-cube-texture-coordinates)
               []))

(defn find-line [lines start]
  (find-if #(.startsWith % start) lines))

(defn parse-line [line]
  (map read-string (rest (.split line " "))))

(defn parse-material [lines]
  (let [name (subs (find-line lines "newmtl") 7)
        texture-line (find-line lines "map_Kd")]
    {name {:diffuse (parse-line (find-line lines "Kd"))
           :texture (if texture-line
                      (str "res/" (subs texture-line 7)))}}))

(defn parse-materials [filename]
  (with-open [reader (clojure.java.io/reader filename)]
    (let [lines (doall (line-seq reader))
          lines (filter (fn [line]
                          (or (.startsWith line "newmtl")
                              (.startsWith line "Kd")
                              (.startsWith line "map_Kd")))
                        lines)
          materials (create-groups [] #(.startsWith % "newmtl") lines)]
      (apply merge (cons {"white" {:diffuse [1 1 1]
                                   :texture nil}}
                         (map parse-material materials))))))

(defn parse-line-with-slashes [line]
  (map (fn [item]
         (map read-string (filter (comp not empty?)
                                  (.split item "/"))))
       (rest (.split line " "))))

(defn use-indices [vector indices]
  (let [min-index (apply min indices)
        indices (map #(- % min-index) indices)]
    (map (fn [v]
           (nth vector v))
         indices)))

(defn create-colors [lines materials]
  (let [material-lines (filter (fn [[index line]]
                                 (.startsWith line "usemtl"))
                               (map vector (range) lines))]
    (mapcat (fn [[i material] [j _]]
              (let [name (subs material 7)
                    color (get-in materials [name :diffuse])]
                (repeat (* (- j i 1) 3) (conj (vec color) 1))))
            material-lines
            (conj (vec (rest material-lines))
                  [(count lines) nil]))))

(defn create-model-mesh [filename position rotation scale color]
  (with-open [reader (clojure.java.io/reader filename)]
    (let [materials-filename (-> filename
                                 (subs 0 (.lastIndexOf filename "."))
                                 (str ".mtl"))
          materials (parse-materials materials-filename)
          lines (filter (fn [line]
                          (or (.startsWith line "o")
                              (.startsWith line "v")
                              (.startsWith line "vn")
                              (.startsWith line "vt")
                              (.startsWith line "f")
                              (.startsWith line "usemtl")))
                        (line-seq reader))
          v (map parse-line (filter #(.startsWith % "v") lines))
          n (map parse-line (filter #(.startsWith % "vn") lines))
          t (map parse-line (filter #(.startsWith % "vt") lines))
          faces (mapcat parse-line-with-slashes
                        (filter #(.startsWith % "f") lines))
          vertices (use-indices v (map first faces))
          normals (use-indices n (map last faces))
          texture-name (some :texture (vals materials))
          texture-coords (if texture-name
                           (use-indices t (map #(nth % 1) faces))
                           [])
          texture-coords (map (fn [[u v]]
                                [u (- 1.0 v)])
                              texture-coords)
          skin (or color
                   texture-name
                   (create-colors lines materials))]
      (create-mesh vertices position rotation scale
                   skin texture-coords normals))))

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

(defn create-grid-mesh [num-cells size]
  (let [vertices (get-grid-vertices num-cells size)
        color (get-color :black)
        r (/ (get-red color) 255)
        g (/ (get-green color) 255)
        b (/ (get-blue color) 255)]
    {:vertices-buffer (get-float-buffer vertices)
     :color [r g b 1.0]
     :transform (make-transform [0.0 0.0 0.0] [0 1 0 0])
     :program :basic
     :scale [1 1 1]
     :draw-fn draw-lines!}))

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

(defn create-ortho-mesh [width height]
  (let [image (new-image width height)
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

(defn create-wireframe-cube [position rotation scale color-name]
  (let [corners [[-0.5 0.5 0.5]
                 [-0.5 -0.5 0.5]
                 [-0.5 0.5 -0.5]
                 [-0.5 -0.5 -0.5]
                 [0.5 0.5 0.5]
                 [0.5 -0.5 0.5]
                 [0.5 0.5 -0.5]
                 [0.5 -0.5 -0.5]]
        indices [0 1 1 3 3 2 2 0
                 4 5 5 7 7 6 6 4
                 0 4 2 6 3 7 1 5]
        vertices (vec (flatten (map (fn [index]
                                      (nth corners index)) indices)))]
    (create-wireframe-mesh vertices position rotation
                           scale color-name)))

