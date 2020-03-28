
(ns temp.core)

(load "util")
(load "world")

(import org.lwjgl.glfw.GLFW)
(import org.lwjgl.system.MemoryUtil)
(import org.lwjgl.opengl.GL)
(import org.lwjgl.opengl.GL11)
(import org.lwjgl.opengl.GL12)
(import org.lwjgl.opengl.GL13)
(import org.lwjgl.opengl.GL20)
(import org.lwjgl.opengl.GL30)
;; (import org.lwjgl.glfw.GLFWCursorPosCallback)
;; (import org.lwjgl.glfw.GLFWMouseButtonCallback)
;; (import org.lwjgl.glfw.GLFWKeyCallback)
;; (import org.lwjgl.glfw.GLFWScrollCallback)
;; (import org.lwjgl.glfw.GLFWWindowSizeCallback)
;; (import org.lwjgl.glfw.GLFWWindowMaximizeCallback)                       
;; (import java.awt.image.BufferedImage)
;; (import javax.imageio.ImageIO)
;; (import java.io.File)
;; (import java.awt.Color)
;; (import java.awt.geom.Ellipse2D$Double)
;; (import java.awt.RenderingHints)
;; (import java.awt.Font)
;; (import java.awt.Polygon)
;; (import java.awt.geom.AffineTransform)
;; (import java.awt.AlphaComposite)
;; (import com.bulletphysics.linearmath.Transform)
;; (import java.net.ServerSocket)
;; (import java.net.Socket)
;; (import java.io.BufferedReader)
;; (import java.io.InputStreamReader)
;; (import java.nio.ByteBuffer)
;; (import java.nio.ByteOrder)

(def println! println) ;;##############################

(declare create-world)

(defn -main [& args]
  (.start (new Thread (proxy [Runnable] []
                        (run []
    (GLFW/glfwInit)
    (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
    (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
    (GLFW/glfwWindowHint GLFW/GLFW_SAMPLES 8)
    (GLFW/glfwWindowHint GLFW/GLFW_MAXIMIZED GLFW/GLFW_TRUE)

    (let [width 640
          height 480
          window (GLFW/glfwCreateWindow width height "-"
                                        MemoryUtil/NULL MemoryUtil/NULL)]
      ;; (create-key-handler! window)
      ;; (create-mouse-handler! window)
      ;; (create-mouse-motion-handler! window)
      ;; (create-mouse-scroll-handler! window)
      ;; (create-window-size-handler! window)
          
      (GLFW/glfwMakeContextCurrent window)
      (GLFW/glfwSwapInterval 1)
      (GLFW/glfwShowWindow window)

      (GL/createCapabilities)

      (GL11/glViewport 0 0 width height)
      (GL11/glClearColor 0.0 0.0 0.0 0.0)

      (GL11/glEnable GL11/GL_BLEND)
      (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)

      (GL11/glEnable GL11/GL_DEPTH_TEST)

      (GL/createCapabilities)
      (GL11/glEnable GL11/GL_CULL_FACE)
      (GL11/glCullFace GL11/GL_BACK)

      ;; (try
      ;;   (reset! world (create-world))
      ;;   (catch Exception e))

      (while (not (GLFW/glfwWindowShouldClose window))
        (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT
                              GL11/GL_DEPTH_BUFFER_BIT))
        ;; (try
        ;;   (run-pending!)
        ;;   (catch Exception e))
        ;; (try
        ;;   (draw-world! @world)
        ;;   (catch Exception e))
        ;; (try
        ;;   (swap! world (fn [w] (update-world w 16)))
        ;;   (catch Exception e))

        (GLFW/glfwSwapBuffers window)
        (GLFW/glfwPollEvents))

      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwTerminate)
      ))))))

(defn reset-world! []
  (try
    (reset! world (create-world))
    (catch Exception e)))

(defn draw-mesh! [world mesh]
  )

(defn create-program [base-name]
  )

(defn create-camera [vector distance x-angle y-angle]
  )

(do
1

(defn create-world []
  (println! "world created" (rand))
  (-> {}
      ;; (assoc-in [:programs :basic] (create-program "basic"))
      ;; (assoc-in [:programs :flat] (create-program "flat"))
      ;; (assoc-in [:programs :textured] (create-program "textured"))
      ;; (assoc-in [:programs :ortho] (create-program "ortho"))
      ;; (assoc-in [:programs :colored] (create-program "colored"))
      ;; (recompute-viewport {:width @window-width
      ;;                      :height @window-height})
      ;; (assoc-in [:camera] (create-camera [0 0 1] 40 25 -35))
      ;; (compute-camera)
      ;; (assoc-in [:meshes :cube]
      ;;           (create-cube-mesh [0 0.5 0] [1 0 0 0] [1 1 1] :yellow))

      (assoc-in [:x] 100)
      ))
(reset-world!)
)

(defn draw-3d! [world]
  (doseq [mesh (vals (:meshes world))]
    (draw-mesh! world mesh))
  )
