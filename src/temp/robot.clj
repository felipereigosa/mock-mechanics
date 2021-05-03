
(import java.awt.Robot)
(import java.awt.event.InputEvent)
(import java.awt.event.KeyEvent)
(require '[clojure.java.shell :refer [sh]])

(defn get-window-coordinates []
  (map parse-int
       (clojure.string/split (:out (sh "./window-coords.sh")) #"\n")))

(def robot (atom {:robot (new Robot)
                  :origin (get-window-coordinates)
                  :events [{:type :mouse-press
                            :x 100
                            :y 100
                            :button :right}

                           {:type :mouse-release
                            :x 200
                            :y 100
                            :button :right}
                           ]
                  :active false}))


(defn robot-add-event! [event]
  (if (not (:active @robot))
    (swap! robot
           (fn [r]
             (update-in r [:events]
                        #(conj % event))))))

(defn robot-clear-events! []
  (swap! robot
         (fn [r]
           (assoc-in r [:events] []))))

(defn robot-set-active! [value]
  (swap! robot
         (fn [r]
           (assoc-in r [:active] value))))

(def reverse-keymap
  {GLFW/GLFW_KEY_COMMA               KeyEvent/VK_COMMA 
   GLFW/GLFW_KEY_MINUS               KeyEvent/VK_MINUS 
   GLFW/GLFW_KEY_PERIOD              KeyEvent/VK_PERIOD
   GLFW/GLFW_KEY_SLASH               KeyEvent/VK_SLASH 
   GLFW/GLFW_KEY_0                   KeyEvent/VK_0
   GLFW/GLFW_KEY_1                   KeyEvent/VK_1
   GLFW/GLFW_KEY_2                   KeyEvent/VK_2
   GLFW/GLFW_KEY_3                   KeyEvent/VK_3
   GLFW/GLFW_KEY_4                   KeyEvent/VK_4
   GLFW/GLFW_KEY_5                   KeyEvent/VK_5
   GLFW/GLFW_KEY_6                   KeyEvent/VK_6
   GLFW/GLFW_KEY_7                   KeyEvent/VK_7
   GLFW/GLFW_KEY_8                   KeyEvent/VK_8
   GLFW/GLFW_KEY_9                   KeyEvent/VK_9
   GLFW/GLFW_KEY_A                   KeyEvent/VK_A
   GLFW/GLFW_KEY_B                   KeyEvent/VK_B
   GLFW/GLFW_KEY_C                   KeyEvent/VK_C
   GLFW/GLFW_KEY_D                   KeyEvent/VK_D
   GLFW/GLFW_KEY_E                   KeyEvent/VK_E
   GLFW/GLFW_KEY_F                   KeyEvent/VK_F
   GLFW/GLFW_KEY_G                   KeyEvent/VK_G
   GLFW/GLFW_KEY_H                   KeyEvent/VK_H
   GLFW/GLFW_KEY_I                   KeyEvent/VK_I
   GLFW/GLFW_KEY_J                   KeyEvent/VK_J
   GLFW/GLFW_KEY_K                   KeyEvent/VK_K
   GLFW/GLFW_KEY_L                   KeyEvent/VK_L
   GLFW/GLFW_KEY_M                   KeyEvent/VK_M
   GLFW/GLFW_KEY_N                   KeyEvent/VK_N
   GLFW/GLFW_KEY_O                   KeyEvent/VK_O
   GLFW/GLFW_KEY_P                   KeyEvent/VK_P
   GLFW/GLFW_KEY_Q                   KeyEvent/VK_Q
   GLFW/GLFW_KEY_R                   KeyEvent/VK_R
   GLFW/GLFW_KEY_S                   KeyEvent/VK_S
   GLFW/GLFW_KEY_T                   KeyEvent/VK_T
   GLFW/GLFW_KEY_U                   KeyEvent/VK_U
   GLFW/GLFW_KEY_V                   KeyEvent/VK_V
   GLFW/GLFW_KEY_W                   KeyEvent/VK_W
   GLFW/GLFW_KEY_X                   KeyEvent/VK_X
   GLFW/GLFW_KEY_Y                   KeyEvent/VK_Y
   GLFW/GLFW_KEY_Z                   KeyEvent/VK_Z
   GLFW/GLFW_KEY_LEFT_BRACKET        KeyEvent/VK_OPEN_BRACKET    
   GLFW/GLFW_KEY_RIGHT_BRACKET       KeyEvent/VK_CLOSE_BRACKET   
   GLFW/GLFW_KEY_ESCAPE              KeyEvent/VK_ESCAPE          
   GLFW/GLFW_KEY_GRAVE_ACCENT        KeyEvent/VK_BACK_QUOTE      
   GLFW/GLFW_KEY_ENTER               KeyEvent/VK_ENTER           
   GLFW/GLFW_KEY_BACKSPACE           KeyEvent/VK_BACK_SPACE      
   GLFW/GLFW_KEY_DELETE              KeyEvent/VK_DELETE          
   GLFW/GLFW_KEY_RIGHT               KeyEvent/VK_RIGHT           
   GLFW/GLFW_KEY_LEFT                KeyEvent/VK_LEFT            
   GLFW/GLFW_KEY_DOWN                KeyEvent/VK_DOWN            
   GLFW/GLFW_KEY_UP                  KeyEvent/VK_UP              
   GLFW/GLFW_KEY_LEFT_SHIFT          KeyEvent/VK_SHIFT           
   GLFW/GLFW_KEY_RIGHT_SHIFT         KeyEvent/VK_SHIFT           
   GLFW/GLFW_KEY_LEFT_CONTROL        KeyEvent/VK_CONTROL         
   GLFW/GLFW_KEY_RIGHT_CONTROL       KeyEvent/VK_CONTROL         
   GLFW/GLFW_KEY_LEFT_ALT            KeyEvent/VK_ALT             
   GLFW/GLFW_KEY_RIGHT_ALT           KeyEvent/VK_ALT             
   })

(defn robot-move [x y]
  (let [{:keys [robot origin]} @robot
        [ox oy] origin]
    (.mouseMove robot (+ ox x) (+ oy y))))

(defn get-button-mask [name]
  (case name
    :right InputEvent/BUTTON3_DOWN_MASK
    :left InputEvent/BUTTON1_DOWN_MASK
    :middle InputEvent/BUTTON2_DOWN_MASK
    ))

(defn robot-mouse-press [button]
  (.mousePress (:robot @robot) (get-button-mask button)))

(defn robot-mouse-release [button]
  (.mouseRelease (:robot @robot) (get-button-mask button)))

(defn move-from-to [speed start end]
  (let [d (distance start end)
        v (vector-normalize (vector-subtract end start))
        start-time (get-current-time)]
    (loop []
        (let [time (get-current-time)
              elapsed (- (get-current-time) start-time)
              dt (/ elapsed 1000.0)
              ds (* dt speed)
              point (->> (vector-multiply v ds)
                         (vector-add start)
                         (map round)
                         (vec))]
          (sleep 16)
          (if (>= (distance start point) d)
            (apply robot-move end)
            (do
              (apply robot-move point)
              (recur)))))))

(defn robot-key-press [code]
  (.keyPress (:robot @robot) (get reverse-keymap code)))

(defn robot-key-release [code]
  (.keyRelease (:robot @robot) (get reverse-keymap code)))

(defn robot-run-events! []
  (robot-set-active! true)
  (let [speed 250
        evts (:events @robot)]
    (loop [x (* @window-width 0.5)
           y (* @window-height 0.5)
           events evts]
      (let [event (first events)]
        (when (not (empty? events))
          (case (:type event)
            :delay
            (do
              (sleep (:amount event))
              (recur x y (rest events)))

            :key-press
            (do
              (robot-key-press (:code event))
              (recur x y (rest events)))

            :key-release
            (do
              (robot-key-release (:code event))
              (recur x y (rest events)))

            (let [ex (:x event)
                  ey (:y event)]

              (when (not= [x y] [ex ey])
                (move-from-to speed [x y] [ex ey]))

              (if (= (:type event) :mouse-press)
                (robot-mouse-press (:button event))
                (robot-mouse-release (:button event)))

              (recur ex ey (rest events))))))))
  (robot-set-active! false))

(robot-clear-events!)
(:events @robot)
