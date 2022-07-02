
(import java.awt.Robot)
(import java.awt.event.InputEvent)

(defn get-window-coordinates []
  (map parse-int
       (clojure.string/split (:out (sh "./window-coords.sh")) #"\n")))

(def robot (atom nil))

(defn reset-robot! []
  (reset! robot {:robot (new Robot)
                 :origin (get-window-coordinates)
                 :active false}))

(defn robot-move [[x y]]
  (let [{:keys [robot origin]} @robot
        [ox oy] origin]
    (.mouseMove robot (+ ox x) (+ oy y))))

(defn get-button-mask [name]
  (case name
    :right InputEvent/BUTTON3_DOWN_MASK
    :left InputEvent/BUTTON1_DOWN_MASK
    :middle InputEvent/BUTTON2_DOWN_MASK))

(defn robot-set-active! [value]
  (swap! robot
         (fn [r]
           (-> r
               (assoc-in [:active] value)
               (assoc-in [:origin] (get-window-coordinates))))))

(defn robot-mouse-press [button]
  (.mousePress (:robot @robot) (get-button-mask button)))

(defn robot-mouse-release [button]
  (.mouseRelease (:robot @robot) (get-button-mask button)))
