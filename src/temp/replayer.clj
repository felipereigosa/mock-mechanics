
;; (import java.awt.Robot)
;; (import java.awt.event.InputEvent)

;; (require '[clojure.java.shell :refer [sh]])
;; (require '[clojure.set :refer [difference]])

;; (load "compiler")
;; (load "interpreter")
;; (load "mouse")
;; (load "path")
;; (load "robot")
;; (load "extend")

(defn replay-draw [world]
  (let [x (- (:window-width world) 10)
        y (- (:window-height world) 10 105)]
    (fill-rect! :black x y 20 20)
    (draw-text! :white "R" (- x 4) (+ y 5) 15)))

(defn toggle-replay [world]
  (if (:replay-filename world)
    (dissoc-in world [:replay-filename])
    (do
      (when (nil? @robot)
        (reset! robot {:robot (new Robot)
                       :origin (get-window-coordinates)
                       :active false}))
      (read-input world
        (fn [w text]
          (-> w
            (assoc-in [:replay-filename] text)
            (assoc-in [:instruction-index] 0)
            (assoc-in [:replay-history] [])
            (update-history)))))))

;; (defn remove-mark [instruction]
;;   (if (.startsWith instruction "*")
;;     (subs instruction 2)
;;     instruction))

(defn replayer-restart [world]
  (let [new-history (vec (take 1 (:replay-history world)))]
    (-> world
      (reset-variables)
      (assoc-in [:parts] (:parts (last new-history)))
      (assoc-in [:camera] (:camera (last new-history)))
      (compute-camera)
      (assoc-in [:replay-history] new-history)
      (assoc-in [:instruction-index] 0)
      (tree-changed))))

(defn replay-forward [world]
  (if (and
        (:replay-filename world)
        (nil? (:animation world)))
    (let [filename (str "res/" (:replay-filename world) ".txt")
          instructions (read-lines filename)
          instructions (extend-instructions instructions)
          index (:instruction-index world)]
      (if (< index (count instructions))
        (-> world
          (run-instruction (nth instructions index))
          (update-in [:instruction-index] inc))
        world))
    world))

(defn replay-back [world]
  (if (and
        (:replay-filename world)
        (> (:instruction-index world) 0))
    (let [new-history (pop (:replay-history world))
          filename (str "res/" (:replay-filename world) ".txt")
          instructions (read-lines filename)
          instructions (extend-instructions instructions)
          instruction (nth instructions (dec (:instruction-index world)))]       
      (println! "<<" (subs instruction 0 (min 100 (count instruction))))
      (-> world
          (assoc-in [:parts] (:parts (last new-history)))
          (assoc-in [:camera] (:camera (last new-history)))
          (compute-camera)
          (assoc-in [:replay-history] new-history)
          (update-in [:instruction-index] dec)
          (tree-changed)))
    world))

(def replaying (atom false))

(defn run-instructions! []
  (if @replaying
    (reset! replaying false)
    (let [filename (str "res/" (get-thing! [:replay-filename]) ".txt")
          instructions (read-lines filename)
          instructions (extend-instructions instructions)
          delay 500]
      (reset! replaying true)
      (while (and @replaying
               (< (:instruction-index @world) (count instructions)))
        (let [instruction (nth instructions (:instruction-index @world))]
          (update-thing! [:instruction-index] inc)
          (when (not (empty? instruction))
            (update-thing! [] #(run-instruction % instruction))
            (while (get-thing! [:animation]))
            (redraw!)
            (sleep delay))))
      (reset! replaying false))))

(defn toggle-run-instructions [world]
  (.start
    (new Thread
      (proxy [Runnable] []
        (run []
          (run-instructions!)))))
  world)

