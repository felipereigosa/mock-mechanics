(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

(load "replayer/compiler")
(load "replayer/robot")
(load "replayer/interpreter")
(load "replayer/extend")
(load "replayer/helpers")

(defn load-instructions [world]
  (assoc-in world [:instructions]
            (-> (str "replayer/" (:replay-filename world) ".txt")
                (read-lines)
                (extend-instructions))))

(def history (atom []))

(defn start-replay [world filename]
  (reset-robot!)
  (let [w (-> world
              (assoc-in [:replay-filename] filename)
              (assoc-in [:instruction-index] 0)
              (assoc-in [:replay-speed] 1.0)
              (load-instructions))]
    (println "-----")
    (reset! history [])
    (reset! replaying false)
    w))

(defn skip-instruction? [world instruction]
  (if (or (.startsWith instruction ";;")
          (empty? (.trim instruction)))
    true
    (let [elements (read-string (str "[" instruction "]"))]
      (cond
        (.startsWith instruction "set variable")
        (let [[_ _ key _ value] elements
              key (keyword key)
              value (keyword value)]
          (= (get-in world [key]) value))

        (.startsWith instruction "select motherboard")
        (let [[_ part-name _ tab-num] elements
              part-name (keyword part-name)]
          (= (get-in world [:parts part-name :tab]) tab-num))

        (.startsWith instruction "select")
        (let [[_ part-name] elements
              part-name (keyword part-name)]
          (= part-name (:last-selected-part world)))

        (.startsWith instruction "set view")
        (let [[_ _ _ chip-name _ end-view] elements
              chip-name (keyword chip-name)
              chip (get-in world [:parts chip-name])
              {:keys [zoom-x zoom-y offset]} (:view chip)
              [x y] offset]
          (vector/equal? end-view [x y zoom-x zoom-y]))

        :else false))))

(def step (atom false))

(defn shift-times [events]
  (let [start-time (third (first events))]
    (map #(update-in % [2] (fn [t] (- t start-time))) events)))

(defn toggle-mouse-recording [world]
  (if (:mouse-recording world)
    (let [events (->> (:mouse-instruction world)
                      (shift-times)
                      (interpose " ")
                      (apply str))]
      (println "-----------------------------------------")
      (println "mouse true" events)
      (assoc-in world [:mouse-recording] false))
    (do
      (println "recording mouse...")
      (-> world
          (assoc-in [:mouse-recording] true)
          (assoc-in [:mouse-instruction] [])))))

(defn add-event [world type event]
  (update-in world [:mouse-instruction]
             #(conj % [(int (:x event))
                       (int (:y event))
                       (get-current-time)
                       type
                       (:button event)])))

(defn replay-pressed [world event]
  (if (and (:replay-filename world)
           (not @replaying)
           (:mouse-recording world))
    (add-event world :pressed event)
    world))

(defn replay-moved [world event]
  (if (and (:replay-filename world)
           (not @replaying)
           (:mouse-recording world))
    (add-event world :moved event)
    world))

(defn replay-released [world event]
  (if (and (:replay-filename world)
           (not @replaying)
           (:mouse-recording world))
    (add-event world :released event)
    world))

(def replaying (atom false))
(def last-instruction (atom ""))
(def wait-chip-flag (atom false))

(defn run-instructions! []
  (if @replaying
    (reset! replaying false)
    (let [instructions (:instructions @world)]
      (reset! replaying true)

      (while (and
               @replaying
               (< (:instruction-index @world) (count instructions)))

        ;; skip instructions
        (while (skip-instruction?
                 @world
                 (nth instructions (:instruction-index @world)))
          (update-thing! [:instruction-index] inc))

        (when (.startsWith (nth instructions (:instruction-index @world)) ">")
          (println "checkpoint reached")
          (reset! replaying false)
          (update-thing! [:instruction-index] inc))

        ;; big or small delay depending on current and last instructions
        (if (not @step)
          (let [last @last-instruction
                next (nth instructions (:instruction-index @world))]
            (if (not (or
                       (starts-with? last "sleep")
                       (starts-with? next "sleep")))
              (if (or
                    (and (starts-with? last "set variable mode")
                         (starts-with? next "set variable"))

                    (and (starts-with? last "scale")
                         (starts-with? next "scale"))

                    (and (starts-with? last "set view")
                         (starts-with? next "put"))

                    (starts-with? next "add motherboard")
                    )
                (sleep (int (/ 400 (:replay-speed @world))))
                (sleep (int (/ 800 (:replay-speed @world))))))))

        ;; save last instruction
        (reset! last-instruction
                (nth instructions (:instruction-index @world)))

        ;; run instruction
        (let [instruction (nth instructions (:instruction-index @world))]
          (if (starts-with? instruction "sleep")
            (->> (split instruction #" ")
                 (second)
                 (parse-int)
                 (* 1000)
                 (sleep))
            (if (or (starts-with? instruction "copy")
                    (starts-with? instruction "import"))
              (do
                (sleep 100)
                (gl-thread
                  (update-thing! [] #(run-instruction % instruction))))
              (update-thing! [] #(run-instruction % instruction))))
          (update-thing! [:instruction-index] inc))

        ;; wait for instruction to finish
        (while (or
                 (get-thing! [:animation])
                 (and @wait-chip-flag (any-chip-active? @world))
                 (:active @robot))
          nil)
        (reset! wait-chip-flag false)
        (robot-move [-100 0])
        (swap! history conj @world)

        (when @step
          (reset! replaying false)
          (reset! step false))

        ;; (if (.startsWith @last-instruction "mouse")
        ;;   (reset! replaying false))

        )

      (reset! replaying false))))

(reset! replaying false)

(defn toggle-run-instructions [world]
  (when (:replay-filename world)
    (.start
      (new Thread
           (proxy [Runnable] []
             (run []
               (run-instructions!))))))
  world)

(defn replay-forward [world]
  (if (not (:animation world))
    (do
      (reset! step true)
      (toggle-run-instructions world))
    world))

(defn replay-back [world]
  (if (and (:replay-filename world)
           (not (empty? @history)))
    (let [new-instructions (-> (str "replayer/" (:replay-filename world) ".txt")
                               (read-lines)
                               (extend-instructions))
          difference-index (find-if #(not= (nth (:instructions world) %)
                                           (nth new-instructions %))
                                    (range (min (count (:instructions world))
                                                (count new-instructions))))
          n (if (nil? difference-index)
              1
              (- (:instruction-index world) difference-index))
          n (if (neg? n)
              1
              n)]
      (swap! history #(vec (drop-last n %)))
      (-> (last @history)
          (load-instructions)
          (tree-changed)))
    world))

(defn replay-up [world]
  (assoc-in world [:replay-speed] 20))

(defn replay-down [world]
  (assoc-in world [:replay-speed] 1))
