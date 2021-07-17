
(load "replayer/compiler")
(load "replayer/robot")
(load "replayer/interpreter")
(load "replayer/extend")

(defn print-part-names [filename]
  (let [types ["sphere" "wagon" "block" "button" "gear" "chip"
               "motherboard" "cylinder" "probe" "cone" "lamp"
               "display" "rack" "speaker" "track" "gate" "connection"]
        get-type (fn [word]
                   (second (re-find #"([a-z]*).*?" (str word))))
        names (>> (str "res/" filename ".txt")
                (slurp)
                (str "[" . "]")
                (read-string)
                (flatten)
                (filter #(in? (get-type %) types) .)
                (distinct))]
    (println! "{")
    (doseq [name names]
      (if (.startsWith (str name) "gate")
        (println! (keyword name) "foobar")
        (println! (keyword name) (keyword (get-type name)))))
    (println! "}")))

(defn replay-draw [world]
  (when (:replay-filename world)
    (let [x (- (:window-width world) 10)
          y (- (:window-height world) 10 105)]
      (fill-rect! :black x y 20 20)
      (draw-text! :white "R" (- x 4) (+ y 5) 15))))

(defn load-instructions [world]
  (assoc-in world [:instructions]
    (-> (str "res/" (:replay-filename world) ".txt")
      (read-lines)
      (extend-instructions))))

(def checkpoint (atom nil))

(defn start-replay [world filename]
  (reset-robot!)
  (let [w (-> world
              (assoc-in [:replay-filename] filename)
              (assoc-in [:instruction-index] 0)
              (load-instructions))]
    (println! "-----")
    (reset! checkpoint w)
    (reset! replaying false)
    w))

(defn get-camera-view [world]
  (let [camera (:camera world)]
    [(:eye camera) (:pivot camera)]))

(defn skip-instruction? [world instruction]
  (if (or (.startsWith instruction ";;")
          (empty? (.trim instruction)))
    true
    (let [elements (read-string (str "[" instruction "]"))]
      (cond
        (.startsWith instruction "set camera")
        (let [[_ _ [eye pivot]] elements
              [eye2 pivot2] (get-camera-view world)]
          (and
            (vector= eye eye2)
            (vector= pivot pivot2)))

        (.startsWith instruction "set variable")
        (let [[_ _ key _ value] elements
              key (keyword key)
              value (keyword value)]
          (= (get-in world [key]) value))

        (.startsWith instruction "select motherboard")
        (let [[_ part-name _ tab-num] elements
              part-name (keyword part-name)]
          (= (get-in world [:parts part-name :tab]) tab-num))

        (.startsWith instruction "set view")
        (let [[_ _ _ chip-name _ end-view] elements
              chip-name (keyword chip-name)
              chip (get-in world [:parts chip-name])
              {:keys [zoom-x zoom-y offset]} (:view chip)
              [x y] offset]
          (vector= end-view [x y zoom-x zoom-y]))

        :else false))))

(defn run-next-instruction [world]
  (let [instructions (:instructions world)
        index (:instruction-index world)]
    (if (< index (count instructions))
      (let [instruction (nth instructions index)
            world (update-in world [:instruction-index] inc)]
        (cond
          (= instruction ">")
          (do
            (println! "checkpoint reached")
            (reset! replaying false)
            (reset! checkpoint world)
            world)

          (skip-instruction? world instruction)
          (recur world)

          :else
          (run-instruction world instruction)))
      world)))

(defn replay-forward [world]
  (if (and
        (:replay-filename world)
        (not (any-chip-active? world))
        (not (:active @robot)))
    (if (nil? (:animation world))
      (run-next-instruction world)
      (-> world
        (assoc-in [:animation :t] 1.0)
        (run-animation 16)
        (run-next-instruction)))
    world))

(defn replay-back [world]
  (if (:replay-filename world)
    (do
      (println! "restored checkpoint")
      (load-instructions @checkpoint))
    world))

(def replaying (atom false))

(defn run-instructions! []
  (if @replaying
    (reset! replaying false)
    (let [instructions (:instructions @world)
          delay 50]
      (reset! replaying true)

      (while (and
               @replaying
               (< (:instruction-index @world) (count instructions)))
        (update-thing! [] run-next-instruction)
        (redraw!)
        (sleep delay)
        (while (or
                 (get-thing! [:animation])
                 (any-chip-active? @world)
                 (:active @robot))
          nil))
      
      (reset! replaying false))))

(defn toggle-run-instructions [world]
  (when (:replay-filename world)
    (.start
      (new Thread
        (proxy [Runnable] []
          (run []
            (run-instructions!))))))
  world)

(defn change-event [event start-time]
  [(int (:x event)) (int (:y event)) (- (get-current-time) start-time)])

(def zoom-amount (atom 0))
(def zoom-time (atom 0))

(defn replay-zoomed [world event]
  (if (:replay-filename world)
    (if (:active @robot)
      world
      (let [current-time (get-current-time)
            zoom-elapsed (- current-time @zoom-time)]
        (when (> zoom-elapsed 2000)
          (reset! zoom-amount 0))
        (swap! zoom-amount #(+ % (:amount event)))
        (reset! zoom-time current-time)
        world))
    world))

(defn replay-pressed [world event]
  (if (:replay-filename world)
    (if (:active @robot)
      world
      (-> world
          (assoc-in [:replay-button] (dekeyword (:button event)))
          (assoc-in [:replay-events]
            [(change-event event (:press-time world))])
          (assoc-in [:start-camera] (get-camera-view world))))
    world))

(defn replay-moved [world event]
  (if (:replay-filename world)
    (if (not-nil? (:replay-events world))
      (update-in world [:replay-events]
                 #(conj % (change-event event (:press-time world))))
      world)
    world))

(defn replay-released [world event]
  (if (:replay-filename world)
    (if (:active @robot)
      world
      (let [points (conj (:replay-events world)
                         (change-event event (:press-time world)))
            button (:replay-button world)
            view (:start-camera world)
            instruction (str "mouse " view " "
                          button " " (join " " points))]
        (dissoc-in world [:replay-events])))
    world))
