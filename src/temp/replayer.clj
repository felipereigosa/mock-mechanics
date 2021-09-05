
(load "replayer/compiler")
(load "replayer/robot")
(load "replayer/interpreter")
(load "replayer/extend")

(defn get-camera-vector [world]
  (let [camera (:camera world)]
    [(:pivot camera)
     [(:x-angle camera) (:y-angle camera)]
     (:distance camera)]))

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

(def legend (atom nil))

(defn replay-draw [world]
  (when (:replay-filename world)
    (let [x (- (:window-width world) 10)
          y (- (:window-height world) 10 105)]

      (if-let [{:keys [text x y size]} @legend]
        (draw-text! :white text x y size))

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
              (load-instructions))
        w (if (some #(starts-with? % ">") (:instructions w))
            (assoc-in w [:replay-speed] 20.0)
            (assoc-in w [:replay-speed] 1.0))]
    (println "-----")
    (reset! checkpoint w)
    (reset! replaying false)
    w))

(defn skip-instruction? [world instruction]
  (if (or (.startsWith instruction ";;")
          (empty? (.trim instruction)))
    true
    (let [elements (read-string (str "[" instruction "]"))]
      (cond
        (.startsWith instruction "set camera")
        (let [[_ _ _ pivot _ angles _ distance] elements
              [pivot2 angles2 distance2] (get-camera-vector world)]
          (and
           (vector= pivot pivot2)
           (vector= angles angles2)
           (float= distance distance2)))

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
            (set-thing! [:replay-speed] 1.0)
            (reset! checkpoint world)
            world)

          (skip-instruction? world instruction)
          (recur world)

          :else
          (run-instruction world instruction)))
      (do
        (println "no more instructions")
        world))))

(defn replay-forward [world]
  (if (and
        (:replay-filename world)
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
      (-> @checkpoint
          (load-instructions)
          (tree-changed)))
    world))

(declare run-instructions!)

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
    (if @replaying
      world
      (let [current-time (get-current-time)
            zoom-elapsed (- current-time @zoom-time)]
        (when (> zoom-elapsed 2000)
          (reset! zoom-amount 0))
        (swap! zoom-amount #(+ % (:amount event)))
        (reset! zoom-time current-time)
        (println! "zoom" (:x event) (:y event) @zoom-amount)
        world))
    world))

(defn replay-pressed [world event]
  (if (:replay-filename world)
    (if @replaying
      world
      (-> world
          (assoc-in [:replay-button] (dekeyword (:button event)))
          (assoc-in [:replay-events]
            [(change-event event (:press-time world))])
          (assoc-in [:start-camera] (get-camera-vector world))))
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
    (if @replaying
      world
      (let [points (conj (:replay-events world)
                         (change-event event (:press-time world)))
            button (:replay-button world)
            camera (:start-camera world)
            instruction (format "mouse %s true %s %s"
                                camera button (join " " points))]
        (println! instruction)
        (dissoc-in world [:replay-events])))
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

        ;; big or small delay depending on current and last instructions
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
              (sleep (int (/ 800 (:replay-speed @world)))))))

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
            (update-thing! [] #(run-instruction % instruction)))
          (update-thing! [:instruction-index] inc))

        ;; wait for instruction to finish
        (while (or
                (get-thing! [:animation])
                (and @wait-chip-flag (any-chip-active? @world))
                (:active @robot))
          nil)
        (reset! wait-chip-flag false)

        (robot-move [-100 0])
        (do-later stop-sound! 300)

        (when (and
               (< (:instruction-index @world) (count instructions))
               (= (nth instructions (:instruction-index @world)) ">"))
          (println! "checkpoint reached")
          (set-thing! [:replay-speed] 1.0)
          (update-thing! [:instruction-index] inc)
          (reset! checkpoint @world)
          (reset! replaying false))
        )

      (reset! replaying false))))
