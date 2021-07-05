
(load "replayer/compiler")
(load "replayer/interpreter")
(load "replayer/robot")
(load "replayer/extend")
;; (load "replayer/mouse")
;; (load "replayer/path")

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
            (assoc-in [:instructions]
              (-> (str "res/" text ".txt")
                (read-lines)
                (extend-instructions)))
            (update-history)))))))

(defn replayer-restart [world]
  (let [new-history (vec (take 1 (:replay-history world)))]
    (-> world
      (assoc-in [:mode] :simulation)
      (assoc-in [:add-type] :block)
      (assoc-in [:edit-subcommand] :move)
      (assoc-in [:selected-part] nil)
      (assoc-in [:selected-chip] nil)
      (assoc-in [:current-color] :red)
      (assoc-in [:parts] (:parts (last new-history)))
      (assoc-in [:camera] (:camera (last new-history)))
      (compute-camera)
      (assoc-in [:replay-history] [])
      (assoc-in [:instruction-index] 0)
      (redraw)
      (tree-changed))))

(defn skip-instruction? [world instruction]
  (cond
    (.startsWith instruction "set variable")
    (let [[_ _ key _ value] (read-string (str "[" instruction "]"))
          key (keyword key)
          value (keyword value)]
      (= (get-in world [key]) value))

    (.startsWith instruction "select motherboard")
    (let [[_ part-name _ tab-num] (read-string (str "[" instruction "]"))
          part-name (keyword part-name)]
      (= (get-in world [:parts part-name :tab]) tab-num))

    :else false))

(defn run-next-instruction [world]
  (let [instructions (:instructions world)
        index (:instruction-index world)]
    (if (< index (count instructions))
      (let [instruction (nth instructions index)
            world (update-in world [:instruction-index] inc)]
        (if (skip-instruction? world instruction)
          (do
            ;; (println! "#### skipped" instruction)
            (recur world))
          (run-instruction world instruction)))
      world)))

(defn replay-forward [world]
  (if (and
        (:replay-filename world)
        (nil? (:animation world))
        (not (any-chip-active? world)))
    (run-next-instruction world)
    world))

(defn replay-back [world]
  ;; (if (> (count (:replay-history world)) 1)
  (let [new-history (pop (:replay-history world))]
    ;; (println! "<<" (subs instruction 0 (min 100 (count instruction))))
    (-> world
      (assoc-in [:parts] (:parts (last new-history)))
      (assoc-in [:camera] (:camera (last new-history)))
      (compute-camera)
      (assoc-in [:replay-history] new-history)
      ;; (update-in [:instruction-index] dec)
      (tree-changed))))

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
        (while (or
                 (get-thing! [:animation])
                 (any-chip-active? @world)))
        (redraw!)
        (sleep delay))
      
      (reset! replaying false))))

(defn toggle-run-instructions [world]
  (.start
    (new Thread
      (proxy [Runnable] []
        (run []
          (run-instructions!)))))
  world)
