
(defn update-history [world]
  (update-in world [:replay-history]
             #(conj % {:parts (:parts world)
                       :camera (:camera world)})))

(defn get-part-type [part-name]
  (keyword (second (re-find #":([a-z]*)" (str part-name)))))

(defn run-add-instruction [world instruction]
  (let [[_ part-name
         _ parent-name
         _ position rotation] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        parent-name (keyword parent-name)
        type (get-part-type part-name)
        layer (apply min (:visible-layers world))
        color (get-in world [:info type :color])
        part (create-part type color layer (:info world))
        transform (make-transform position rotation)
        prepare-wagon (fn [w]
                        (if (= type :wagon)
                          (set-wagon-loop w part-name parent-name)
                          w))]
    (-> world                           
      (assoc-in [:parts part-name] part)
      (assoc-in [:parts parent-name :children part-name] transform)
      (prepare-wagon)
      (compute-transforms :parts)
      (update-history)
      (tree-changed))))

;; (defn run-set-instruction [world instruction]
;;   (let [[_ property-name
;;          _ part-name
;;          _ value] (read-string (str "[" instruction "]"))
;;         property-name (keyword property-name)
;;         part-name (keyword part-name)
;;         value (if (symbol? value) (keyword value) value)]
;;     (-> world
;;         (set-part-value part-name property-name (str value))
;;         (update-history)
;;         (tree-changed))))

(defn scale-animation [world animation]
  (let [{:keys [t start-scale final-scale
                start-transform final-transform
                part-name parent-name]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (-> world
          (assoc-in [:parts part-name :scale] final-scale)
          (assoc-in [:parts part-name :transform] final-transform)
          (create-relative-transform part-name parent-name)
          (update-history)
          (tree-changed))

      :else
      (let [scale (vector-interpolate start-scale final-scale t)
            transform (interpolate-transforms start-transform
                                              final-transform t)]
        (-> world
            (assoc-in [:parts part-name :scale] scale)
            (assoc-in [:parts part-name :transform] transform))))))

(defn run-scale-instruction [world instruction]
  (let [[_ part-name
         _ change] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        offset (if (= (:type part) :track)
                 change
                 (vector-multiply change 0.5))
        rotation (get-rotation-component (:transform part))
        offset (apply-transform rotation offset)
        offset-transform (make-transform offset [1 0 0 0])
        transform (:transform part)
        final-transform (combine-transforms transform offset-transform)]
    (assoc-in world [:animation]
      {:t 0.0
       :time 0.3
       :fn scale-animation
       :part-name part-name
       :parent-name parent-name
       :start-scale (:scale part)
       :final-scale (vector-add (:scale part) change)
       :start-transform transform
       :final-transform final-transform})))

(defn run-set-color-instruction [world instruction]
  (let [[_ _ _ part-name _ color] (read-string (str "[" instruction "]"))
        color (keyword color)
        part-name (keyword part-name)]
    (-> world
      (assoc-in [:parts part-name :color] color)
      (update-history)
      (tree-changed))))

(defn value-animation [world animation]
  (let [{:keys [t part-name final-value]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (-> world
        (assoc-in [:parts part-name :value] final-value)
        (update-history)
        (tree-changed)
        (redraw))

      :else
      (-> world
        (assoc-in [:parts part-name :value] (* t final-value))
        (redraw)))))

(defn run-set-value-instruction [world instruction]
  (let [[_ _ _ part-name _ value] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        part (get-in world [:parts part-name])
        value (if (= (:type part) :wagon)
                (/ value (reduce + (:track-lengths part)))
                value)
        time (if (= (:type part) :wagon)
               (abs value)
               (* 2 (abs value)))]
    (assoc-in world [:animation]
      {:t 0.0
       :time time
       :fn value-animation
       :part-name part-name
       :final-value value})))

;; (defn interpolate-mouse [start end]
;;   (let [ts (last start)
;;         te (last end)
;;         start (take 2 start)
;;         end (take 2 end)
;;         d (distance start end)
;;         v (vector-subtract end start)
;;         interval (- te ts)
;;         dt 20.0
;;         num-steps (int (/ interval dt))
;;         extra-time (int (mod interval dt))]
;;     (robot-move start)
;;     (dotimes [i (dec num-steps)]
;;       (sleep dt)
;;       (robot-move (vector-add start (vector-multiply v (/ i num-steps)))))
;;     (sleep extra-time)
;;     (robot-move end)))

;; (defn run-mouse-instruction [world instruction]
;;   (let [[_ button & points] (read-string (str "(" instruction ")"))
;;         button (keyword button)]

;;     (.start
;;      (new Thread
;;           (proxy [Runnable] []
;;             (run []
;;               (robot-set-active! true)
;;               (robot-move (take 2 (first points)))
;;               (robot-mouse-press button)
;;               (dotimes [i (dec (count points))]
;;                 (interpolate-mouse (nth points i)
;;                                    (nth points (inc i))))
;;               (sleep 16)
;;               (robot-mouse-release button)
;;               (sleep 100) ;;##########################
;;               (robot-set-active! false)
;;               ))))
;;     world))

(defn run-sleep-instruction [world instruction]
  (sleep (parse-int (subs instruction 6)))
  world)

(defn run-set-variable-instruction [world instruction]
  (let [[_ _ key _ value] (read-string (str "(" instruction ")"))
        key (keyword key)
        value (if (symbol value)
                (keyword value)
                value)]
    (-> world
      (assoc-in [key] value)
      (update-history))))

(defn run-put-instruction [world instruction]
  (let [[_ part-name _ chip-name] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        chip-name (keyword chip-name)
        chip (get-in world [:parts chip-name])
        part (get-in world [:parts part-name])]
    (-> world
      (assoc-in [:parts chip-name :functions part-name]
        {:points [[0 0] [1 1]]
         :relative false
         :z (inc (get-max-z chip))})
      (update-history)
      (tree-changed))))

(defn run-add-point-instruction [world instruction]
  (let [[_ _ point _ _ part-name _ chip-name] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        chip-name (keyword chip-name)]
    (-> world
      (update-in [:parts chip-name :functions part-name]
        (fn [function]
          (-> function
            (update-in [:points] #(conj % point))
            (normalize-function))))
      (update-history))))

(defn run-move-point-instruction [world instruction]
  (let [[_ _ n _ _ part-name _ chip-name _ point] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        chip-name (keyword chip-name)]
    (-> world
      (update-in [:parts chip-name :functions part-name]
        (fn [function]
          (-> function
            (assoc-in [:points n] point)
            (normalize-function))))
      (update-history))))

(defn get-bounding-viewbox [points box]
  (let [xs (map first points)
        ys (map second points)
        x1 (first xs)
        x2 (last xs)
        y1 (reduce min ys)
        y2 (reduce max ys)
        zoom-x (/ 1.0 (- x2 x1))
        zoom-y (/ 1.0 (- y2 y1))
        x (* x1 zoom-x -1)
        y (* y1 zoom-y -1)]
    {:offset [x y]
     :zoom-x zoom-x
     :zoom-y zoom-y}))

(defn function-zoom-animation [world animation]
  (let [{:keys [t chip-name start-view end-view]} animation]
    (cond
      (float= t 0.0)
      (do
        (println! "started")
        world)

      (float= t 1.0)
      (-> world
          (assoc-in [:parts chip-name :view] end-view)
          (redraw))

      :else
      (let [start-zoom [(:zoom-x start-view)
                        (:zoom-y start-view)]
            end-zoom [(:zoom-x end-view)
                      (:zoom-y end-view)]
            zoom (vector-interpolate start-zoom end-zoom t)
            offset (vector-interpolate
                     (:offset start-view) (:offset end-view) t)]
        (-> world
          (assoc-in [:parts chip-name :view]
            {:zoom-x (first zoom)
             :zoom-y (second zoom)
             :offset offset})
          (redraw))))))

(defn run-zoom-instruction [world instruction]
  (let [[_ _ chip-name _ part-name] (read-string (str "[" instruction "]"))
        chip-name (keyword chip-name)
        part-name (keyword part-name)
        chip (get-in world [:parts chip-name])
        points (get-in chip [:functions part-name :points])
        start-view (:view chip)
        end-view (get-bounding-viewbox points (:graph-box world))]
    (assoc-in world [:animation]
      {:t 0.0
       :time 1
       :fn function-zoom-animation
       :chip-name chip-name
       :start-view start-view
       :end-view end-view})))

(defn run-instruction [world instruction]
  (let [words (split instruction #" ")
        instruction-name (cond
                           (= (second words) "point")
                           (str (first words) "-point")
                           
                           (= (first words) "set")
                           (str (first words) "-" (second words))

                           :else
                           (first words))]
    (if-let [function (-> (str "temp.core/run-"
                               instruction-name
                               "-instruction")
                          (symbol)
                          (resolve))]
      (do
        (println! (subs instruction 0 (min 100 (count instruction))))
        (function world instruction)
        (redraw))
      (do
        (println! "invalid instruction")
        world))))
