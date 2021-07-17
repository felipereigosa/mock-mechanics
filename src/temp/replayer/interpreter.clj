
(defn get-part-type [part-name]
  (keyword (second (re-find #":([a-z]*)" (str part-name)))))

(defn print-part-relative-location [world part-name]
  (let [part (get-in world [:parts part-name])
        parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        point (get-part-position world part-name)
        transform (:transform part)
        vy (apply-rotation transform [0 1 0])
        plane (case (:type parent)
                :ground [[0.25 0 0.25] [1.25 0 0.25] [0.25 0 1.25]]
                :track (get-track-plane parent)
                (get-block-plane parent vy))
        surface-point (point-plane-projection point plane)
        v (vector-subtract surface-point (first plane))
        [a b c] plane
        v1 (vector-subtract b a)
        v2 (vector-subtract c a)
        ox (vector-scalar-projection v v1)
        oy (vector-scalar-projection v v2)]
    (user-message! "x = " (format "%.2f" ox)
                   ", y = " (format "%.2f" oy))
    world))

(defn run-add-instruction [world instruction]
  (let [[_ _ part-name _ parent-name _ position rotation] instruction
        part-name (keyword part-name)
        parent-name (keyword parent-name)
        type (get-part-type part-name)
        layer (apply min (:visible-layers world))
        color (get-in world [:info type :color])
        part (create-part type color layer (:info world))
        transform (make-transform position rotation)
        prepare-wagon (fn [w]
                        (if (= type :wagon)
                          (let [w (set-wagon-loop w part-name parent-name)
                                value (->> (get-in w [:parts part-name :track-lengths])
                                        (reduce +)
                                        (/ (last instruction)))]
                            (assoc-in w [:parts part-name :value] value))
                          w))]
    (-> world
      (assoc-in [:parts part-name] part)
      (assoc-in [:parts parent-name :children part-name] transform)
      (prepare-wagon)
      (compute-transforms :parts)
      (tree-changed)
      (print-part-relative-location part-name))))

;; (defn run-set-instruction [world instruction]
;;   (let [[_ property-name _ part-name _ value] instruction
;;         property-name (keyword property-name)
;;         part-name (keyword part-name)
;;         value (if (symbol? value) (keyword value) value)]
;;     (-> world
;;         (set-part-value part-name property-name (str value))
;;         (tree-changed))))

(defn print-scale-change [world part-name scale change]
  (let [type (get-in world [:parts part-name :type])
        index (second (find-if #(not (float= (first %) 0.0))
                               (map vector change (range 3))))
        value (float (nth scale index))]
    (case type
      :block
      (user-message! (format "side: %.2f" value))

      (:cylinder :cone)
      (if (= index 0)
        (user-message! (format "diameter: %.2f" value))
        (user-message! (format "height: %.2f" value)))

      :track
      (user-message! (format "length: %.2f" value))

      :sphere
      (user-message! (format "diameter: %.2f" value)))
    world))

(defn scale-animation [world animation]
  (let [{:keys [t start-scale final-scale
                start-transform final-transform
                part-name parent-name change]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (-> world
          (assoc-in [:parts part-name :scale] final-scale)
          (assoc-in [:parts part-name :transform] final-transform)
          (create-relative-transform part-name parent-name)
          (tree-changed)
          (print-scale-change part-name final-scale change))

      :else
      (let [scale (vector-interpolate start-scale final-scale t)
            transform (interpolate-transforms start-transform
                                              final-transform t)]
        (-> world
            (assoc-in [:parts part-name :scale] scale)
            (assoc-in [:parts part-name :transform] transform)
            (print-scale-change part-name scale change)))

(defn run-scale-instruction [world instruction]
  (let [[_  part-name _ direction change] instruction
        part-name (keyword part-name)
        parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        offset (cond
                 (= (:type part) :track)
                 change

                 (and
                   (in? (:type part) [:cylinder :cone])
                   (float= (second change) 0.0))
                 [0 0 0]

                 (= (:type part) :sphere)
                 [0 0 0]

                 :else
                 (vector-multiply change 0.5))

        offset (if (= direction '-)
                 (vector-multiply offset -1)
                 offset)
        rotation (get-rotation-component (:transform part))
        offset (apply-transform rotation offset)
        offset-transform (make-transform offset [1 0 0 0])
        transform (:transform part)
        final-transform (combine-transforms transform offset-transform)]
    (assoc-in world [:animation]
      {:t 0.0
       :time 0.3;;(/ (reduce + (map abs change)) 2)
       :fn scale-animation
       :change change
       :part-name part-name
       :parent-name parent-name
       :start-scale (:scale part)
       :final-scale (vector-add (:scale part) change)
       :start-transform transform
       :final-transform final-transform})))

(defn sink-animation [world animation]
  (let [{:keys [t part-name parent-name
                start-transform final-transform
                amount]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (do
        (user-message! "height = " (format "%.2f" amount))
        (-> world
            (assoc-in [:parts part-name :transform] final-transform)
            (create-relative-transform part-name parent-name)
            (tree-changed)))

      :else
      (let [transform (interpolate-transforms start-transform
                                              final-transform t)
            h (* amount t)]
        (user-message! "height = " (format "%.2f" h))
        (assoc-in world [:parts part-name :transform] transform)))))

(defn run-sink-instruction [world instruction]
  (let [[_  part-name _ amount] instruction
        part-name (keyword part-name)
        parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        rotation (get-rotation-component (:transform part))
        offset (apply-transform rotation [0 amount 0])
        offset-transform (make-transform offset [1 0 0 0])
        transform (:transform part)
        final-transform (combine-transforms transform offset-transform)]
    (assoc-in world [:animation]
      {:t 0.0
       :time (abs amount)
       :fn sink-animation
       :part-name part-name
       :parent-name parent-name
       :start-transform transform
       :final-transform final-transform
       :amount amount})))

(defn rotate-animation [world animation]
  (let [{:keys [t part-name parent-name
                start-transform angle]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (let [rotation-transform (make-transform [0 0 0] [0 1 0 angle])
            transform (combine-transforms rotation-transform start-transform)]
        (user-message! "angle = " (format "%.2f" (float angle)))
        (-> world
          (assoc-in [:parts part-name :transform] transform)
          (create-relative-transform part-name parent-name)
          (tree-changed)))

      :else
      (let [rotation-transform (make-transform [0 0 0] [0 1 0 (* t angle)])
            transform (combine-transforms rotation-transform start-transform)]
        (user-message! "angle = " (format "%.2f" (* t angle)))
        (assoc-in world [:parts part-name :transform] transform)))))

(defn run-rotate-instruction [world instruction]
  (let [[_  part-name _ amount] instruction
        part-name (keyword part-name)
        parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        part (get-in world [:parts part-name])
        transform (:transform part)]
    (assoc-in world [:animation]
      {:t 0.0
       :time (/ amount 100)
       :fn rotate-animation
       :part-name part-name
       :parent-name parent-name
       :start-transform transform
       :angle amount})))

(defn move-animation [world animation]
  (let [{:keys [t part-name parent-name
                start-transform final-transform]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (-> world
          (assoc-in [:parts part-name :transform] final-transform)
          (create-relative-transform part-name parent-name)
          (tree-changed)
          (print-part-relative-location part-name))

      :else
      (let [transform (interpolate-transforms start-transform
                                              final-transform t)]
        (-> world
            (assoc-in [:parts part-name :transform] transform)
            (print-part-relative-location part-name))))))

(defn run-move-instruction [world instruction]
  (let [[_  part-name _ location] instruction
        part-name (keyword part-name)
        parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        relative-transform (get-in parent [:children part-name])
        relative-rotation (get-transform-rotation relative-transform)
        new-transform (make-transform location relative-rotation)
        w2 (-> world
             (assoc-in [:parts parent-name
                        :children part-name] new-transform)
             (compute-transforms :parts))
        start-transform (get-in world [:parts part-name :transform])
        final-transform (get-in w2 [:parts part-name :transform])
        p1 (get-transform-position start-transform)
        p2 (get-transform-position final-transform)]
    (assoc-in world [:animation]
      {:t 0.0
       :time (/ (distance p1 p2) 2)
       :fn move-animation
       :part-name part-name
       :parent-name parent-name
       :start-transform start-transform
       :final-transform final-transform})))

(defn run-set-color-instruction [world instruction]
  (let [[_ _ _ part-name _ color] instruction
        color (keyword color)
        part-name (keyword part-name)
        world (assoc-in world [:parts part-name :color] color)
        part (get-in world [:parts part-name])
        world (if (= (:type part) :lamp)
                (assoc-in world [:parts part-name :dark-color]
                  (get-dark-color color))
                world)]
    (tree-changed world)))

(defn value-animation [world animation]
  (let [{:keys [t part-name start-value final-value]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (-> world
        (assoc-in [:parts part-name :value] final-value)
        (tree-changed)
        (redraw))

      :else
      (-> world
        (assoc-in [:parts part-name :value]
          (interpolate-values start-value final-value t))
        (redraw)))))

(defn run-set-value-instruction [world instruction]
  (let [[_ _ _ part-name _ value] instruction
        part-name (keyword part-name)
        part (get-in world [:parts part-name])
        start-value (:value part)
        length (if (= (:type part) :wagon)
                 (reduce + (:track-lengths part))
                 1)
        final-value (/ value length)
        s-value (* (:value part) length)
        time (abs (- s-value value))

        time (if (= (:type part) :wagon)
               time
               (* time 3))]
    (assoc-in world [:animation]
      {:t 0.0
       :time time
       :fn value-animation
       :part-name part-name
       :start-value start-value
       :final-value final-value})))

(defn interpolate-mouse [start end]
  (let [ts (last start)
        te (last end)
        start (take 2 start)
        end (take 2 end)
        d (distance start end)
        v (vector-subtract end start)
        interval (- te ts)
        dt 20.0
        num-steps (int (/ interval dt))
        extra-time (int (mod interval dt))]
    (robot-move start)
    (dotimes [i (dec num-steps)]
      (sleep dt)
      (robot-move (vector-add start (vector-multiply v (/ i num-steps)))))
    (sleep extra-time)
    (robot-move end)))

(defn run-mouse-instruction [world instruction]
  (robot-set-active! true)
  (let [[_ button & points] instruction
        button (keyword button)]
    (.start
     (new Thread
          (proxy [Runnable] []
            (run []
              (robot-move (take 2 (first points)))
              (robot-mouse-press button)
              (dotimes [i (dec (count points))]
                (interpolate-mouse (nth points i)
                                   (nth points (inc i))))
              (sleep 16)
              (robot-mouse-release button)
              (sleep 16)
              (robot-set-active! false)))))
    world))

(defn run-zoom-instruction [world instruction]
  (robot-set-active! true)
  (let [[_ x y amount] instruction]
    (.start
     (new Thread
          (proxy [Runnable] []
            (run []
              (robot-move [x y])

              (dotimes [i (abs amount)]
                (if (pos? amount)
                  (robot-scroll 1)
                  (robot-scroll -1))
                (sleep 30))

              (robot-set-active! false)))))
    world))

(defn run-set-variable-instruction [world instruction]
  (let [[_ _ key _ value] instruction
        key (keyword key)
        value (if (symbol value)
                (keyword value)
                value)
        world (if (in? key [:selected-part
                            :selected-chip
                            :selected-motherboard])
                (select-part world value)
                world)]
    (assoc-in world [key] value)))

(declare replaying)

(defn run-put-instruction [world instruction]
  (let [[_ part-name _ chip-name] instruction
        part-name (keyword part-name)
        chip-name (keyword chip-name)
        chip (get-in world [:parts chip-name])
        part (get-in world [:parts part-name])]
    (if (nil? part)
      (do
        (println! "part" part-name "doesn't exist")
        (reset! replaying false)
        world)
      (-> world
        (select-part part-name)
        (assoc-in [:parts chip-name :functions part-name]
          {:points [[0 0] [1 1]]
           :relative false
           :z (inc (get-max-z chip))})
        (tree-changed)))))

(defn move-point-animation [world animation]
  (let [{:keys [t start-point end-point
                chip-name part-name n]} animation]
    (cond
      (float= t 0.0) world

      (float= t 1.0)
      (do
        (user-message! (apply format "node value = %.2f, %.2f" (map float end-point)))
        (-> world
            (update-in [:parts chip-name :functions part-name]
                       (fn [function]
                         (assoc-in function [:points n] end-point)))
            (redraw)))

      :else
      (let [point (vector-interpolate start-point end-point t)]
        (user-message! (apply format "node value = %.2f, %.2f" (map float point)))
        (-> world
            (update-in [:parts chip-name :functions part-name]
                       (fn [function]
                         (assoc-in function [:points n] point)))
            (redraw))))))

(defn run-add-point-instruction [world instruction]
  (let [[_ _ point _ _ part-name _ chip-name] instruction
        part-name (keyword part-name)
        chip-name (keyword chip-name)
        points (get-in world [:parts chip-name :functions
                              part-name :points])
        [before after] (split-with #(< (first %) (first point)) points)
        middle (vector-interpolate (last before) (first after) 0.5)
        new-function (vec (concat before [middle] after))]
    (-> world
        (assoc-in [:parts chip-name :functions
                   part-name :points] new-function)
        (assoc-in [:animation]
                  {:t 0.0
                   :time 0.5
                   :fn move-point-animation
                   :chip-name chip-name
                   :part-name part-name
                   :n (count before)
                   :start-point middle
                   :end-point point}))))

(defn run-move-point-instruction [world instruction]
  (let [[_ _ n _ _ part-name _ chip-name _ point] instruction
        part-name (keyword part-name)
        chip-name (keyword chip-name)
        start-point (if (= n 0)
                    [0 0]
                    [1 1])]
    (assoc-in world [:animation]
      {:t 0.0
       :time 0.5
       :fn move-point-animation
       :chip-name chip-name
       :part-name part-name
       :n n
       :start-point start-point
       :end-point point})))

(defn function-zoom-animation [world animation]
  (let [{:keys [t chip-name start-view end-view]} animation]
    (cond
      (float= t 0.0) world

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

(defn run-set-view-instruction [world instruction]
  (let [[_ _ _ chip-name _ view] instruction
        [ox oy zx zy] view
        chip-name (keyword chip-name)
        chip (get-in world [:parts chip-name])
        start-view (:view chip)
        end-view  {:zoom-x zx
                   :zoom-y zy
                   :offset [ox oy]}]
    (assoc-in world [:animation]
      {:t 0.0
       :time 0.5
       :fn function-zoom-animation
       :chip-name chip-name
       :start-view start-view
       :end-view end-view})))

(defn run-activate-instruction [world instruction]
  (let [chip-name (keyword (second instruction))]
    (activate-chip world chip-name)))

(defn run-select-instruction [world instruction]
  (let [[_ motherboard-name _ tab-num] instruction
        motherboard-name (keyword motherboard-name)]
    (assoc-in world [:parts motherboard-name :tab] tab-num)))

(defn run-set-pin-instruction [world instruction]
  (let [[_ motherboard-name _ pin] instruction
        motherboard-name (keyword motherboard-name)
        [part-name x] pin
        part-name (keyword part-name)
        part (get-in world [:parts part-name])]
    (if (nil? part)
      (do
        (println! "part" part-name "doesn't exist")
        (reset! replaying false)
        world)
      (-> world
        (select-part part-name)
        (assoc-in [:parts motherboard-name :pins part-name]
          {:x x
           :trigger false
           :value (:value part)})))))

(defn run-toggle-instruction [world instruction]
  (let [[_ selected-name _ part-name] instruction
        selected-name (keyword selected-name)
        part-name (keyword part-name)
        selected (get-in world [:parts selected-name])]
    (if (= (:type selected) :chip)
      (assoc-in world [:parts selected-name :functions
                       part-name :relative] true)
      (assoc-in world [:parts selected-name
                       :pins part-name :trigger] true))))

(defn run-set-gate-instruction [world instruction]
  (let [[_ motherboard-name _ gate] instruction
        motherboard-name (keyword motherboard-name)
        tab (get-in world [:parts motherboard-name :tab])
        [gate-name x y] gate
        type (->> gate-name
               (str)
               (re-find #"([a-z]*)[0-9]*")
               (second)
               (keyword))
        gate-name (keyword (str "gate-" gate-name))]
    (assoc-in world [:parts motherboard-name :gates gate-name]
      {:type type
       :x x
       :y y
       :tab tab})))

(defn run-set-connection-instruction [world instruction]
  (let [[_ motherboard-name _ connection-name points] instruction
        motherboard-name (keyword motherboard-name)
        connection-name (keyword connection-name)
        points (vec (map #(if (symbol? %)
                            (keyword %)
                            %)
                      points))
        tab (get-in world [:parts motherboard-name :tab])]
    (assoc-in world [:parts motherboard-name :connections connection-name]
      {:points points
       :tab tab})))

(defn run-add-connection-point-instruction [world instruction]
  (let [[_ motherboard-name connection-name _ point] instruction
        motherboard-name (keyword motherboard-name)
        connection-name (keyword connection-name)
        point (if (symbol? point)
                (keyword point)
                point)]
    (update-in world [:parts motherboard-name
                      :connections connection-name :points]
      #(conj % point))))

(defn run-set-camera-instruction [world instruction]
  (let [[_ _ [eye pivot]] instruction]
    (-> world
      (assoc-in [:view-matrix] (get-look-at-matrix eye pivot [0 1 0]))
      (assoc-in [:camera :eye] eye)
      (reverse-compute-camera))))

(defn run-instruction [world instruction]
  (let [words (split instruction #" ")
        instruction-name (cond
                           (= (second words) "point")
                           (str (first words) "-point")

                           (.startsWith instruction "set motherboard")
                           (str (first words) "-" (third words))

                           (= (first words) "set")
                           (str (first words) "-" (second words))

                           (and
                             (= (first words) "add")
                             (= (fourth words) "point"))
                           "add-connection-point"

                           :else
                           (first words))]
    (if-let [function (-> (str "temp.core/run-"
                               instruction-name
                               "-instruction")
                          (symbol)
                          (resolve))]
      (let [short-instruction (apply str (take 80 instruction))]
        (if (= (count short-instruction)
              (count instruction))
          (println instruction)
          (println short-instruction "..."))
        (-> world
          (function (read-string (str "[" instruction "]")))
          (redraw)))
      (do
        (println! "invalid instruction")
        world))))
