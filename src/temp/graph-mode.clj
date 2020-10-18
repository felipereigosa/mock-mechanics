(ns temp.core (:gen-class))

(defn local->global [graph-box view [t v]]
  (let [{:keys [x y w h]} graph-box
        [to vo] (:offset view)
        zoom-x (:zoom-x view)
        zoom-y (:zoom-y view)
        t (+ (* t zoom-x) to)
        v (+ (* v zoom-y) vo)]
    [(map-between-ranges t 0 1 7 (- w 7))
     (map-between-ranges v 0 1 (- h 7) 7)]))

(defn global->local [graph-box view [px py]]
  (let [{:keys [x y w h]} graph-box
        hw (* w 0.5)
        hh (* h 0.5)
        x1 (- x hw)
        x2 (+ x hw)
        y1 (- y hh)
        y2 (+ y hh)
        [to vo] (:offset view)
        zoom-x (:zoom-x view)
        zoom-y (:zoom-y view)
        t (map-between-ranges px (+ x1 7) (- x2 7) 0 1)
        v (map-between-ranges py (- y2 7) (+ y1 7) 0 1)]
    [(/ (- t to) zoom-x)
     (/ (- v vo) zoom-y)]))

(defn draw-graph-cross [graph-box]
  (let [buffer (:buffer graph-box)
        {:keys [x y w h]} graph-box
        x1 7
        x2 (- w 7)
        y1 7
        y2 (- h 7)]
    (draw-line buffer :dark-gray x1 y1 x2 y2)
    (draw-line buffer :dark-gray x1 y2 x2 y1)))

(defn draw-normal-function! [graph-box view points color]
  (let [{:keys [x y w h]} graph-box]
    (doseq [i (range 1 (count points))]
      (let [buffer (:buffer graph-box)
            [x1 y1] (local->global
                     graph-box view (nth points (dec i)))
            [x2 y2] (local->global
                     graph-box view (nth points i))]
        (draw-line buffer color x1 y1 x2 y2)
        (fill-circle buffer color x2 y2 7)

        (if (= i 1)
          (fill-circle buffer color x1 y1 7))))))

(defn draw-square-function! [graph-box view points color]
  (let [{:keys [x y w h]} graph-box]
    (doseq [i (range 1 (count points))]
      (let [buffer (:buffer graph-box)
            p1 (nth points (dec i))
            p2 (nth points i)
            h1 (second p1)
            h2 (second p2)
            [x1 y1] (local->global graph-box view p1)
            [x2 y2] (local->global graph-box view p2)]
        (draw-line buffer color x1 y1 x2 y1)
        (fill-rect buffer color x2 y2 12 12)
        (draw-line buffer color x2 y1 x2 y2)
        (if (= i 1)
          (fill-rect buffer color x1 y1 12 12))))))

(defn draw-function! [graph-box view function color]
  (if (:relative function)
    (draw-square-function! graph-box view (:points function) color)
    (draw-normal-function! graph-box view (:points function) color)))

(defn draw-grid [graph-box view]
  (let [buffer (:buffer graph-box)
        [x y] (local->global graph-box view [0 0])
        [a b] (local->global graph-box view [1 1])]
    (doseq [i (range -10 10)]
      (let [dx (- a x)
            dy (- b y)
            xi (+ x (* i dx))
            yi (+ y (* i dy))]
        (draw-line buffer :dark-gray (- x 1500) yi (+ x 1500) yi)
        (draw-text buffer :dark-gray (str i) (- x 15) (- yi 6) 12)
        (draw-line buffer :dark-gray xi (- y 1500) xi (+ y 1500))
        (draw-text buffer :dark-gray (str i) (+ xi 6) (+ y 14) 12)))
    (draw-line buffer :gray (- x 1500) y (+ x 1500) y)
    (draw-line buffer :gray x (- y 1500) x (+ y 1500))))

(defn graph-mode-draw [world]
  (let [graph-box (:graph-box world)
        {:keys [x y w h]} graph-box
        buffer (:buffer graph-box)
        hw (* w 0.5)
        hh (* h 0.5)
        menu (:graph-menu world)
        border-color (if (= (:graph-subcommand world) :move)
                       :black
                       :white)]

    (clear buffer border-color)
    (fill-rect buffer :black hw hh (- w 14) (- h 14))
    (fill-rect! :black x (+ y hh 15) w 30)

    (let [region (get-in (:regions menu) [(:graph-subcommand world)])
          {:keys [x y w h]} (get-absolute-region region menu)]
      (fill-rect! :dark-gray x y w h))
    (draw-image! (:image menu) (:x menu) (:y menu))

    (if-let [chip-name (:selected-chip world)]
      (let [chip (get-in world [:parts chip-name])
            view (:view chip)]
        (draw-grid graph-box view)
        (doseq [[part-name function] (sort-by #(:z (second %)) < (:functions chip))]
          (let [color (get-in world [:parts part-name :color])]
            (draw-function! (:graph-box world) view function color))))
      (draw-graph-cross graph-box))

    (draw-image! buffer x y)

    (let [x (+ x (* w 0.5) -20)
          y (:y menu)]
      (draw-rect! :gray x y 15 10)
      (draw-line! :gray (- x 4) y (+ x 3) y))
    ))

(defn normalize-function [function]
  (let [points (:points function)
        points (map (fn [[t v]]
                      [(if (< t 0.0) 0.0 t) v])
                    points)
        points (sort-by first points)]
    (assoc-in function [:points] (vec points))))

(defn activate-chip [world chip-name]
  (let [chip (get-in world [:parts chip-name])
        functions (map-map (fn [[part-name function]]
                              {part-name (normalize-function function)})
                           (:functions chip))
        final-time (apply max (map (comp first last :points)
                                   (vals functions)))]
    (-> world
        (assoc-in [:parts chip-name :functions] functions)
        (assoc-in [:parts chip-name :time] 0.0)
        (assoc-in [:parts chip-name :final-time] final-time))))

(defn graph-mode-entered [world]
  (assoc-in world [:graph-subcommand] :move))

(defn run-selected-chip [world]
  (if-let [selected-chip (:selected-chip world)]
    (activate-chip world selected-chip)
    world))

(defn compute-absolute-points [start-value relative-points]
  (let [pairs (map vector relative-points (rest relative-points))
        deltas (map (fn [[[t1 v1] [t2 v2]]]
                      (* (- t2 t1) v1))
                    pairs)]
    (vec (map vector (map first relative-points)
              (map #(+ start-value %) (accumulate deltas))))))


(defn compute-final-points [world function part-name]
  (let [part (get-in world [:parts part-name])
        start-value (if (= (:type part) :wagon)
                      (* (:value part) (reduce + (:track-lengths part)))
                      (:value part))]
    (if (:relative function)
      (compute-absolute-points start-value (:points function))
      (:points function))))

(defn run-wave [world chip-name part-name time dt]
  (let [part (get-in world [:parts part-name])
        linear-interpolator (fn [a b t]
                              (+ (* a (- 1.0 t)) (* b t)))
        function (get-in world [:parts chip-name :functions part-name])
        points (:points function)
        t1 (first (first points))
        t2 (first (last points))]
    (if (or (< t1 time t2)
            (< (abs (- time t1)) dt)
            (< (abs (- time t2)) dt))
      (let [time (within time t1 t2)
            world (if (float= time t1)
                    (-> world
                        (assoc-in [:parts part-name :final-points]
                                  (compute-final-points world function part-name))
                        (assoc-in [:parts part-name :controlling-chip]
                                  chip-name))
                    world)
            final-points (get-in world [:parts part-name :final-points])
            new-value (float (get-function-value
                              final-points time linear-interpolator))]
        (if (= chip-name (:controlling-chip part))
          (cond
            (= (:type part) :wagon)
            (let [total-length (reduce + (:track-lengths part))]
              (assoc-in world [:parts part-name :value]
                        (within (/ new-value total-length) 0 1)))

            (= (:type part) :track)
            (let [max-angle (get-in world [:parts part-name :max-angle])]
              (assoc-in world [:parts part-name :value]
                        (if (nil? max-angle)
                          new-value
                          (within new-value 0 max-angle))))
            (= (:type part) :button)
            (assoc-in world [:parts part-name :value] (round new-value)))
          world))
      world)))

(defn run-chip [world chip-name dt]
  (let [chip (get-in world [:parts chip-name])
        final-time (:final-time chip)
        time (:time chip)]
    (if (> time (+ final-time dt))
      world
      (let [world (update-in world [:parts chip-name :time] #(+ % dt))]
        (reduce (fn [w function-name]
                  (run-wave w chip-name function-name time dt))
                world
                (keys (:functions chip)))))))

(defn run-chips [world elapsed]
  (let [dt (float (/ elapsed 1000))
        chip-names (filter (fn [name]
                             (= (get-in world [:parts name :type])
                                :chip))
                           (keys (:parts world)))]
    (reduce (fn [w chip-name]
              (run-chip w chip-name dt))
            world
            chip-names)))

(defn get-node-at [world x y]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        view (:view chip)
        [t v] (global->local graph-box view [x y])
        functions (sort-by #(:z (second %)) > (:functions chip))
        named-points (mapcat (fn [[name function]]
                               (let [points (:points function)]
                               (map (fn [point index]
                                      [name index point])
                                    points (range (count points)))))
                             functions)
        dx (/ (* 2 (/ 7.0 (:w graph-box))) (:zoom-x view))
        dy (/ (* 2 (/ 7.0 (:h graph-box))) (:zoom-y view))
        named-point (find-if (fn [[name index [px py]]]
                               (and (< (abs (- px t)) dx)
                                    (< (abs (- py v)) dy)))
                             named-points)]
    (if (nil? named-point)
      nil
      (vec (take 2 named-point)))))

(defn set-node-value-callback [world node which text]
  (if-let [value (parse-float text)]
    (let [[function-name node-index] node
          coord-index (if (= which :x) 0 1)]
      (update-in world [:parts (:selected-chip world)
                        :functions function-name]
                 (fn [function]
                   (-> function
                       (assoc-in [:points node-index coord-index] value)
                       (normalize-function)))))
    (do
      (user-message! "invalid value")
      world)))

(defn set-node-values-callback [world node text]
  (let [values (map parse-float (split text #","))]
    (if (or (not (= (count values) 2))
            (some nil? values))
      (do
        (user-message! "invalid format")
        world)
      (let [[x y] values
            [function-name node-index] node]
        (update-in world [:parts (:selected-chip world)
                          :functions function-name]
                   (fn [function]
                     (-> function
                         (assoc-in [:points node-index 0] x)
                         (assoc-in [:points node-index 1] y)
                         (normalize-function))))))))

(defn set-node [world which x y]
  (if-let [node (get-node-at world x y)]
    (if (= which :both)
      (read-input world #(set-node-values-callback %1 node %2))
      (read-input world #(set-node-value-callback %1 node which %2)))
    world))

(defn normal-function-collision [world points x y]
  (if-let [[function _] (get-node-at world x y)]
    function
    (let [point [x y]
          graph-box (:graph-box world)
          chip-name (:selected-chip world)
          view (get-in world [:parts chip-name :view])
          p2 (local->global graph-box view
                            (global->local graph-box view point))
          points (map #(local->global graph-box view %) points)
          segments (map vector points (rest points))]
      (some (fn [[a b]]
              (point-between-points? p2 a b 10))
            segments))))

(defn square-function-collision [world points x y]
  (let [segments (map vector points (rest points))
        corners (map (fn [[[_ y1] [x2 _]]]
                       [x2 y1])
                     segments)
        new-points (butlast (interleave points
                                        (conj (vec corners) nil)))]
    (normal-function-collision world new-points x y)))

(defn function-collision [world function x y]
  (if (:relative function)
    (square-function-collision world (:points function) x y)
    (normal-function-collision world (:points function) x y)))

(defn get-function-at [world x y]
  (let [chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)]
    (first (find-if (fn [[name function]]
                      (function-collision world function x y))
                    functions))))

(defn add-node [world x y]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)
        view (:view chip)
        [t v] (global->local graph-box view [x y])]
    (if-let [function-name (get-function-at world x y)]
      (update-in world [:parts chip-name :functions function-name]
                 (fn [function]
                   (-> function
                       (update-in [:points] #(conj % [t v]))
                       (normalize-function))))
      world)))

(defn delete-node [world x y]
  (let [chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)]
    (if-let [[function-name index] (get-node-at world x y)]
      (let [function (get-in functions [function-name])]
        (if (or (= index 0)
                (= (count (:points function)) 2))
          world
          (update-in world [:parts chip-name :functions function-name :points]
                     #(vector-remove % index))))
      world)))

(defn toggle-relative-flag [world x y]
  (if-let [function-name (get-function-at world x y)]
    (update-in world [:parts (:selected-chip world) :functions
                      function-name :relative] not)
    world))

(defn snap-coords [coords spec]
  (cond
    (number? spec) (vec (map #(snap-value % spec) coords))
    (vector? spec) (vec (map snap-value coords spec))
    :else coords))

(defn parse-snap [text]
  (let [value (parse-float text)]
    (if (nil? value)
      (let [values (map parse-float (split text #","))]
        (if (and (= (count values) 2)
                 (number? (first values))
                 (> (first values) 0)
                 (number? (second values))
                 (> (second values) 0))
          (vec values)
          (do
            (user-message! "invalid snap format")
            nil)))
      (if (<= value 0.0)
        (do
          (user-message! "disable snap")
          nil)
        value))))

(defn set-snap-value [world]
  (read-input world
              (fn [w text]
                (assoc-in w [:graph-snap-value] (parse-snap text)))))

(defn get-max-z [chip]
  (if (empty? (:functions chip))
    0
    (apply max (map #(:z (second %)) (:functions chip)))))

(defn move-node-pressed [world x y]
  (if-let [node (get-node-at world x y)]
    (let [chip (get-in world [:parts (:selected-chip world)])
          coords (get-in chip [:functions (first node)
                               :points (second node)])
          [function-name index] node
          value (get-in chip [:functions function-name
                              :points index])]
      (user-message! (apply format "node value = %.2f, %.2f" (map float value)))
      (-> world
          (assoc-in [:original-node] coords)
          (assoc-in [:moving-node] node)
          (assoc-in [:parts (:selected-chip world)
                     :functions (first node)
                     :z] (inc (get-max-z chip)))))
    world))

(defn move-node-moved [world x y]
  (if-let [[function-name index] (:moving-node world)]
    (let [graph-box (:graph-box world)
          chip-name (:selected-chip world)
          chip (get-in world [:parts chip-name])
          view (:view chip)
          coords (global->local graph-box view [x y])
          grain (if (:shift-pressed world)
                  0.5
                  (:graph-snap-value world))
          coords (snap-coords coords grain)
          new-points (-> (get-in chip [:functions function-name :points])
                         (assoc-in [index] coords))
          world (assoc-in world [:parts chip-name
                                 :functions function-name :points]
                          new-points)]
      (user-message! "move node:" (apply format "%.2f, %.2f" coords))
      (redraw world))
    world))

(defn move-node-released [world x y]
  (if-let [[function-name _] (:moving-node world)]
    (-> world
        (update-in [:parts (:selected-chip world)
                    :functions function-name]
                   normalize-function)
        (dissoc-in [:moving-node]))
    world))

(defn pan-graph-pressed [world x y]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        view (:view chip)]
    (-> world
        (assoc-in [:start-point] [x y])
        (assoc-in [:saved-offset] (:offset view))
        (assoc-in [:press-time] (get-current-time)))))

(defn pan-graph-moved [world x y]
  (if-let [start-point (:start-point world)]
    (let [graph-box (:graph-box world)
          chip-name (:selected-chip world)
          chip (get-in world [:parts chip-name])
          view (:view chip)
          end-point [x y]
          p1 (global->local graph-box view start-point)
          p2 (global->local graph-box view end-point)
          [dx dy] (vector-subtract p2 p1)
          displacement [(* dx (:zoom-x view))
                        (* dy (:zoom-y view))]]
      (-> world
          (assoc-in [:parts chip-name :view :offset]
                    (vector-add (:saved-offset world) displacement))
          (redraw)))
    world))

(defn pan-graph-released [world x y]
  (let [world (dissoc-in world [:start-point])
        elapsed (- (get-current-time) (:press-time world))]
    (if (> elapsed 200)
      world
      (if-let [function (get-function-at world x y)]
        (let [chip (get-in world [:parts (:selected-chip world)])]
          (assoc-in world [:parts (:selected-chip world)
                           :functions function
                           :z] (inc (get-max-z chip))))
        world))))
      
(defn pan-or-move-pressed [world x y]
  (if (nil? (get-node-at world x y))
    (pan-graph-pressed world x y)
    (move-node-pressed world x y)))

(defn pan-or-move-moved [world x y]
  (if (:moving-node world)
    (move-node-moved world x y)
    (pan-graph-moved world x y)))

(defn pan-or-move-released [world x y]
  (if (:moving-node world)
    (move-node-released world x y)
    (pan-graph-released world x y)))

(defn reset-graph-view [world]
  (if-let [chip-name (:selected-chip world)]
    (assoc-in world [:parts chip-name :view] {:offset [0.025 0.1]
                                              :zoom-x 0.5
                                              :zoom-y 0.5})
    world))

(defn chip-change-part [world event]
  (if-let [part-name (get-part-at world event)]
    (let [chip-name (:selected-chip world)
          chip (get-in world [:parts chip-name])
          part (get-in world [:parts part-name])
          part-type (:type part)]
      (if (in? part-type [:wagon :track :button])
        (let [world (if (in? part-name (keys (:functions chip)))
                      (dissoc-in world [:parts chip-name :functions part-name])
                      (assoc-in world [:parts chip-name :functions part-name]
                                {:points [[0 0] [1 1]]
                                 :relative false
                                 :z (inc (get-max-z chip))}))]
          (tree-changed world))
        world))
    world))

(defn print-lengths [world x y]
  (if-let [function-name (get-function-at world x y)]
    (let [part (get-in world [:parts function-name])]
      (if (= (:type part) :wagon)
        (do
          (user-message! "track lengths: "
                    (vec (map #(format "%.2f" %)
                              (:track-lengths part)))
                    (format "%.2f" (reduce + (:track-lengths part))))
          world)
        world))
    world))

(defn graph-mode-pressed [world {:keys [x y] :as event}]
  (let [graph-box (:graph-box world)
        menu (:graph-menu world)
        button {:x (+ (:x graph-box) (* (:w graph-box) 0.5) -20)
                :y (:y menu)
                :w 20
                :h 20}]
    (cond
      (inside-box? button x y)
      (-> world
          (update-in [:show-submenu] not)
          (place-elements))

      (inside-box? graph-box x y)
      (if-let [selected-chip (:selected-chip world)]
        (let [world (case (:graph-subcommand world)
                      :set-x (set-node world :x x y)
                      :set-y (set-node world :y x y)
                      :set-both (set-node world :both x y)
                      :add (add-node world x y)
                      :delete (delete-node world x y)
                      :move (pan-or-move-pressed world x y)
                      :toggle-relative (toggle-relative-flag world x y)
                      :print-lengths (print-lengths world x y)
                      world)]
          (assoc-in world [:graph-subcommand] :move))
        world)

      (inside-box? menu x y)
      (if-let [selected-chip (:selected-chip world)]
        (if-let [region (get-region-at menu x y)]
          (let [world (case region
                        :run (run-selected-chip world)
                        :view (reset-graph-view world)
                        (assoc-in world [:graph-subcommand] region))]
            (show-hint world :graph region))
          world)
        world)

      :else
      (if-let [part-name (get-part-at world event)]
        (let [part (get-in world [:parts part-name])]
          (if (= (:type part) :chip)
            (assoc-in world [:selected-chip] part-name)
            (if (:selected-chip world)
              (chip-change-part world event)
              world)))
        world))
    ))

(defn graph-mode-moved [world event]
  (pan-or-move-moved world (:x event) (:y event)))

(defn graph-mode-released [world event]
  (pan-or-move-released world (:x event) (:y event)))

(defn place-point [view graph-box local-point global-point]
  (let [p (global->local graph-box view global-point)
        [dx dy] (vector-subtract p local-point)
        v [(* dx (:zoom-x view))
           (* dy (:zoom-y view))]]
    (update-in view [:offset] #(vector-add % v))))

(defn change-zoom [view graph-box event shift-pressed]
  (let [offset (:offset view)
        f (if (pos? (:amount event)) 1.1 0.9)
        new-zoom-x (if shift-pressed
                     (:zoom-x view)
                     (* (:zoom-x view) f))
        new-zoom-y (* (:zoom-y view) f)
        global-point [(:x event) (:y event)]
        local-point (global->local graph-box view global-point)]
    (place-point {:offset offset
                  :zoom-x new-zoom-x
                  :zoom-y new-zoom-y}
                 graph-box local-point global-point)))

(defn graph-mode-scrolled [world event]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)]
    (-> world
        (update-in [:parts chip-name :view]
                   #(change-zoom % (:graph-box world)
                                 event (:shift-pressed world)))
        (redraw))))
