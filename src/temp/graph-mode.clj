
(ns temp.core)

(defn normalize-function [function]
  (let [points (:points function)
        points (map (fn [[t v]]
                      [(if (< t 0.0) 0.0 t) v])
                    points)
        points (sort-by first points)]
    (-> function
        (assoc-in [:points] (vec points))
        (assoc-in [:points 0 0] 0))))

(defn compute-absolute-points [start-value relative-points]
  (let [pairs (map vector relative-points (rest relative-points))
        deltas (map (fn [[[t1 v1] [t2 v2]]]
                      (* (- t2 t1) v1))
                    pairs)]
    (vec (map vector (map first relative-points)
              (map #(+ start-value %) (accumulate deltas))))))

(defn compute-final-time [functions]
  (apply max (map (comp first last :points) (vals functions))))

(defn activate-chip [world chip-name]
  (let [chip (get-in world [:parts chip-name])
        functions (map-map (fn [[name function]]
                             (let [part (get-in world [:parts name])
                                   start-value (if (= (:type part) :wagon)
                                                 (* (:value part) (reduce + (:track-lengths part)))
                                                 (:value part))
                                   final-points (if (:relative function)
                                                  (compute-absolute-points start-value (:points function))
                                                  (:points function))
                                   function (assoc-in function [:final-points] final-points)]
                               {name (normalize-function function)}))
                           (:functions chip))
        final-time (compute-final-time functions)]
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

(defn local->global [graph-box view [t v]]
  (let [{:keys [x y w h]} graph-box
        [to vo] (:offset view)
        zoom (:zoom view)
        t (+ (* t zoom) to)
        v (+ (* v zoom) vo)]
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
        zoom (:zoom view)
        t (map-between-ranges px (+ x1 7) (- x2 7) 0 1)
        v (map-between-ranges py (- y2 7) (+ y1 7) 0 1)]
    [(/ (- t to) zoom)
     (/ (- v vo) zoom)]))

(defn draw-cross [graph-box]
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
        (fill-circle buffer color x2 y2 7)
        (draw-line buffer color x2 y1 x2 y2)
        (if (= i 1)
          (fill-circle buffer color x1 y1 7))))))

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
        (draw-line buffer :dark-gray xi (- y 1500) xi (+ y 1500))))
    (draw-line buffer :gray (- x 1500) y (+ x 1500) y)
    (draw-line buffer :gray x (- y 1500) x (+ y 1500))))

(defn graph-mode-draw [world]
  (let [graph-box (:graph-box world)
        {:keys [x y w h]} graph-box
        buffer (:buffer graph-box)
        hw (* w 0.5)
        hh (* h 0.5)]

    (clear buffer :black)

    (if-let [chip-name (:selected-chip world)]
      (let [chip (get-in world [:parts chip-name])
            view (:view chip)]
        (draw-grid graph-box view)
        
        (doseq [function-name (keys (:functions chip))]
          (let [color (get-in world [:parts function-name :color])
                function (get-in chip [:functions function-name])]
            (draw-function! (:graph-box world) view function color))))
      (draw-cross graph-box))

    (let [color (if (= (:graph-subcommand world) :move)
                  :black
                  :white)]
      (fill-rect buffer color 0 hh 14 h)
      (fill-rect buffer color w hh 14 h)
      (fill-rect buffer color hw 0 w 14)
      (fill-rect buffer color hw h w 14)
      (draw-rect buffer :dark-gray hw hh (- w 14) (- h 14))
      )
    
    (draw-image! buffer x y)))

(defn run-wave [world part-name function time]
  (let [part (get-in world [:parts part-name])
        linear-interpolator (fn [a b t]
                              (+ (* a (- 1.0 t)) (* b t)))
        new-value (float (get-function-value
                          (:final-points function)
                          time linear-interpolator))]
    (if (= (:type part) :wagon)
      (let [total-length (reduce + (:track-lengths part))]
        (assoc-in world [:parts part-name :value]
                  (/ new-value total-length)))
      (assoc-in world [:parts part-name :value] new-value))))

(defn run-chip [world chip-name dt]
  (let [chip (get-in world [:parts chip-name])
        final-time (:final-time chip)]
    (if (= (:time chip) final-time)
      world
      (let [old-time (:time chip)
            time (within (+ old-time dt) 0.0 final-time)
            world (assoc-in world [:parts chip-name :time] time)]
        (reduce (fn [w [name function]]
                  (run-wave w name function time))
                world
                (:functions chip))))))

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

(defn select-chip [world x y]
  (if (inside-box? (:graph-box world) x y)
    world
    (if-let [part-name (get-part-at world x y)]
      (let [part (get-in world [:parts part-name])]
        (if (= (:type part) :chip)
          (-> world
              (assoc-in [:selected-mesh :transform] (:transform part))
              (assoc-in [:selected-chip] part-name))
          world))
      world)))

(defn chip-change-part [world x y]
  (if-let [part-name (get-part-at world x y)]
    (let [chip-name (:selected-chip world)
          chip (get-in world [:parts chip-name])
          part (get-in world [:parts part-name])
          part-type (:type part)
          part-direction (get-in world [:info part-type :direction])]
      (if (in? part-type [:wagon :track])
        (if (in? part-name (keys (:functions chip)))
          (dissoc-in world [:parts chip-name :functions part-name])
          (if (or (nil? part-name)
                  (= part-name chip-name))
            world
            (assoc-in world [:parts chip-name :functions part-name]
                      {:points [[0 0] [1 1]]
                       :relative false})))
        world))
    world))

(defn get-node-at [world x y]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        view (:view chip)
        [t v] (global->local graph-box view [x y])
        functions (:functions chip)
        named-points (mapcat (fn [[name function]]
                               (let [points (:points function)]
                               (map (fn [point index]
                                      [name index point])
                                    points (range (count points)))))
                             functions)
        dx (/ (* 2 (/ 7.0 (:w graph-box))) (:zoom view))
        dy (/ (* 2 (/ 7.0 (:h graph-box))) (:zoom view))
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
      (println! "Invalid value")
      world)))

(defn set-node-values-callback [world node text]
  (let [values (map parse-float (split text #","))]
    (if (or (not (= (count values) 2))
            (some nil? values))
      (do
        (println! "Invalid format")
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

(defn point-between-points? [p p1 p2 zoom]
  (if (vector= p1 p2)
    (vector= p p1)
    (let [v (vector-subtract p2 p1)
          line [p1 (vector-normalize v)]
          l (vector-length v)]
      (and
       (< (point-line-distance p line) (/ 0.04 zoom))
       (< (distance p p1) l)
       (< (distance p p2) l)))))

(defn normal-function-collision [points t v zoom]
  (let [segments (map vector points (rest points))
        point [t v]]
    (some (fn [[a b]]
            (point-between-points? [t v] a b zoom))
          segments)))

(defn square-function-collision [points t v zoom]
  (let [segments (map vector points (rest points))
        corners (map (fn [[[_ y1] [x2 _]]]
                       [x2 y1])
                     segments)
        new-points (butlast (interleave points
                                        (conj (vec corners) nil)))]
    (normal-function-collision new-points t v zoom)))

(defn function-collision [function t v zoom]
  (if (:relative function)
    (square-function-collision (:points function) t v zoom)
    (normal-function-collision (:points function) t v zoom)))

(defn get-function-at [world x y]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        view (:view chip)
        [t v] (global->local graph-box view [x y])
        functions (:functions chip)]
    (first (find-if (fn [[name function]]
                      (function-collision function t v (:zoom view)))
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

(defn print-wagon-lengths [world x y]
  (if-let [part-name (get-part-at world x y)]
    (let [part (get-in world [:parts part-name])]
      (if (= (:type part) :wagon)
        (println! "track lengths: " (:track-lengths part))
        (println! "not a wagon!"))))
  world)

(defn move-node-pressed [world x y]
  (if-let [node (get-node-at world x y)]
    (assoc-in world [:moving-node] node)
    world))

(defn snap-value [value step]
  (* (round (/ value step)) step))

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
            (println! "invalid snap format")
            nil)))
      (if (<= value 0.0)
        (do
          (println! "disable snap")
          nil)
        value))))

(defn set-snap-value [world]
  (read-input world
              (fn [w text]
                (assoc-in w [:graph-snap-value] (parse-snap text)))))

(defn move-node-moved [world x y]
  (if-let [[function-name index] (:moving-node world)]
    (let [graph-box (:graph-box world)
          chip-name (:selected-chip world)
          chip (get-in world [:parts chip-name])
          view (:view chip)
          coords (global->local graph-box view [x y])
          coords (snap-coords coords (:graph-snap-value world))
          new-points (-> (get-in chip [:functions function-name :points])
                         (assoc-in [index] coords))
          world (assoc-in world [:parts chip-name
                                 :functions function-name :points]
                          new-points)]
      (println! "move node:" (apply format "%.2f, %.2f" coords))
      (draw-2d! world)
      world)
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
        (assoc-in [:saved-offset] (:offset view)))))

(defn pan-graph-moved [world x y]
  (if-let [start-point (:start-point world)]
    (let [graph-box (:graph-box world)
          chip-name (:selected-chip world)
          chip (get-in world [:parts chip-name])
          view (:view chip)
          end-point [x y]
          p1 (global->local graph-box view start-point)
          p2 (global->local graph-box view end-point)
          displacement (vector-multiply (vector-subtract p2 p1)
                                        (:zoom view))
          world (assoc-in world [:parts chip-name :view :offset]
                          (vector-add (:saved-offset world)
                                      displacement))]
      (draw-2d! world)
      world)      
    world))

(defn pan-graph-released [world x y]
  (dissoc-in world [:start-point]))

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
    (assoc-in world [:parts chip-name :view] {:offset [0 0]
                                              :zoom 1})
    world))

(defn graph-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [selected-chip (:selected-chip world)]
      (let [world (if (inside-box? (:graph-box world) x y)
                    (case (:graph-subcommand world)
                      :set-x (set-node world :x x y)
                      :set-y (set-node world :y x y)
                      :set-both (set-node world :both x y)
                      :add (add-node world x y)
                      :delete (delete-node world x y)
                      :move (pan-or-move-pressed world x y)
                      :toggle-relative (toggle-relative-flag world x y)
                      world)
                    (case (:graph-subcommand world)
                      :print-lengths (print-wagon-lengths world x y)
                      (chip-change-part world x y)))]
        (assoc-in world [:graph-subcommand] :move))
      (select-chip world x y))))  

(defn graph-mode-moved [world event]
  (pan-or-move-moved world (:x event) (:y event)))

(defn graph-mode-released [world event]
  (pan-or-move-released world (:x event) (:y event)))

(defn change-zoom [view graph-box event]
  (let [point (global->local graph-box view [(:x event) (:y event)])
        f (if (pos? (:amount event))
            1.1
            0.9)
        offset (:offset view)
        zoom (:zoom view)
        new-zoom (* zoom f)
        displacement (-> (vector-subtract offset point)
                         (vector-multiply f))
        new-offset (vector-add point displacement)]
    {:offset new-offset
     :zoom new-zoom}
    ))

(defn graph-mode-scrolled [world event]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        world (update-in world [:parts chip-name :view]
                         #(change-zoom % (:graph-box world) event))]
    (draw-2d! world)
    world))
