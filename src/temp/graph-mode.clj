(ns temp.core)

(defn normalize-function [function]
  (let [points (sort-by first (:points function))
        points (vec (map (fn [[t v]]
                             [(within t 0 1)
                              (within v 0 1)])
                           points))
        n (dec (count points))]
    (-> function
        (assoc-in [:points] points)
        (assoc-in [:points 0 0] 0)
        (assoc-in [:points n 0] 1))))

(defn compute-absolute-points [start-value relative-points]
  (let [pairs (map vector relative-points (rest relative-points))
        deltas (map (fn [[[x1 y1] [x2 y2]]]
                      (* (- x2 x1) (map-between-ranges y1 0 1 -1 1)))
                    pairs)]
    (vec (map vector (map first relative-points)
              (map #(+ start-value %) (accumulate deltas))))))

(defn activate-chip [world chip-name]
  (let [chip (get-in world [:parts chip-name])
        functions (map-map (fn [[name function]]
                             (let [start-value (get-in world [:parts name :value])
                                   final-points (if (:relative function)
                                                  (compute-absolute-points start-value (:points function))
                                                  (:points function))
                                   function (assoc-in function [:final-points] final-points)]
                               {name (normalize-function function)}))
                           (:functions chip))]
    (-> world
        (assoc-in [:parts chip-name :functions] functions)
        (assoc-in [:parts chip-name :time] 0.0))))

(defn graph-mode-entered [world]
  (assoc-in world [:graph-subcommand] :move))

(defn run-selected-chip [world]
  (if-let [selected-chip (:selected-chip world)]
    (activate-chip world selected-chip)
    world))

(defn local->global [graph-box [t v]]
  (let [{:keys [x y w h]} graph-box
        hw (/ w 2)
        hh (/ h 2)
        x1 (- x hw)
        x2 (+ x hw)
        y1 (- y hh)
        y2 (+ y hh)]
    [(map-between-ranges t 0 1 (+ x1 7) (- x2 7))
     (map-between-ranges v 0 1 (- y2 7) (+ y1 7))]))

(defn global->local [graph-box [px py]]
  (let [{:keys [x y w h]} graph-box
        hw (/ w 2)
        hh (/ h 2)
        x1 (- x hw)
        x2 (+ x hw)
        y1 (- y hh)
        y2 (+ y hh)]
    [(within (map-between-ranges px (+ x1 7) (- x2 7) 0 1) 0 1)
     (within (map-between-ranges py (- y2 7) (+ y1 7) 0 1) 0 1)]))

(defn draw-normal-function! [graph-box points color]
  (let [{:keys [x y w h]} graph-box]
    (doseq [i (range 1 (count points))]
      (let [[x1 y1] (local->global
                     graph-box (nth points (dec i)))
            [x2 y2] (local->global
                     graph-box (nth points i))]
        (draw-line! color x1 y1 x2 y2)
        (fill-circle! color x2 y2 7)

        (if (= i 1)
          (fill-circle! color x1 y1 7))))))

(defn draw-square-function! [graph-box points color]
  (let [{:keys [x y w h]} graph-box]
    (doseq [i (range 1 (count points))]
      (let [p1 (nth points (dec i))
            p2 (nth points i)
            h1 (second p1)
            h2 (second p2)
            [x1 y1] (local->global graph-box p1)
            [x2 y2] (local->global graph-box p2)]
        (draw-line! color x1 y1 x2 y1)
        (fill-circle! color x2 y2 7)
        (draw-line! color x2 y1 x2 y2)
        (if (= i 1)
          (fill-circle! color x1 y1 7))))))

(defn draw-cross [graph-box]
  (let [{:keys [x y w h]} graph-box
        hw (* w 0.5)
        hh (* h 0.5)
        o (- 7)
        x1 (- x hw o)
        x2 (+ x hw o)
        y1 (- y hh o)
        y2 (+ y hh o)]
    (draw-line! :dark-gray x1 y1 x2 y2)
    (draw-line! :dark-gray x1 y2 x2 y1)))

(defn draw-function! [graph-box function color]
  (if (:relative function)
    (draw-square-function! graph-box (:points function) color)
    (draw-normal-function! graph-box (:points function) color)))

(defn graph-mode-draw [world]
  (let [graph-box (:graph-box world)
        {:keys [x y w h]} graph-box]
    (fill-rect! :black x y w h)
    (draw-rect! :dark-gray x y (- w 14) (- h 14))

    (if-let [chip-name (:selected-chip world)]
      (let [chip (get-in world [:parts chip-name])]
        (doseq [function-name (keys (:functions chip))]
          (let [color (get-in world [:parts function-name :color])
                function (get-in chip [:functions function-name])]
            (draw-function! (:graph-box world) function color))))
      (draw-cross graph-box))))

(defn run-wave [world part-name function old-time time]
  (let [part (get-in world [:parts part-name])
        linear-interpolator (fn [a b t]
                              (+ (* a (- 1.0 t)) (* b t)))
        new-value (float (get-function-value
                          (:final-points function)
                          time linear-interpolator))]
    (assoc-in world [:parts part-name :value] new-value)))

(defn run-chip [world chip-name dt]
  (let [chip (get-in world [:parts chip-name])]
    (if (= (:time chip) 1.0)
      world
      (let [old-time (:time chip)
            time (within (+ old-time dt) 0.0 1.0)
            world (assoc-in world [:parts chip-name :time] time)]
        (reduce (fn [w [name function]]
                  (run-wave w name function old-time time))
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

(defn get-node-at [functions t v]
  (let [named-points (mapcat (fn [[name function]]
                               (let [points (:points function)]
                               (map (fn [point index]
                                      [name index point])
                                    points (range (count points)))))
                             functions)
        named-point (find-if (fn [[name index point]]
                               (< (distance point [t v]) 0.1))
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
      (println "Invalid value")
      world)))

(defn set-node-values-callback [world node text]
  (let [values (map parse-float (split text #","))]
    (if (or (not (= (count values) 2))
            (some nil? values))
      (do
        (println "Invalid format")
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
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)
        [t v] (global->local graph-box [x y])]
    (if-let [node (get-node-at functions t v)]
      (if (= which :both)
        (read-input world #(set-node-values-callback %1 node %2))
        (read-input world #(set-node-value-callback %1 node which %2)))
      world)))

(defn point-between-points? [p p1 p2]
  (if (vector= p1 p2)
    (vector= p p1)
    (let [v (vector-subtract p2 p1)
          line [p1 (vector-normalize v)]
          l (vector-length v)]
      (and
       (< (point-line-distance p line) 0.04)
       (< (distance p p1) l)
       (< (distance p p2) l)))))

(defn normal-function-collision [points t v]
  (let [segments (map vector points (rest points))
        point [t v]]
    (some (fn [[a b]]
            (point-between-points? [t v] a b))
          segments)))

(defn square-function-collision [points t v]
  (let [segments (map vector points (rest points))
        corners (map (fn [[[_ y1] [x2 _]]]
                       [x2 y1])
                     segments)
        new-points (butlast (interleave points
                                        (conj (vec corners) nil)))]
    (normal-function-collision new-points t v)))

(defn function-collision [function t v]
  (if (:relative function)
    (square-function-collision (:points function) t v)
    (normal-function-collision (:points function) t v)))

(defn get-function-at [functions t v]
  (first (find-if (fn [[name function]]
                    (function-collision function t v))
                  functions)))

(defn add-node [world x y]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)
        [t v] (global->local graph-box [x y])]
    (if-let [function-name (get-function-at functions t v)]
      (update-in world [:parts chip-name :functions function-name]
                 (fn [function]
                   (-> function
                       (update-in [:points] #(conj % [t v]))
                       (normalize-function))))
      world)))

(defn delete-node [world x y]
  (let [graph-box (:graph-box world)
        [t v] (global->local graph-box [x y])
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)]
    (if-let [[function-name index] (get-node-at functions t v)]
      (let [function (get-in functions [function-name])]
        (if (or
             (= index 0)
             (= index (dec (count function))))
          world
          (update-in world [:parts chip-name :functions function-name :points]
                     #(vector-remove % index))))
      world)))

(defn move-node-pressed [world x y]
  (let [graph-box (:graph-box world)
        [t v] (global->local graph-box [x y])
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)]
    (if-let [node (get-node-at functions t v)]
      (assoc-in world [:moving-node] node)
      world)))

(defn move-node-moved [world x y]
  (if-let [[function-name index] (:moving-node world)]
    (let [graph-box (:graph-box world)
          coords (global->local graph-box [x y])
          chip-name (:selected-chip world)
          chip (get-in world [:parts chip-name])
          new-points (-> (get-in chip [:functions function-name :points])
                         (assoc-in [index] coords))
          world (assoc-in world [:parts chip-name
                                 :functions function-name :points]
                          new-points)]
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

(defn toggle-relative-flag [world x y]
  (let [graph-box (:graph-box world)
        chip-name (:selected-chip world)
        chip (get-in world [:parts chip-name])
        functions (:functions chip)
        [t v] (global->local graph-box [x y])]
    (if-let [function-name (get-function-at functions t v)]
      (update-in world [:parts chip-name :functions
                        function-name :relative] not)
      world)))

(defn set-part-value [world x y]
  (if-let [part-name (get-part-at world x y)]
    (do
      (println "set value of " part-name)
      (read-input world (fn [w text]
                          (let [value (within (parse-float text) 0 1)]
                            (assoc-in w [:parts part-name :value] value)))))
    world))

(defn graph-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [selected-chip (:selected-chip world)]
      (if (inside-box? (:graph-box world) x y)
        (let [world (case (:graph-subcommand world)
                      :set-x (set-node world :x x y)
                      :set-y (set-node world :y x y)
                      :set-both (set-node world :both x y)
                      :add (add-node world x y)
                      :delete (delete-node world x y)
                      :move (move-node-pressed world x y)
                      :toggle-relative (toggle-relative-flag world x y)
                      world)]
          (assoc-in world [:graph-subcommand] :move))
        (if (= (:graph-subcommand world) :set-value)
          (set-part-value world x y)
          (chip-change-part world x y)))
      (select-chip world x y))))

(defn graph-mode-moved [world event]
  (move-node-moved world (:x event) (:y event)))

(defn graph-mode-released [world event]
  (move-node-released world (:x event) (:y event)))
