(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

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
        v (vector/subtract surface-point (first plane))
        [a b c] plane
        v1 (vector/subtract b a)
        v2 (vector/subtract c a)
        ox (vector/scalar-projection v v1)
        oy (vector/scalar-projection v v2)]
    (user-message! "x = " (format "%.2f" (float ox))
                   ", y = " (format "%.2f" (float oy)))))

(declare replaying)

(defn run-add-instruction [world instruction]
  (let [[_ _ part-name _ parent-name _ position rotation] instruction
        parent-name (keyword parent-name)
        parent (get-in world [:parts parent-name])]
    (if (nil? parent)
      (do
        (println "part" parent-name "doesn't exist")
        (reset! replaying false)
        world)
      (let [part-name (keyword part-name)
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
                              w))
            world (-> world
                      (assoc-in [:parts part-name] part)
                      (assoc-in [:parts parent-name :children part-name] transform)
                      (prepare-wagon)
                      (compute-transforms :parts)
                      (tree-changed))]
        (print-part-relative-location world part-name)
        world))))

(defn run-add-gears-instruction [world instruction]
  (let [[_ _ gear-1-name _ gear-2-name _
         part-1-name _ part-2-name _ _ ratio] instruction
        gear-1-name (keyword gear-1-name)
        gear-2-name (keyword gear-2-name)
        part-1-name (keyword part-1-name)
        part-2-name (keyword part-2-name)
        old-names (keys (:parts world))]
    (-> world
        (assoc-in [:parts part-1-name :free] true)
        (assoc-in [:parts part-2-name :free] true)
        (add-two-gears part-1-name part-2-name
                       ratio gear-1-name gear-2-name)
        (tree-changed))))

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
      (user-message! (format "diameter: %.2f" value)))))

(defn scale-animation [world animation]
  (let [{:keys [t start-scale final-scale
                start-transform final-transform
                part-name parent-name change]} animation]
    (cond
      (float= t 0.0)
      (-> world
          (assoc-in [:edited-part] part-name)
          (tree-will-change))

      (float= t 1.0)
      (let [world (-> world
                      (assoc-in [:parts part-name :scale] final-scale)
                      (assoc-in [:parts part-name :transform] final-transform)
                      (edit-children)
                      (dissoc-in [:edited-part])
                      (create-relative-transform part-name parent-name)
                      (tree-changed))]
        (print-scale-change world part-name final-scale change)
        world)

      :else
      (let [t (sigmoid t)
            scale (vector/interpolate start-scale final-scale t)
            transform (interpolate-transforms start-transform
                                              final-transform t)
            world (-> world
                      (assoc-in [:parts part-name :scale] scale)
                      (assoc-in [:parts part-name :transform] transform)
                      (edit-children))]
        (print-scale-change world part-name scale change)
        world))))

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
                 (vector/multiply change 0.5))

        offset (if (= direction '-)
                 (vector/multiply offset -1)
                 offset)
        rotation (get-rotation-component (:transform part))
        offset (apply-transform rotation offset)
        offset-transform (make-transform offset [1 0 0 0])
        transform (:transform part)
        final-transform (combine-transforms transform offset-transform)]
    (assoc-in world [:animation]
              {:t 0.0
               :time (/ 1 (:replay-speed world))
               :fn scale-animation
               :change change
               :part-name part-name
               :parent-name parent-name
               :start-scale (:scale part)
               :final-scale (vector/add (:scale part) change)
               :start-transform transform
               :final-transform final-transform})))

(defn sink-animation [world animation]
  (let [{:keys [t part-name parent-name
                start-transform final-transform
                amount]} animation]
    (cond
      (float= t 0.0)
      (-> world
          (assoc-in [:edited-part] part-name)
          (assoc-in [:start-position] (get-part-position world part-name))
          (tree-will-change))

      (float= t 1.0)
      (do
        (user-message! "height = " (format "%.2f" (float amount)))
        (-> world
            (assoc-in [:parts part-name :transform] final-transform)
            (edit-children)
            (dissoc-in [:edited-part])
            (create-relative-transform part-name parent-name)
            (move-other-gear part-name)
            (tree-changed)))

      :else
      (let [t (sigmoid t)
            transform (interpolate-transforms start-transform
                                              final-transform t)
            h (* amount t)]
        (user-message! "height = " (format "%.2f" (float h)))
        (-> world
            (assoc-in [:parts part-name :transform] transform)
            (edit-children))))))

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
               :time (/ (* (abs amount) 2) (:replay-speed world))
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
      (-> world
          (assoc-in [:edited-part] part-name)
          (tree-will-change))

      (float= t 1.0)
      (let [rotation-transform (make-transform [0 0 0] [0 1 0 angle])
            transform (combine-transforms rotation-transform start-transform)]
        (user-message! "angle = " (format "%.2f" (float angle)))
        (-> world
            (assoc-in [:parts part-name :transform] transform)
            (edit-children)
            (dissoc-in [:edited-part])
            (create-relative-transform part-name parent-name)
            (tree-changed)))

      :else
      (let [t (sigmoid t)
            rotation-transform (make-transform [0 0 0] [0 1 0 (* t angle)])
            transform (combine-transforms rotation-transform start-transform)]
        (user-message! "angle = " (format "%.2f" (float (* t angle))))
        (-> world
            (assoc-in [:parts part-name :transform] transform)
            (edit-children))))))

(defn run-rotate-instruction [world instruction]
  (let [[_  part-name _ amount] instruction
        part-name (keyword part-name)
        parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        part (get-in world [:parts part-name])
        transform (:transform part)]
    (assoc-in world [:animation]
              {:t 0.0
               :time (/ (/ amount 100) (:replay-speed world))
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
      (-> world
          (assoc-in [:edited-part] part-name)
          (tree-will-change))

      (float= t 1.0)
      (let [world (-> world
                      (assoc-in [:parts part-name :transform] final-transform)
                      (edit-children)
                      (dissoc-in [:edited-part])
                      (create-relative-transform part-name parent-name)
                      (tree-changed))]
        (print-part-relative-location world part-name)
        world)

      :else
      (let [transform (interpolate-transforms start-transform
                                              final-transform
                                              (sigmoid t))
            world (-> world
                      (assoc-in [:parts part-name :transform] transform)
                      (edit-children))]
        (print-part-relative-location world part-name)
        world))))

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
               :time (/ 0.7 (:replay-speed world))
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

(declare wait-chip-flag)

(defn run-mouse-instruction [world instruction]
  (robot-set-active! true)
  (let [[_ wait & events] instruction]

    (when wait
      (reset! wait-chip-flag true))

    (.start
      (new Thread
           (proxy [Runnable] []
             (run []
               (robot-move (take 2 (first events)))
               (doseq [i (range 1 (count events))]
                 (let [[x y time type button] (nth events i)
                       delay (- time (nth (nth events (dec i)) 2))]
                   (sleep delay)
                   (case type
                     :pressed (robot-mouse-press button)
                     :moved (robot-move [x y])
                     :released (robot-mouse-release button))))
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
                world)
        world (if (= key :mode)
                (-> world
                    (dissoc-in [:last-selected-part])
                    (dissoc-in [:selected-motherboard])
                    (dissoc-in [:selected-chip]))
                world)]
    (assoc-in world [key] value)))

(defn run-put-instruction [world instruction]
  (let [[_ part-name _ chip-name] instruction
        part-name (keyword part-name)
        chip-name (keyword chip-name)
        chip (get-in world [:parts chip-name])
        part (get-in world [:parts part-name])]
    (if (nil? part)
      (do
        (println "part" part-name "doesn't exist")
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
      (let [point (vector/interpolate start-point end-point (sigmoid t))]
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
        middle (vector/interpolate (last before) (first after) 0.5)
        new-function (vec (concat before [middle] after))]
    (-> world
        (assoc-in [:parts chip-name :functions
                   part-name :points] new-function)
        (assoc-in [:animation]
                  {:t 0.0
                   :time (/ 0.5 (:replay-speed world))
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
               :time (/ 1 (:replay-speed world))
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
            t (sigmoid t)
            zoom (vector/interpolate start-zoom end-zoom t)
            offset (vector/interpolate
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
               :time (/ 1 (:replay-speed world))
               :fn function-zoom-animation
               :chip-name chip-name
               :start-view start-view
               :end-view end-view})))

(defn draw-activate-highlight! [world]
  (let [graph-box (:graph-box world)
        cx (:x graph-box)
        cy (:y graph-box)
        size 20
        points [[(- cx size) (- cy size)]
                [(- cx size) (+ cy size)]
                [(+ cx (* size 0.8)) cy]]]
    (fill-polygon! :white points)))

(defn flash-animation [world animation]
  (let [{:keys [t draw-fn]} animation]
    (draw-fn)
    (redraw world)))

(defn run-activate-instruction [world instruction]
  (let [[_ wait chip-name] instruction
        chip-name (keyword chip-name)]
    (when wait
      (reset! wait-chip-flag true))
    (-> world
        (assoc-in [:animation]
                  {:t 0.0
                   :time 0.5
                   :fn flash-animation
                   :draw-fn #(draw-activate-highlight! world)})
        (activate-chip chip-name))))

(defn run-set-pin-instruction [world instruction]
  (let [[_ motherboard-name _ pin] instruction
        motherboard-name (keyword motherboard-name)
        [part-name x] pin
        part-name (keyword part-name)
        part (get-in world [:parts part-name])]
    (if (nil? part)
      (do
        (println "part" part-name "doesn't exist")
        (reset! replaying false)
        world)
      (-> world
          (select-part part-name)
          (assoc-in [:parts motherboard-name :pins part-name]
                    {:x x
                     :trigger false
                     :value (:value part)})))))

(defn draw-function-toggle-highlight! [world chip-name function-name]
  (let [chip (get-in world [:parts chip-name])
        points (get-in chip [:functions function-name :points])
        view (:view chip)
        graph-box (:graph-box world)
        points (map #(local->global graph-box view %)
                    points)
        x-offset (- (:x graph-box) (* 0.5 (:w graph-box)))
        y-offset (- (:y graph-box) (* 0.5 (:h graph-box)))
        color (get-in world [:parts function-name :color])]
    (doseq [[x y] points]
      (fill-rect! color (+ x x-offset) (+ y y-offset) 20 20))))

(defn run-toggle-instruction [world instruction]
  (let [[_ selected-name _ part-name] instruction
        selected-name (keyword selected-name)
        part-name (keyword part-name)
        selected (get-in world [:parts selected-name])]
    (if (= (:type selected) :chip)
      (-> world
          (assoc-in [:parts selected-name :functions
                     part-name :relative] true)
          (assoc-in [:animation]
                    {:t 0.0
                     :time 0.5
                     :fn flash-animation
                     :draw-fn #(draw-function-toggle-highlight!
                                 world selected-name part-name)}))
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

(defn interpolate [a b t]
  (if (vector? a)
    (vector/interpolate a b t)
    (interpolate-values a b t)))

(defn interpolate-maps [m1 m2 t]
  (merge-with #(interpolate %1 %2 t) m1 m2))

(defn camera-animation [world animation]
  (let [{:keys [t start-camera end-camera]} animation
        set-camera (fn [{:keys [pivot angles distance]}]
                     (let [[x y] angles]
                       (-> world
                           (assoc-in [:camera :pivot] pivot)
                           (assoc-in [:camera :x-angle] x)
                           (assoc-in [:camera :y-angle] y)
                           (assoc-in [:camera :distance] distance)
                           (compute-camera))))]
    (cond
      (float= t 0.0)
      world

      (float= t 1.0)
      (set-camera end-camera)

      :else
      (->> (sigmoid t)
           (interpolate-maps start-camera end-camera)
           (set-camera)))))

(defn run-set-camera-instruction [world instruction]
  (let [partial-camera (apply merge
                              (map (fn [[k v]]
                                     {(keyword k) v})
                                   (partition 2 (nthrest instruction 2))))
        camera (:camera world)
        start-camera {:pivot (:pivot camera)
                      :angles [(:x-angle camera) (:y-angle camera)]
                      :distance (:distance camera)}
        end-camera (merge start-camera partial-camera)]
    (assoc-in world [:animation]
              {:start-camera start-camera
               :end-camera end-camera
               :time (/ 1 (:replay-speed world))
               :t 0.0
               :fn camera-animation})))

(defn run-copy-instruction [world instruction]
  (let [[_ part-name _ parent-name _
         position rotation _ _ suffix] instruction
        part-name (keyword part-name)
        parent-name (keyword parent-name)
        suffix (keyword suffix)
        part (get-in world [:parts part-name])
        [parts copy-part-name] (copy-tree (:parts world) part-name suffix)
        copied-parts (get-tree-with-root parts part-name)
        parts (fix-references parts copied-parts suffix)
        transform (make-transform position rotation)
        world (-> world
                  (assoc-in [:parts] parts)
                  (assoc-in [:parts parent-name :children copy-part-name] transform)
                  (compute-transforms :parts)
                  (tree-changed))]
    (print-part-relative-location world copy-part-name)
    world))

(defn run-transfer-instruction [world instruction]
  (let [[_ part-name _ parent-name _
         position rotation] instruction
        part-name (keyword part-name)
        old-parent-name (get-parent-part world part-name)
        parent-name (keyword parent-name)
        transform (make-transform position rotation)
        world (-> world
                  (dissoc-in [:parts old-parent-name :children part-name])
                  (assoc-in [:parts parent-name :children part-name] transform)
                  (compute-transforms :parts)
                  (tree-changed))]
    (print-part-relative-location world part-name)
    world))

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
                    (interpolate-values start-value final-value (sigmoid t)))
          (redraw)))))

(defn run-set-value-instruction [world instruction]
  (let [[_ _ _ _ part-name _ value] instruction
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
               (* time 6))]
    (assoc-in world [:animation]
              {:t 0.0
               :time (/ time (:replay-speed world))
               :fn value-animation
               :part-name part-name
               :start-value start-value
               :final-value final-value})))

(defn draw-property-highlight! [world property]
  (let [selected-part (:selected-part world)
        type (get-in world [:parts selected-part :type])
        properties (keys (get-in world [:info type :properties]))
        index (get-index property properties)
        box (get-in world [:property-box])
        cell (get-in box [:regions (keyword (str "value" index))])
        {:keys [x y w h]} (get-absolute-region cell box)]
    (draw-rect! :white x y w h)))

(defn run-set-property-instruction [world instruction]
  (let [[_ _ property _ part-name _ value] instruction
        part-name (keyword part-name)
        property (keyword property)]
    (if (= property :value)
      (run-set-value-instruction world instruction)
      (-> world
          (assoc-in [:parts part-name property] value)
          (tree-changed)
          (assoc-in [:animation]
                    {:t 0.0
                     :time 0.2
                     :fn flash-animation
                     :draw-fn #(draw-property-highlight!
                                 world property)})))))

(defn run-select-instruction [world instruction]
  (if (= (count instruction) 2)
    (let [[_ part-name] instruction
          part-name (keyword part-name)]
      (-> world
          (select-part part-name)
          (assoc-in [:last-selected-part] part-name)))
    (let [[_ motherboard-name _ tab-num] instruction
          motherboard-name (keyword motherboard-name)]
      (assoc-in world [:parts motherboard-name :tab] tab-num))))

(declare legend)

(defn run-legend-instruction [world instruction]
  (let [[_ location time text] instruction
        size 30
        text-width (get-text-width! text size)
        x (- (* @window-width 0.5) (* text-width 0.5))
        oy (+ (* (:num-lines world) 16) 30)
        y (- @window-height oy)]

    (reset! legend {:text text
                    :size size
                    :x x
                    :y (cond
                         (= location 'bottom) y
                         (= location 'top) 40
                         :else location)})
    (redraw!)
    (do-later (fn []
                (reset! legend nil)
                (redraw!))
              (* time 1000))
    world))

(defn run-delete-instruction [world instruction]
  (if (= (second instruction) 'part)
    (let [part-name (keyword (third instruction))]
      (-> world
          (delete-part part-name)
          (compute-transforms :parts)
          (tree-changed)))
    (let [[_ motherboard-name _ _ type element-name] instruction
          motherboard-name (keyword motherboard-name)
          type (keyword type)
          element-name (keyword element-name)]
      (case type
        :connection
        (dissoc-in world [:parts motherboard-name
                          :connections element-name])
        :pin
        (-> world
            (dissoc-in [:parts motherboard-name
                        :pins element-name])
            (update-in [:parts motherboard-name] prune-connections))))))

(defn run-import-instruction [world instruction]
  (let [filename (str (second instruction))]
    (-> world
        (import-machine filename)
        tree-changed)))

(defn run-instruction [world instruction]
  (let [words (split instruction #" ")
        instruction-name (cond
                           (= (second words) "point")
                           (str (first words) "-point")

                           (= (second words) "gears")
                           (str (first words) "-gears")

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
    (if-let [function (-> (str "mockmechanics.core/run-"
                               instruction-name
                               "-instruction")
                          (symbol)
                          (resolve))]
      (let [short-instruction (apply str (take 80 instruction))]
        ;; (if (= (count short-instruction)
        ;;        (count instruction))
        ;;   (println instruction)
        ;;   (println short-instruction "..."))
        (-> world
            (function (read-string (str "[" instruction "]")))
            (redraw)))
      (do
        (println "invalid instruction: " instruction)
        world))))
