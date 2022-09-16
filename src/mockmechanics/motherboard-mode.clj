(ns mockmechanics.core
  (:require [mockmechanics.library.util :refer :all]
            [mockmechanics.library.vector :as vector]
            [clojure.java.shell :refer [sh]]))

(declare get-element-value)

(defn motherboard-mode-entered [world]
  (dissoc-in world [:selected-motherboard]))

(defn get-input-names [world motherboard element-name]
  (let [connections (filter (fn [[name connection]]
                              (= (last (:points connection))
                                 element-name))
                            (:connections motherboard))]
    (map (comp first :points second) connections)))

(defn get-pin-direction [motherboard pin-name]
  (if (in? pin-name (keys (:pins motherboard)))
    (let [points (map :points (vals (:connections motherboard)))]
      (cond
        (in? pin-name (map first points)) :input
        (in? pin-name (map last points)) :output
        :else nil))
    nil))

(defn get-pins [motherboard type]
  (filter #(= (get-pin-direction motherboard %) type)
          (keys (:pins motherboard))))

(defn get-input-pin-value [world pin-name]
  (let [part (get-in world [:parts pin-name])]
    (if (= (:type part) :wagon)
      (* (:value part) (reduce + (:track-lengths part)))
      (:value part))))

(defn get-output-pin-value [world motherboard pin-name]
  (let [inputs (get-input-names world motherboard pin-name)]
    (if (= (count inputs) 1)
      (get-element-value world motherboard (first inputs))
      0)))

(defn get-pin-value [world motherboard pin-name]
  (let [part (get-in world [:parts pin-name])
        direction (get-pin-direction motherboard pin-name)]
    (if (= direction :output)
      (get-output-pin-value world motherboard pin-name)
      (get-input-pin-value world pin-name))))

(defn get-and-value [world motherboard gate-name]
  (let [inputs (get-input-names world motherboard gate-name)
        values (map #(get-element-value world motherboard %) inputs)]
    (if (every? #(float= % 1.0) values)
      1
      0)))

(defn get-or-value [world motherboard gate-name]
  (let [inputs (get-input-names world motherboard gate-name)
        values (map #(get-element-value world motherboard %) inputs)]
    (if (some #(float= % 1.0) values)
      1
      0)))

(defn get-not-value [world motherboard gate-name]
  (let [inputs (get-input-names world motherboard gate-name)]
    (if (= (count inputs) 1)
      (- 1 (get-element-value world motherboard (first inputs)))
      0)))

(defn get-gate-value [world motherboard gate-name]
  (let [gate (get-in motherboard [:gates gate-name])]
    (case (:type gate)
      :not (get-not-value world motherboard gate-name)
      :and (get-and-value world motherboard gate-name)
      :or (get-or-value world motherboard gate-name)
      0)))

(defn get-element-value [world motherboard element-name]
  (cond
    (in? element-name (keys (:pins motherboard)))
    (get-pin-value world motherboard element-name)

    (in? element-name (keys (:gates motherboard)))
    (get-gate-value world motherboard element-name)

    :else 0))

(defn run-graph [world motherboard-name]
  (let [motherboard (get-in world [:parts motherboard-name])
        output-names (get-pins motherboard :output)]
    (reduce (fn [w part-name]
              (let [type (get-in w [:parts part-name :type])]
                (case type
                  :chip
                  (let [value (get-element-value w motherboard part-name)]
                    (if (float= value 1.0)
                      (activate-chip w part-name)
                      w))

                  :speaker
                  (let [value (get-element-value w motherboard part-name)
                        part (get-in w [:parts part-name])
                        note (get-note (:frequency part))]
                    (if (float= value 1.0)
                      (note-on note)
                      (note-off note))
                    w)

                  :wagon
                  (let [wagon (get-in w [:parts part-name])
                        value (get-element-value w motherboard part-name)
                        scaled-value (/ value (reduce + (:track-lengths wagon)))]
                    (assoc-in w [:parts part-name :value] scaled-value))

                  :track
                  (-> w
                      (assoc-in [:parts part-name :value]
                                (get-element-value w motherboard part-name))
                      (update-in [:driven-parts] #(vec (distinct (conj % part-name)))))

                  (assoc-in w [:parts part-name :value]
                            (get-element-value w motherboard part-name)))))
            world
            output-names)))

(defn map-bindings [names values]
  (flatten (vec (apply merge (map (fn [a b]
                                    (if (= a '_)
                                      nil
                                      {a b}))
                                  names values)))))

(declare get-pixel-coordinates)

(defn get-color-at [world spec]
  (let [collision (get-part-collision world spec)
        part-name (:part-name collision)
        part (get-in world [:parts part-name])
        color (if (= (:type part) :display)
                (let [image (get-in part [:texture :image])
                      [px py] (get-pixel-coordinates world spec)
                      c (get-color-vector (get-pixel image (+ 10 (* 20 px)) (+ 10 (* 20 py))))
                      [r g b _] (map #(int (* 255 %)) c)]
                  (new Color r g b))
                (:color part))]
    (get-reverse-color color)))

(defn process-code [code pins]
  (let [names (nth code 1)
        body (nthrest code 2)
        bindings (map-bindings names pins)
        helpers '[get-value (fn [name]
                              (let [part (get-in @world [:parts name])
                                    value (:value part)]
                                (if (= (:type part) :wagon)
                                  (* value (reduce + (:track-lengths part)))
                                  value)))

                  set-value (fn [name value]
                              (let [part (get-in @world [:parts name])
                                    value (if (= (:type part) :wagon)
                                            (/ value (reduce + (:track-lengths part)))
                                            value)]
                                (set-thing! [:parts name :value] value)
                                (sleep 50)))

                  chip-active? #(chip-active? @world %)

                  on? (fn [part-name]
                        (float= (get-value part-name) 1.0))

                  off? (fn [part-name]
                         (float= (get-value part-name) 0.0))

                  get-children #(keys (get-thing! [:parts % :children]))

                  get-part-position #(get-part-position
                                       (compute-transforms @world :parts) %)

                  get-color #(get-thing! [:parts % :color])

                  set-color (fn [part-name color]
                              (set-thing! [:parts part-name :color] color)
                              (update-thing! [] tree-changed))

                  wait (fn [pred]
                         (while (pred) (sleep 50))
                         (sleep 100))

                  activate-chip (fn [chip-name]
                                  (update-thing! [] #(activate-chip % chip-name))
                                  (wait #(chip-active? chip-name)))

                  activate-button (fn [button-name]
                                    (set-value button-name 1)
                                    (wait #(on? button-name)))

                  activate-wagon (fn [wagon-name]
                                   (set-value wagon-name 1)
                                   (wait #(not (off? wagon-name)))
                                   (set-value wagon-name 0.5))

                  activate (fn [part-name]
                             (let [world @world
                                   part (get-in world [:parts part-name])]
                               (case (:type part)
                                 :wagon (activate-wagon part-name)
                                 :button (activate-button part-name)
                                 :chip (activate-chip part-name))))

                  = (fn [a b]
                      (if (and (number? a)
                               (number? b))
                        (float= a b)
                        (= a b)))

                  clear-display (fn [display-name color]
                                  (let [display (get-thing! [:parts display-name])
                                        mesh (:texture display)]
                                    (clear (:image mesh) color)
                                    (gl-thread (reset-texture mesh))))

                  set-pixel (fn [display-name x y color]
                              (let [display (get-thing! [:parts display-name])
                                    mesh (:texture display)]
                                (set-pixel! display x y color)
                                (gl-thread (reset-texture mesh))))

                  draw-pattern (fn [display-name pattern x y color]
                                 (let [display (get-thing! [:parts display-name])]
                                   (dotimes [yo (count pattern)]
                                     (dotimes [xo (count (first pattern))]
                                       (if (= (get-in pattern [yo xo]) 1)
                                         (set-pixel display-name (+ x xo) (+ y yo) color))))))
                  ]]
    `(do
       (require '[mockmechanics.core :refer :all])
       (require '[mockmechanics.library.util :refer :all])

       (let [~@bindings
             ~@helpers]
         ~@body))))

(defn get-sorted-pin-list [world motherboard-name]
  (let [motherboard (get-in world [:parts motherboard-name])
        helper (fn [[name pin]]
                 (let [type (get-in world [:parts name :type])]
                   [name (:x pin)]))]
    (map first (sort-by last (map helper (:pins motherboard))))))

(def motherboard-activation-count (atom 0))

(defn get-script-filename [motherboard-name]
  (str "temp/" (dekeyword motherboard-name) "-script.clj"))

(defn show-message [world message]
  (let [world (assoc-in world [:hint]
                        {:text message
                         :time (get-current-time)})]
    (do-later redraw! 1000)
    (redraw world)))

(defn open-editor [world]
  (let [motherboard-name (:selected-motherboard world)
        motherboard (get-in world [:parts motherboard-name])
        filename (get-script-filename (:selected-motherboard world))]
    (create-directory! "temp")
    (spit filename (:script motherboard))
    (.start
      (new Thread
           (proxy [Runnable] []
             (run []
               (sh "emacs" "-rv" filename)
               ))))
    (show-message world "Opening editor...")))

(defn update-scripts [world]
  (reduce (fn [w motherboard-name]
            (if (get-in world [:parts motherboard-name :use-script])
              (let [filename (get-script-filename motherboard-name)]
                (if (file-exists? filename)
                  (assoc-in world [:parts motherboard-name :script]
                            (slurp filename))
                  w))
              w))
          world
          (get-parts-with-type (:parts world) :motherboard)))

(defn run-script [world motherboard-name pin-name]
  (try
    (let [motherboard (get-in world [:parts motherboard-name])
          sorted-pins (get-sorted-pin-list world motherboard-name)
          text (read-string (str "(do" (:script motherboard) ")"))
          code (process-code text sorted-pins)]
      (.start
        (new Thread
             (proxy [Runnable] []
               (run []
                 (swap! motherboard-activation-count inc)
                 ((eval code) pin-name)
                 (swap! motherboard-activation-count dec))))))
    (catch Exception e
      (user-message! e)
      (user-message! "script failed")))
  world)

(defn pin-value-changed [world motherboard-name pin-name]
  (if (get-in world [:parts motherboard-name :use-script])
    (run-script world motherboard-name pin-name)
    (run-graph world motherboard-name)))

(defn update-motherboard [world motherboard-name]
  (let [motherboard (get-in world [:parts motherboard-name])]
    (reduce (fn [w [pin-name pin]]
              (if (:trigger pin)
                (let [old-value (:value pin)
                      new-value (get-in world [:parts pin-name :value])]
                  (if (= new-value old-value)
                    w
                    (-> w
                        (pin-value-changed motherboard-name pin-name)
                        (assoc-in [:parts motherboard-name
                                   :pins pin-name :value] new-value))))
                w))
            world (:pins motherboard))))

(defn update-motherboards [world]
  (reduce (fn [w motherboard-name]
            (update-motherboard w motherboard-name))
          world
          (get-parts-with-type (:parts world) :motherboard)))

(defn get-element-position [motherboard name]
  (if-let [pin (get-in motherboard [:pins name])]
    [(:x pin) 12]
    (let [gate (get-in motherboard [:gates name])]
      [(:x gate) (:y gate)])))

(defn get-connection-points [motherboard connection]
  (map (fn [p]
         (if (keyword? p)
           (get-element-position motherboard p)
           p))
       (:points connection)))

(defn motherboard->world-coords [motherboard-box px py]
  (let [{:keys [x y w h]} motherboard-box
        ox (- x (* w 0.5))
        oy (- y (* h 0.5))]
    [(+ ox px) (+ oy py)]))

(defn world->motherboard-coords [motherboard-box px py]
  (let [{:keys [x y w h]} motherboard-box
        ox (- x (* w 0.5))
        oy (- y (* h 0.5))]
    [(- px ox) (- py oy)]))

(defn draw-connections [motherboard motherboard-box]
  (doseq [connection (vals (:connections motherboard))]
    (when (= (:tab connection) (:tab motherboard))
      (let [points (get-connection-points motherboard connection)
            buffer (:buffer motherboard-box)]
        (when (> (count points) 1)
          (dotimes [i (dec (count points))]
            (let [[x1 y1] (nth points i)
                  [x2 y2] (nth points (inc i))]
              (when (= i 0)
                (let [start-name (first (:points connection))
                      distance (if (in? start-name (keys (:gates motherboard)))
                                 14
                                 (if (> (abs (- x2 x1))
                                        (abs (- y2 y1)))
                                   9
                                   4))
                      [ax ay] (-> (vector/subtract [x2 y2] [x1 y1])
                                  (vector/normalize)
                                  (vector/multiply distance)
                                  (vector/add [x1 y1]))]
                  (fill-circle buffer :yellow ax ay 5)))
              (draw-line buffer :yellow x1 y1 x2 y2)))
          (doseq [i (range 1 (count points))]
            (let [[x y] (nth points i)]
              (fill-rect buffer :black x y 10 10)
              (draw-rect buffer :yellow x y 10 10))))))))

(defn draw-pins [world motherboard]
  (doseq [[name pin] (:pins motherboard)]
    (let [motherboard-box (:motherboard-box world)
          buffer (:buffer motherboard-box)
          part (get-in world [:parts name])]
      (fill-rect buffer (:color part) (:x pin) 12 20 10)
      (draw-rect buffer :gray (:x pin) 12 20 10)
      (when (:trigger pin)
        (draw-circle buffer :blue (:x pin) 12 13)))))

(defn draw-gates [motherboard motherboard-box]
  (doseq [[gate-name gate] (:gates motherboard)]
    (when (= (:tab gate) (:tab motherboard))
      (let [buffer (:buffer motherboard-box)
            x (:x gate)
            y (:y gate)]
        (fill-circle buffer :gray x y 15)
        (let [text (case (:type gate)
                     :and "A"
                     :or "O"
                     :not "N")]
          (draw-text buffer :black text (- x 8) (+ y 7) 20))))))

(defn draw-tab-switcher [motherboard motherboard-box]
  (let [tab-x 659
        buffer (:buffer motherboard-box)
        y (* (:h motherboard-box) 0.5)
        active-tabs (distinct (map (comp :tab second)
                                   (merge (:gates motherboard)
                                          (:connections motherboard))))]
    (fill-rect buffer :dark-gray tab-x y 30 130)
    (doseq [i (range 0 5)]
      (let [tab-y (+ 25 (* i 25))]
        (fill-rect buffer :black tab-x tab-y 25 23)

        (if (in? i active-tabs)
          (fill-circle buffer :white tab-x tab-y 3))

        (if (= i (:tab motherboard))
          (draw-rect buffer :white tab-x tab-y 25 23))))))

(defn draw-arrow [world]
  (if-let [moving-element (:moving-element world)]
    (if (= (first moving-element) :pin)
      (let [motherboard-name (:selected-motherboard world)
            motherboard (get-in world [:parts motherboard-name])
            motherboard-box (:motherboard-box world)
            pin-name (second moving-element)
            x (get-in motherboard [:pins pin-name :x])
            [x1 y1] (motherboard->world-coords motherboard-box x 12)
            pin (get-in world [:parts pin-name])
            transform (if (= (:type pin) :track)
                        (get-tail-transform pin)
                        (:transform pin))
            position (get-transform-position transform)
            [x2 y2] (project-point world position)]
        (draw-line! :white x1 y1 x2 y2)
        (fill-circle! :white x1 y1 3)
        (fill-circle! :white x2 y2 3)))))

(defn draw-code [code buffer]
  (let [w (get-image-width buffer)
        h (get-image-height buffer)
        sub-buffer (new-image (- w 40) (- h 36))
        lines (split code #"\n")
        offset 0]
    (clear sub-buffer :black)
    (dotimes [i (count lines)]
      (draw-text-mono sub-buffer :white (nth lines i) 0 (+ 0 offset (* i 11)) 11))
    (draw-image buffer sub-buffer (+ (/ w 2) 10) (+ (/ h 2) 10))))

(defn motherboard-mode-draw [world]
  (let [motherboard-box (:motherboard-box world)
        {:keys [x y w h]} motherboard-box
        buffer (:buffer motherboard-box)
        hw (* w 0.5)
        hh (* h 0.5)
        menu (:motherboard-menu world)
        border-color (if (= (:motherboard-subcommand world) :move)
                       :black
                       :white)]
    (clear buffer border-color)
    (fill-rect buffer :black hw hh (- w 14) (- h 14))
    (draw-rect buffer :dark-gray hw hh (- w 14) (- h 14))

    (fill-rect! :black x (+ y hh 15) w 30)

    (let [region (get-in (:regions menu) [(:motherboard-subcommand world)])
          {:keys [x y w h]} (get-absolute-region region menu)]
      (fill-rect! :dark-gray x y w h))
    (draw-image! (:image menu) (:x menu) (:y menu))

    (if-let [motherboard-name (:selected-motherboard world)]
      (let [motherboard (get-in world [:parts motherboard-name])]
        (if (:use-script motherboard)
          (draw-code (:script motherboard) buffer)
          (do
            (draw-connections motherboard motherboard-box)
            (draw-gates motherboard motherboard-box)
            (draw-tab-switcher motherboard motherboard-box)))
        (draw-pins world motherboard))
      (draw-graph-cross (:motherboard-box world)))

    (draw-image! buffer x y)
    (draw-arrow world)

    (let [x (+ x (* w -0.5) +20)
          y (:y menu)]
      (draw-rect! :gray x y 15 10)
      (draw-line! :gray (- x 4) y (+ x 3) y))))

(defn prune-connections [motherboard]
  (let [elements (concat (vec (keys (:pins motherboard)))
                         (vec (keys (:gates motherboard))))
        new-connections (map-map (fn [[name connection]]
                                   (let [e (filter keyword? (:points connection))]
                                     (if (every? #(in? % elements) e)
                                       {name connection}
                                       {})))
                                 (:connections motherboard))]
    (assoc-in motherboard [:connections] new-connections)))

(defn get-pin-at [motherboard motherboard-box x y]
  (first (find-if (fn [[name pin]]
                    (let [point (motherboard->world-coords motherboard-box (:x pin) 12)]
                      (< (vector/distance [x y] point) 10)))
                  (:pins motherboard))))

(defn get-gate-at [motherboard motherboard-box x y]
  (first (find-if (fn [[name gate]]
                    (let [point (motherboard->world-coords
                                  motherboard-box (:x gate) (:y gate))]
                      (and (= (:tab gate) (:tab motherboard))
                           (< (vector/distance [x y] point) 15))))
                  (:gates motherboard))))

(defn get-joint-at [motherboard motherboard-box x y]
  (let [named-points (mapcat (fn [[name connection]]
                               (map vector (repeat name) (range)
                                    (:points connection)))
                             (:connections motherboard))
        named-joints (filter (comp not keyword? third) named-points)
        named-joints (map (fn [[joint index [px py]]]
                            [joint index (motherboard->world-coords motherboard-box px py)])
                          named-joints)]
    (find-if (fn [[connection index point]]
               (let [tab (get-in motherboard [:connections connection :tab])]
                 (and (= tab (:tab motherboard))
                      (< (vector/distance point [x y]) 10))))
             named-joints)))

(defn get-connection-at [motherboard motherboard-box x y]
  (let [helper
        (fn [[name connection]]
          (if (= (:tab connection) (:tab motherboard))
            (let [points (get-connection-points motherboard connection)
                  points (map (fn [[px py]]
                                (motherboard->world-coords motherboard-box px py))
                              points)
                  segments (map vector points (rest points))]
              (some (fn [[a b]]
                      (point-between-points? [x y] a b 10))
                    segments))))]
    (if-let [[name _] (find-if helper (:connections motherboard))]
      [:connection name]
      nil)))

(defn get-element-at [motherboard motherboard-box x y]
  (if-let [pin (get-pin-at motherboard motherboard-box x y)]
    [:pin pin]
    (if-let [gate (get-gate-at motherboard motherboard-box x y)]
      [:gate gate]
      (if-let [joint (get-joint-at motherboard motherboard-box x y)]
        (vec (concat [:joint] joint))
        nil))))

(defn get-available-pin-spot [motherboard motherboard-box]
  (let [x-values (sort (map (comp :x second) (:pins motherboard)))
        helper (fn [i]
                 (let [x (* i 40)]
                   (if (and (< x 600)
                            (some #(< (abs (- x %)) 21) x-values))
                     (recur (inc i))
                     x)))]
    (helper 1)))

(defn motherboard-change-part [world event]
  (if-let [part-name (get-part-at world event)]
    (let [motherboard-name (:selected-motherboard world)
          motherboard (get-in world [:parts motherboard-name])
          motherboard-box (:motherboard-box world)
          part (get-in world [:parts part-name])
          part-type (:type part)]
      (if (in? part-name (keys (:pins motherboard)))
        (-> world
            (dissoc-in [:parts motherboard-name :pins part-name])
            (update-in [:parts motherboard-name] prune-connections))

        (let [x (get-available-pin-spot motherboard motherboard-box)
              part (get-in world [:parts part-name])
              trigger (= (:type part) :button)]
          (assoc-in world [:parts motherboard-name :pins part-name]
                    {:x x
                     :trigger trigger
                     :value (:value part)}))))
    world))

(defn motherboard-move-pressed [world {:keys [x y]}]
  (let [motherboard-box (:motherboard-box world)
        selected-motherboard (:selected-motherboard world)]
    (if-let [moving-element (get-element-at
                              (get-in world [:parts selected-motherboard])
                              motherboard-box x y)]
      (assoc-in world [:moving-element] moving-element)
      world)))

(defn motherboard-move-moved [world {:keys [x y]}]
  (if-let [moving-element (:moving-element world)]
    (let [motherboard-box (:motherboard-box world)
          [x y] (world->motherboard-coords motherboard-box x y)
          [x y] (snap-point [x y])
          motherboard-name (:selected-motherboard world)
          world (case (first moving-element)
                  :pin (let [pin-name (second moving-element)]
                         (assoc-in world [:parts motherboard-name :pins
                                          pin-name :x] x))
                  :gate (let [gate-name (second moving-element)]
                          (-> world
                              (assoc-in [:parts motherboard-name
                                         :gates gate-name :x] x)
                              (assoc-in [:parts motherboard-name
                                         :gates gate-name :y] y)))
                  :joint (let [[_ connection-name index _] moving-element]
                           (assoc-in world [:parts motherboard-name
                                            :connections connection-name
                                            :points index] [x y])))]
      (redraw world))
    world))

(defn motherboard-move-released [world event]
  (-> world
      (redraw)
      (dissoc-in [:moving-element])))

(defn add-gate [motherboard motherboard-box type {:keys [x y]}]
  (let [gate-name (gen-keyword (join-keywords :gate type))
        [x y] (world->motherboard-coords motherboard-box x y)
        [x y] (snap-point [x y])]
    (assoc-in motherboard [:gates gate-name]
              {:type type
               :x x
               :y y
               :tab (:tab motherboard)})))

(defn delete-element [motherboard motherboard-box {:keys [x y]}]
  (let [element (or (get-element-at motherboard motherboard-box x y)
                    (get-connection-at motherboard motherboard-box x y))
        name (second element)]
    (case (first element)
      :pin (-> motherboard
               (dissoc-in [:pins name])
               (prune-connections))
      :gate (-> motherboard
                (dissoc-in [:gates name])
                (prune-connections))
      :joint (dissoc-in motherboard [:connections name])
      :connection (dissoc-in motherboard [:connections name])
      motherboard)))

(defn validate-connection [world motherboard-name connection-name]
  (let [connection (get-in world [:parts motherboard-name
                                  :connections connection-name])]
    (if (apply distinct? (filter keyword? (:points connection)))
      world
      (dissoc-in world [:parts motherboard-name
                        :connections connection-name]))))

(defn motherboard-connect-pressed [world {:keys [x y]}]
  (let [motherboard-name (:selected-motherboard world)
        motherboard (get-in world [:parts motherboard-name])
        motherboard-box (:motherboard-box world)
        [type element] (get-element-at motherboard motherboard-box x y)
        element (if (= type :joint)
                  nil
                  element)]
    (if-let [connection-name (:new-connection world)]
      (let [next (if (nil? element)
                   (snap-point (world->motherboard-coords motherboard-box x y))
                   element)
            world (update-in world [:parts motherboard-name :connections
                                    connection-name :points]
                             #(conj % next))]
        (if (nil? element)
          world
          (-> world
              (dissoc-in [:new-connection])
              (assoc-in [:motherboard-subcommand] :move)
              (validate-connection motherboard-name connection-name))))
      (let [connection-name (gen-keyword :connection)]
        (-> world
            (assoc-in [:new-connection] connection-name)
            (assoc-in [:parts motherboard-name :connections connection-name]
                      {:points [element]
                       :tab (:tab motherboard)}))))))

(defn toggle-trigger-pin [motherboard motherboard-box {:keys [x y]}]
  (if-let [pin-name (get-pin-at motherboard motherboard-box x y)]
    (update-in motherboard [:pins pin-name :trigger] not)
    motherboard))

(defn run-chip-at [world event]
  (let [motherboard (get-in world [:parts (:selected-motherboard world)])
        chip-name (get-pin-at motherboard (:motherboard-box world) (:x event) (:y event))]
    (activate-chip world chip-name)))

(defn toggle-script [world]
  (update-in world [:parts
                    (:selected-motherboard world) :use-script] not))

(defn motherboard-mode-pressed [world event]
  (let [{:keys [x y]} event
        motherboard-box (:motherboard-box world)
        menu (:motherboard-menu world)
        button {:x (+ (:x motherboard-box) (* (:w motherboard-box) -0.5) 20)
                :y (:y menu)
                :w 20
                :h 20}
        [cx cy] (world->motherboard-coords motherboard-box x y)]
    (cond
      (inside-box? button x y)
      (-> world
          (update-in [:show-submenu] not)
          (place-elements))

      (inside-box? motherboard-box x y)
      (let [selected-motherboard (:selected-motherboard world)]
        (if (> cx 646)
          (assoc-in world [:parts selected-motherboard :tab]
                    (within (int (/ (- cy 17) 25)) 0 4))

          (let [motherboard-name (:selected-motherboard world)
                motherboard (get-in world [:parts motherboard-name])]
            (case (:motherboard-subcommand world)
              :move (if (and (:use-script motherboard)
                             (nil? (get-pin-at motherboard motherboard-box x y)))
                      (open-editor world)
                      (motherboard-move-pressed world event))
              :and (-> world
                       (update-in [:parts motherboard-name] #(add-gate % motherboard-box :and event))
                       (assoc-in [:motherboard-subcommand] :move))
              :or (-> world
                      (update-in [:parts motherboard-name] #(add-gate % motherboard-box :or event))
                      (assoc-in [:motherboard-subcommand] :move))
              :not (-> world
                       (update-in [:parts motherboard-name] #(add-gate % motherboard-box :not event))
                       (assoc-in [:motherboard-subcommand] :move))
              :delete (-> world
                          (update-in [:parts motherboard-name] #(delete-element % motherboard-box event))
                          (assoc-in [:motherboard-subcommand] :move))
              :connect (motherboard-connect-pressed world event)
              :toggle (-> world
                          (update-in [:parts motherboard-name] #(toggle-trigger-pin % motherboard-box event))
                          (assoc-in [:motherboard-subcommand] :move))

              :run (-> world
                       (run-chip-at event)
                       (assoc-in [:motherboard-subcommand] :move))
              world))))

      (inside-box? menu x y)
      (if-let [selected-motherboard (:selected-motherboard world)]
        (if-let [region (get-region-at menu x y)]
          (let [world (cond
                        (= region :script) (toggle-script world)
                        (= region :editor) (open-editor world)
                        :else (assoc-in world [:motherboard-subcommand] region))]
            (show-hint world :motherboard region))
          world)
        world)

      :else
      (if-let [part-name (get-part-at world event)]
        (let [part (get-in world [:parts part-name])]
          (if (= (:type part) :motherboard)
            (assoc-in world [:selected-motherboard] part-name)
            (if (:selected-motherboard world)
              (motherboard-change-part world event)
              world)))
        world))))

(defn motherboard-mode-moved [world event]
  (case (:motherboard-subcommand world)
    :move (motherboard-move-moved world event)
    world))

(defn motherboard-mode-released [world event]
  (case (:motherboard-subcommand world)
    :move (motherboard-move-released world event)
    world))
