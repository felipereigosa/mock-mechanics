
(ns temp.core)

(defn snap-point [[x y]]
  [(snap-value x 10)
   (snap-value y 10)])

(defn cpu-mode-entered [world]
  (assoc-in world [:cpu-subcommand] :move))

(declare get-element-value)

(defn get-input-names [world cpu element-name]
  (let [connections (filter (fn [[name connection]]
                              (= (last (:points connection))
                                 element-name))
                            (:connections cpu))]
    (map (comp first :points second) connections)))

(defn get-input-pin-value [world cpu pin-name]
  (get-in world [:parts pin-name :value]))

(defn get-output-pin-value [world cpu pin-name]
  (let [inputs (get-input-names world cpu pin-name)]
    (if (= (count inputs) 1)
      (get-element-value world cpu (first inputs))
      0)))

(defn get-pin-value [world cpu pin-name]
  (let [part (get-in world [:parts pin-name])
        direction (get-in world [:info (:type part) :direction])]
    (if (= direction :output)
      (get-output-pin-value world cpu pin-name)
      (get-input-pin-value world cpu pin-name))))

(defn get-and-value [world cpu gate-name]
  (let [inputs (get-input-names world cpu gate-name)
        values (map #(get-element-value world cpu %) inputs)]
    (if (every? #(float= % 1.0) values)
      1
      0)))

(defn get-or-value [world cpu gate-name]
  (let [inputs (get-input-names world cpu gate-name)
        values (map #(get-element-value world cpu %) inputs)]
    (if (some #(float= % 1.0) values)
      1
      0)))

(defn get-not-value [world cpu gate-name]
  (let [inputs (get-input-names world cpu gate-name)]
    (if (= (count inputs) 1)
      (- 1 (get-element-value world cpu (first inputs)))
      0)))

(defn get-gate-value [world cpu gate-name]
  (let [gate (get-in cpu [:gates gate-name])]
    (case (:type gate)
      :not (get-not-value world cpu gate-name)
      :and (get-and-value world cpu gate-name)
      :or (get-or-value world cpu gate-name)
      0)))

(defn get-element-value [world cpu element-name]
  (cond
    (in? element-name (keys (:pins cpu)))
    (get-pin-value world cpu element-name)

    (in? element-name (keys (:gates cpu)))
    (get-gate-value world cpu element-name)

    :else 0))

(defn get-cpu-outputs [world cpu]
  (map (fn [pin-name]
         (let [part (get-in world [:parts pin-name])
               type (:type part)
               direction (get-in world [:info type :direction])]
           (if (= direction :output)
             [pin-name type]
             nil)))
       (keys (:pins cpu))))

(defn run-graph [world cpu-name]
  (let [cpu (get-in world [:parts cpu-name])
        output-names (filter not-nil? (get-cpu-outputs world cpu))]
    (reduce (fn [w [part-name type]]
              (case type
                :lamp
                (assoc-in w [:parts part-name :value]
                          (get-element-value world cpu part-name))

                :chip
                (let [value (get-element-value world cpu part-name)]
                  (if (float= value 1.0)
                    (activate-chip world part-name)
                    w))
                w))
            world
            output-names)))

(defn map-bindings [names values]
  (flatten (vec (apply merge (map (fn [a b]
                                    (if (= a '_)
                                      nil
                                      {a b}))
                                  names values)))))

(defn process-code [code inputs outputs]
  (let [input-names (nth code 1)
        output-names (nth code 2)
        other (nthrest code 3)
        input-bindings (map-bindings input-names inputs)
        output-bindings (map-bindings output-names outputs)
        helpers '[get-value (fn [name]
                              (get-in @world [:parts name :value]))
                  activate (fn [name]
                             (update-thing! [] #(activate-chip % name)))
                  chip-active? (fn [name]
                                 (let [chip (get-thing! [:parts name])]
                                   (not (= (:time chip) (:final-time chip)))))
                  wait (fn [pred]
                         (while (pred) (sleep 50)))]]
    `(do
       (require '[temp.core :refer :all])

       (let [~@input-bindings
             ~@output-bindings
             ~@helpers]
         ~@other))))

(defn get-sorted-pin-list [world cpu-name which]
  (let [cpu (get-in world [:parts :cpu11905])
        helper (fn [[name pin]]
                 (let [type (get-in world [:parts name :type])
                       direction (get-in world [:info type :direction])]
                   [name direction (:x pin)]))
        pins (sort-by last (map helper (:pins cpu)))]
    (map first (filter #(= (second %) which) pins))))

(defn run-script [world cpu-name pin-name]
  (let [cpu (get-in world [:parts cpu-name])
        inputs (get-sorted-pin-list world cpu-name :input)
        outputs (get-sorted-pin-list world cpu-name :output)
        filename (str "resources/scripts/script.clj")]
    (if-let [text (try
                    (read-string (slurp filename))
                    (catch Exception e
                      (println! "eof found on script")))]
      (let [code (process-code text inputs outputs)]
        (.start
         (new Thread
              (proxy [Runnable] []
                (run []
                  (try
                    ((eval code) pin-name)
                    (catch Exception e
                      (do
                        (println! "script failed")
                        (println! (.getMessage e)))))))))))
    world))

(defn pin-value-changed [world cpu-name pin-name]
  (if (get-in world [:parts cpu-name :script])
    (run-script world cpu-name pin-name)
    (run-graph world cpu-name)))

(defn update-cpu [world cpu-name]
  (let [cpu (get-in world [:parts cpu-name])]
    (reduce (fn [w [pin-name pin]]
              (if (:trigger pin)
                (let [old-value (:value pin)
                      new-value (get-in world [:parts pin-name :value])]
                  (if (= new-value old-value)
                    w
                    (-> w
                        (pin-value-changed cpu-name pin-name)
                        (assoc-in [:parts cpu-name
                                   :pins pin-name :value] new-value))))
                w))
            world (:pins cpu))))

(defn update-cpus [world]
  (reduce (fn [w cpu-name]
            (update-cpu w cpu-name))
          world
          (get-parts-with-type (:parts world) :cpu)))

(defn get-element-position [cpu name]
  (if-let [pin (get-in cpu [:pins name])]
    [(:x pin) 12]
    (let [gate (get-in cpu [:gates name])]
      [(:x gate) (:y gate)])))

(defn get-connection-points [cpu connection]
  (map (fn [p]
         (if (keyword? p)
           (get-element-position cpu p)
           p))
       (:points connection)))

(defn cpu->world-coords [cpu-box px py]
  (let [{:keys [x y w h]} cpu-box
        ox (- x (* w 0.5))
        oy (- y (* h 0.5))]
    [(+ ox px) (+ oy py)]))

(defn world->cpu-coords [cpu-box px py]
  (let [{:keys [x y w h]} cpu-box
        ox (- x (* w 0.5))
        oy (- y (* h 0.5))]
    [(- px ox) (- py oy)]))

(defn draw-connections [cpu cpu-box]
  (doseq [connection (vals (:connections cpu))]
    (when (= (:tab connection) (:tab cpu))
      (let [points (get-connection-points cpu connection)
            buffer (:buffer cpu-box)]
        (when (> (count points) 1)
          (dotimes [i (dec (count points))]
            (let [[x1 y1] (nth points i)
                  [x2 y2] (nth points (inc i))]
              (when (= i 0)
                (let [start-name (first (:points connection))
                      distance (if (in? start-name (keys (:gates cpu)))
                                 14
                                 (if (> (abs (- x2 x1))
                                        (abs (- y2 y1)))
                                   9
                                   4))
                      [ax ay] (-> (vector-subtract [x2 y2] [x1 y1])
                                  (vector-normalize)
                                  (vector-multiply distance)
                                  (vector-add [x1 y1]))]
                  (fill-circle buffer :yellow ax ay 5)))
              (draw-line buffer :yellow x1 y1 x2 y2)))
          (doseq [i (range 1 (dec (count points)))]
            (let [[x y] (nth points i)]
              (fill-rect buffer :black x y 10 10)
              (draw-rect buffer :yellow x y 10 10))))))))

(defn draw-pins [world cpu]
  (doseq [[name pin] (:pins cpu)]
    (let [cpu-box (:cpu-box world)
          buffer (:buffer cpu-box)
          part (get-in world [:parts name])]
      (fill-rect buffer (:color part) (:x pin) 12 20 10)
      (when (:trigger pin)
        (draw-circle buffer :blue (:x pin) 12 13)))))

(defn draw-gates [cpu cpu-box]
  (doseq [[gate-name gate] (:gates cpu)]
    (when (= (:tab gate) (:tab cpu))
      (let [buffer (:buffer cpu-box)
            x (:x gate)
            y (:y gate)]
      (fill-circle buffer :gray x y 15)
      (let [text (case (:type gate)
                   :and "A"
                   :or "O"
                   :not "N")]
        (draw-text buffer :black text (- x 8) (+ y 7) 20))))))

(defn draw-tab-switcher [cpu cpu-box]
  (let [tab-x 659
        buffer (:buffer cpu-box)
        y (* (:h cpu-box) 0.5)]
    (fill-rect buffer :dark-gray tab-x y 30 130)
    (doseq [i (range 0 5)]
      (let [tab-y (+ 25 (* i 25))]
        (fill-rect buffer :black tab-x tab-y 25 23)
        (if (= i (:tab cpu))
          (draw-rect buffer :white tab-x tab-y 25 23))))))

(defn draw-arrow [world]
  (if-let [moving-element (:moving-element world)]
    (if (= (first moving-element) :pin)
      (let [cpu-name (:selected-cpu world)
            cpu (get-in world [:parts cpu-name])
            cpu-box (:cpu-box world)
            pin-name (second moving-element)
            x (get-in cpu [:pins pin-name :x])
            [x1 y1] (cpu->world-coords cpu-box x 12)
            pin (get-in world [:parts pin-name])
            transform (if (= (:type pin) :track)
                        (get-tail-transform pin)
                        (:transform pin))
            position (get-transform-position transform)
            [x2 y2] (project-point world position)]
        (draw-line! :white x1 y1 x2 y2)
        (fill-circle! :white x1 y1 3)
        (fill-circle! :white x2 y2 3)))))

(defn cpu-mode-draw [world]
  (let [cpu-box (:cpu-box world)
        {:keys [x y w h]} cpu-box
        buffer (:buffer cpu-box)
        hw (* w 0.5)
        hh (* h 0.5)
        border-color (if (= (:cpu-subcommand world) :move)
                       :black
                       :white)]
    (clear buffer border-color)
    (fill-rect buffer :black hw hh (- w 14) (- h 14))
    (draw-rect buffer :dark-gray hw hh (- w 14) (- h 14))

    (if-let [cpu-name (:selected-cpu world)]
      (let [cpu (get-in world [:parts cpu-name])]
        (if (:script cpu)
          (do
            (fill-rect buffer :white 50 50 30 50)
            (fill-rect buffer :black 50 35 25 4)
            (fill-rect buffer :black 50 45 25 4)
            (fill-rect buffer :black 50 55 25 4))            
          (do
            (draw-connections cpu cpu-box)
            (draw-pins world cpu)
            (draw-gates cpu cpu-box)
            (draw-tab-switcher cpu cpu-box))))
      (draw-graph-cross (:cpu-box world)))

    (draw-image! buffer x y)
    (draw-arrow world)))

(defn prune-connections [cpu]
  (let [elements (concat (vec (keys (:pins cpu)))
                         (vec (keys (:gates cpu))))
        new-connections (map-map (fn [[name connection]]
                                   (let [e (filter keyword? (:points connection))]
                                     (if (every? #(in? % elements) e)
                                       {name connection}
                                       {})))
                                 (:connections cpu))]
    (assoc-in cpu [:connections] new-connections)))

(defn get-pin-at [cpu cpu-box x y]
  (first (find-if (fn [[name pin]]
                    (let [point (cpu->world-coords cpu-box (:x pin) 12)]
                      (< (distance [x y] point) 10)))
                  (:pins cpu))))

(defn get-gate-at [cpu cpu-box x y]
  (first (find-if (fn [[name gate]]
                    (let [point (cpu->world-coords
                                 cpu-box (:x gate) (:y gate))]
                      (and (= (:tab gate) (:tab cpu))
                           (< (distance [x y] point) 15))))
                  (:gates cpu))))

(defn get-joint-at [cpu cpu-box x y]
  (let [named-points (mapcat (fn [[name connection]]
                               (map vector (repeat name) (range)
                                    (:points connection)))
                             (:connections cpu))
        named-joints (filter (comp not keyword? third) named-points)
        named-joints (map (fn [[joint index [px py]]]
                            [joint index (cpu->world-coords cpu-box px py)])
                          named-joints)]
    (find-if (fn [[connection index point]]
               (let [tab (get-in cpu [:connections connection :tab])]
                 (and (= tab (:tab cpu))
                      (< (distance point [x y]) 10))))
             named-joints)))

(defn get-connection-at [cpu cpu-box x y]
  (if-let [[name _]
           (find-if (fn [[name connection]]
                      (if (= (:tab connection) (:tab cpu))
                        (let [points (get-connection-points cpu connection)
                              points (map (fn [[px py]]
                                            (cpu->world-coords cpu-box px py))
                                          points)
                              segments (map vector points (rest points))]
                          (some (fn [[a b]]
                                  (point-between-points? [x y] a b 0.01))
                                segments))))
                    (:connections cpu))]
    [:connection name]
    nil))

(defn get-element-at [cpu cpu-box x y]
  (if-let [pin (get-pin-at cpu cpu-box x y)]
    [:pin pin]
    (if-let [gate (get-gate-at cpu cpu-box x y)]
      [:gate gate]
      (if-let [joint (get-joint-at cpu cpu-box x y)]
        (vec (concat [:joint] joint))
        nil))))

(defn get-available-pin-spot [cpu cpu-box]
  (let [x-values (sort (map (comp :x second) (:pins cpu)))
        helper (fn [i xs]
                 (if (or (empty? xs)
                         (> (abs (- (* i 40) (first xs))) 20))
                   (* i 40)
                   (recur (inc i) (rest xs))))]
    (helper 1 x-values)))

(defn cpu-change-part [world x y]
  (if-let [part-name (get-part-at world x y)]
    (let [cpu-name (:selected-cpu world)
          cpu (get-in world [:parts cpu-name])
          cpu-box (:cpu-box world)
          part (get-in world [:parts part-name])
          part-type (:type part)
          part-direction (get-in world [:info part-type :direction])]
      (if (nil? part-direction)
        world
        (if (in? part-name (keys (:pins cpu)))
          (-> world
              (dissoc-in [:parts cpu-name :pins part-name])
              (update-in [:parts cpu-name] prune-connections))

          (let [x (get-available-pin-spot cpu cpu-box)
                part (get-in world [:parts part-name])]
            (assoc-in world [:parts cpu-name :pins part-name]
                      {:x x
                       :trigger false
                       :value (:value part)})))))
    world))

(defn cpu-move-pressed [world {:keys [x y]}]
  (let [cpu-box (:cpu-box world)
        selected-cpu (:selected-cpu world)]
    (if-let [moving-element (get-element-at
                             (get-in world [:parts selected-cpu])
                             cpu-box x y)]
      (assoc-in world [:moving-element] moving-element)
      world)))

(defn cpu-move-moved [world {:keys [x y]}]
  (if-let [moving-element (:moving-element world)]
    (let [cpu-box (:cpu-box world)
          [x y] (world->cpu-coords cpu-box x y)
          [x y] (snap-point [x y])
          cpu-name (:selected-cpu world)
          world (case (first moving-element)
                  :pin (let [pin-name (second moving-element)]
                         (assoc-in world [:parts cpu-name :pins
                                           pin-name :x] x))
                  :gate (let [gate-name (second moving-element)]
                          (-> world
                              (assoc-in [:parts cpu-name
                                         :gates gate-name :x] x)
                              (assoc-in [:parts cpu-name
                                         :gates gate-name :y] y)))
                  :joint (let [[_ connection-name index _] moving-element]
                           (assoc-in world [:parts cpu-name
                                            :connections connection-name
                                            :points index] [x y])))]
      (redraw world))
    world))

(defn cpu-move-released [world event]
  (-> world
      (redraw)
      (dissoc-in [:moving-element])))

(defn add-gate [cpu cpu-box type {:keys [x y]}]
  (let [gate-name (gen-keyword (join-keywords :gate type))
        [x y] (world->cpu-coords cpu-box x y)
        [x y] (snap-point [x y])]
    (assoc-in cpu [:gates gate-name]
              {:type type
               :x x
               :y y
               :tab (:tab cpu)})))

(defn delete-element [cpu cpu-box {:keys [x y]}]
  (let [element (or (get-element-at cpu cpu-box x y)
                    (get-connection-at cpu cpu-box x y))
        name (second element)]
    (case (first element)
      :pin (-> cpu
               (dissoc-in [:pins name])
               (prune-connections))
      :gate (-> cpu
               (dissoc-in [:gates name])
               (prune-connections))
      :joint (dissoc-in cpu [:connections name])
      :connection (dissoc-in cpu [:connections name])
      cpu)))

(defn cpu-connect-pressed [world {:keys [x y]}]
  (let [cpu-name (:selected-cpu world)
        cpu (get-in world [:parts cpu-name])
        cpu-box (:cpu-box world)
        element (second (get-element-at cpu cpu-box x y))]
    (if-let [connection-name (:new-connection world)]
      (let [next (if (nil? element)
                   (snap-point (world->cpu-coords cpu-box x y))
                   element)
            world (update-in world [:parts cpu-name :connections
                                    connection-name :points]
                             #(conj % next))]
        (if (nil? element)
          world
          (-> world
              (dissoc-in [:new-connection])
              (assoc-in [:cpu-subcommand] :move))))
      (let [connection-name (gen-keyword :connection)]
        (-> world
            (assoc-in [:new-connection] connection-name)
            (assoc-in [:parts cpu-name :connections connection-name]
                      {:points [element]
                       :tab (:tab cpu)}))))))

(defn toggle-trigger-pin [cpu cpu-box {:keys [x y]}]
  (if-let [pin-name (get-pin-at cpu cpu-box x y)]
    (update-in cpu [:pins pin-name :trigger] not)
    cpu))

(defn run-chip-at [world event]
  (let [cpu (get-in world [:parts (:selected-cpu world)])
        chip-name (get-pin-at cpu (:cpu-box world) (:x event) (:y event))]
  (activate-chip world chip-name)))

(defn cpu-mode-pressed [world event]
  (let [cpu-box (:cpu-box world)
        {:keys [x y]} event
        [cx cy] (world->cpu-coords cpu-box x y)]
    (if (inside-box? cpu-box x y)
      (if-let [selected-cpu (:selected-cpu world)]
        (if (> cx 646)
          (assoc-in world [:parts selected-cpu :tab] 
                    (within (int (/ (- cy 17) 25)) 0 4))
          (let [cpu-name (:selected-cpu world)]
            (case (:cpu-subcommand world)
              :move (cpu-move-pressed world event)
              :and (-> world
                       (update-in [:parts cpu-name] #(add-gate % cpu-box :and event))
                       (assoc-in [:cpu-subcommand] :move))
              :or (-> world
                      (update-in [:parts cpu-name] #(add-gate % cpu-box :or event))
                      (assoc-in [:cpu-subcommand] :move))
              :not (-> world
                       (update-in [:parts cpu-name] #(add-gate % cpu-box :not event))
                       (assoc-in [:cpu-subcommand] :move))
              :delete (-> world
                          (update-in [:parts cpu-name] #(delete-element % cpu-box event))
                          (assoc-in [:cpu-subcommand] :move))
              :connect (cpu-connect-pressed world event)
              :toggle (-> world
                          (update-in [:parts cpu-name] #(toggle-trigger-pin % cpu-box event))
                          (assoc-in [:cpu-subcommand] :move))

              :run (-> world
                       (run-chip-at event)
                       (assoc-in [:cpu-subcommand] :move))
              world)))
        world)
      (if-let [part-name (get-part-at world x y)]
        (let [part (get-in world [:parts part-name])]
          (if (= (:type part) :cpu)
            (assoc-in world [:selected-cpu] part-name)
            (if (:selected-cpu world)
              (cpu-change-part world x y)
              world)))
        world))))

(defn cpu-mode-moved [world event]
  (case (:cpu-subcommand world)
    :move (cpu-move-moved world event)
    world))

(defn cpu-mode-released [world event]
  (case (:cpu-subcommand world)
    :move (cpu-move-released world event)
    world))

(defn toggle-script [world]
  (if-let [selected-cpu (:selected-cpu world)]
    (update-in world [:parts selected-cpu :script] not)
    world))
