
(ns temp.core)

(defn cpu-mode-entered [world]
  (let [world (assoc-in world [:cpu-subcommand] :move)]
    (if-let [cpu-name (:selected-cpu world)]
      (show-selected-part world cpu-name)
      world)))

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

(defn pin-value-changed [world cpu-name pin-name]
  (let [part (get-in world [:parts pin-name])
        cpu (get-in world [:parts cpu-name])
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

(defn draw-cross [cpu-box]
  (let [{:keys [x y w h]} cpu-box
        hw (* w 0.5)
        hh (* h 0.5)
        o (- 7)
        x1 (- x hw o)
        x2 (+ x hw o)
        y1 (- y hh o)
        y2 (+ y hh o)]
    (draw-line! :dark-gray x1 y1 x2 y2)
    (draw-line! :dark-gray x1 y2 x2 y1)))

(defn get-element-position [cpu name]
  (if-let [pin (get-in cpu [:pins name])]
    [(:x pin) 468]
    (let [gate (get-in cpu [:gates name])]
      [(:x gate) (:y gate)])))

(defn get-connection-points [cpu connection]
  (map (fn [p]
         (if (keyword? p)
           (get-element-position cpu p)
           p))
       (:points connection)))

(defn draw-connections [cpu]
  (doseq [connection (vals (:connections cpu))]
    (when (= (:tab connection) (:tab cpu))
      (let [points (get-connection-points cpu connection)]
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
                  (fill-circle! :yellow ax ay 5)))
              (draw-line! :yellow x1 y1 x2 y2)))
          (doseq [i (range 1 (dec (count points)))]
            (let [[x y] (nth points i)]
              (fill-rect! :black x y 10 10)
              (draw-rect! :yellow x y 10 10))))))))

(defn draw-pins [world cpu]
  (doseq [[name pin] (:pins cpu)]
    (let [part (get-in world [:parts name])]
      (fill-rect! (:color part) (:x pin) 468 20 10)
      (when (:trigger pin)
        (draw-circle! :blue (:x pin) 468 13)))))

(defn draw-gates [cpu]
  (doseq [gate (vals (:gates cpu))]
    (when (= (:tab gate) (:tab cpu))
      (fill-circle! :gray (:x gate) (:y gate) 15)
      (let [text (case (:type gate)
                   :and "A"
                   :or "O"
                   :not "N")]
        (draw-text! :black text (- (:x gate) 8) (+ (:y gate) 7) 20)))))

(defn draw-tab-switcher [cpu]
  (fill-rect! :dark-gray 660 530 30 130)
  (doseq [i (range 0 5)]
    (let [y (map-between-ranges i 0 5 480 605)]
      (fill-rect! :black 660 y 25 23)
      (if (= i (:tab cpu))
        (draw-rect! :white 660 y 25 23)))))

(defn draw-arrow [world]
  (if-let [moving-element (:moving-element world)]
    (if (= (first moving-element) :pin)
      (let [cpu-name (:selected-cpu world)
            cpu (get-in world [:parts cpu-name])
            pin-name (second moving-element)
            x1 (get-in cpu [:pins pin-name :x])
            y1 465
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
        middle (int (/ window-width 2))
        border-color (if (= (:cpu-subcommand world) :move)
                       :black
                       :white)]
    (fill-rect! border-color x y w h)
    (fill-rect! :black x y (- w 14) (- h 14))
    (draw-rect! :dark-gray x y (- w 14) (- h 14))

    (if-let [cpu-name (:selected-cpu world)]
      (let [cpu (get-in world [:parts cpu-name])]
        (draw-connections cpu)
        (draw-pins world cpu)
        (draw-gates cpu)
        (draw-tab-switcher cpu))
      (draw-cross (:cpu-box world)))

    (draw-arrow world)))

(defn select-cpu [world x y]
  (if (inside-box? (:cpu-box world) x y)
    world
    (if-let [part-name (get-part-at world x y)]
      (let [part (get-in world [:parts part-name])]
        (if (= (:type part) :cpu)
          (-> world
              (show-selected-part part-name)
              (assoc-in [:selected-cpu] part-name))
          world))
      world)))

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

(defn get-pin-at [cpu x y]
  (first (find-if (fn [[name pin]]
                    (< (distance [x y] [(:x pin) 468]) 10))
                  (:pins cpu))))

(defn get-gate-at [cpu x y]
  (first (find-if (fn [[name gate]]
                    (and (= (:tab gate) (:tab cpu))
                         (< (distance [x y] [(:x gate) (:y gate)]) 15)))
                  (:gates cpu))))

(defn get-joint-at [cpu x y]
  (let [named-points (mapcat (fn [[name connection]]
                               (map vector (repeat name) (range)
                                    (:points connection)))
                             (:connections cpu))
        named-joints (filter (comp not keyword? third) named-points)]
    (find-if (fn [[connection index point]]
               (let [tab (get-in cpu [:connections connection :tab])]
                 (and (= tab (:tab cpu))
                      (< (distance point [x y]) 10))))
             named-joints)))

(defn get-connection-at [cpu x y]
  (if-let [[name _]
           (find-if (fn [[name connection]]
                      (if (= (:tab connection) (:tab cpu))
                        (let [points (get-connection-points cpu connection)
                              segments (map vector points (rest points))]
                          (some (fn [[a b]]
                                  (point-between-points? [x y] a b 0.01))
                                segments))))
                    (:connections cpu))]
    [:connection name]
    nil))

(defn get-element-at [cpu x y]
  (if-let [pin (get-pin-at cpu x y)]
    [:pin pin]
    (if-let [gate (get-gate-at cpu x y)]
      [:gate gate]
      (if-let [joint (get-joint-at cpu x y)]
        (vec (concat [:joint] joint))
        nil))))

(defn get-available-pin-spot [cpu]
  (let [helper (fn [i]
                 (if (nil? (get-pin-at cpu (* i 40) 468))
                   (* i 40)
                   (recur (inc i))))]
    (helper 1)))

(defn cpu-change-part [world x y]
  (if-let [part-name (get-part-at world x y)]
    (let [cpu-name (:selected-cpu world)
          cpu (get-in world [:parts cpu-name])
          part (get-in world [:parts part-name])
          part-type (:type part)
          part-direction (get-in world [:info part-type :direction])]
      (if (nil? part-direction)
        world
        (if (in? part-name (keys (:pins cpu)))
          (-> world
              (dissoc-in [:parts cpu-name :pins part-name])
              (update-in [:parts cpu-name] prune-connections))

          (let [x (get-available-pin-spot cpu)
                part (get-in world [:parts part-name])]
            (assoc-in world [:parts cpu-name :pins part-name]
                      {:x x
                       :trigger false
                       :value (:value part)})))))
    world))

(defn cpu-move-pressed [world {:keys [x y]}]
  (if-let [selected-cpu (:selected-cpu world)]
    (if (inside-box? (:cpu-box world) x y)
      (if (> x 645)
        (assoc-in world [:parts selected-cpu :tab]
                  (within (int (/ (- y 468) 25)) 0 4))
        (if-let [moving-element (get-element-at
                                 (get-in world [:parts selected-cpu])
                                 x y)]
          (assoc-in world [:moving-element] moving-element)
          world))
      (cpu-change-part world x y))
    (select-cpu world x y)))

(defn snap-point [[x y]]
  [(snap-value x 10)
   (snap-value y 10)])

(defn cpu-move-moved [world {:keys [x y]}]
  (if-let [moving-element (:moving-element world)]
    (let [[x y] (snap-point [x y])
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

      (redraw!)
      world)
    world))

(defn cpu-move-released [world event]
  (redraw!)
  (dissoc-in world [:moving-element]))

(defn add-gate [cpu type {:keys [x y]}]
  (let [gate-name (gen-keyword (join-keywords :gate type))
        [x y] (snap-point [x y])]
    (assoc-in cpu [:gates gate-name]
              {:type type
               :x x
               :y y
               :tab (:tab cpu)})))

(defn delete-element [cpu {:keys [x y]}]
  (let [element (or (get-element-at cpu x y)
                    (get-connection-at cpu x y))
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
        element (second (get-element-at cpu x y))]
    (if-let [connection-name (:new-connection world)]
      (let [next (if (nil? element)
                   (snap-point [x y])
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

(defn toggle-trigger-pin [cpu {:keys [x y]}]
  (if-let [pin-name (get-pin-at cpu x y)]
    (update-in cpu [:pins pin-name :trigger] not)
    cpu))

(defn run-chip-at [world event]
  (let [cpu (get-in world [:parts (:selected-cpu world)])
        chip-name (get-pin-at cpu (:x event) (:y event))]    
  (activate-chip world chip-name)))

(defn cpu-mode-pressed [world event]
  (let [cpu-name (:selected-cpu world)]
    (case (:cpu-subcommand world)
      :move (cpu-move-pressed world event)
      :and (-> world
               (update-in [:parts cpu-name] #(add-gate % :and event))
               (assoc-in [:cpu-subcommand] :move))
      :or (-> world
               (update-in [:parts cpu-name] #(add-gate % :or event))
               (assoc-in [:cpu-subcommand] :move))
      :not (-> world
               (update-in [:parts cpu-name] #(add-gate % :not event))
               (assoc-in [:cpu-subcommand] :move))
      :delete (-> world
                  (update-in [:parts cpu-name] #(delete-element % event))
                  (assoc-in [:cpu-subcommand] :move))
      :connect (cpu-connect-pressed world event)
      :toggle (-> world
                  (update-in [:parts cpu-name] #(toggle-trigger-pin % event))
                  (assoc-in [:cpu-subcommand] :move))

      :run (-> world
               (run-chip-at event)
               (assoc-in [:cpu-subcommand] :move))
      world)))

(defn cpu-mode-moved [world event]
  (case (:cpu-subcommand world)
    :move (cpu-move-moved world event)
    world))

(defn cpu-mode-released [world event]
  (case (:cpu-subcommand world)
    :move (cpu-move-released world event)
    world))
