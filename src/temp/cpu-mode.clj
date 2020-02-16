
(ns temp.core)

(defn load-script [world]
  (if-let [selected-cpu (:selected-cpu world)]
    (read-input
     world (fn [w text]
             (let [filename (str "resources/scripts/" text ".clj")]
               (if (file-exists? filename)
                 (assoc-in w [:parts selected-cpu :root-filename] text)
                 (do
                   (println! "invalid filename:" text)
                   w)))))
    (do
      (println! "select a cpu")
      world)))

(defn print-script-name [world]
  (if-let [selected-cpu (:selected-cpu world)]
    (let [name (get-in world [:parts selected-cpu :root-filename])]
      (println! "script: " name)
      world)
    (do
      (println! "select a cpu")
      world)))

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

                  set-value (fn [name value]
                              (set-thing! [:parts name :value] value)
                              (update-thing! [] prepare-tree))
                  
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

(defn get-pin-list [cpu type]
  (map first (sort-by #(:index (second %)) (get cpu type))))

(defn run-script! [world cpu-name pin-name]
  (let [cpu (get-in world [:parts cpu-name])
        root (or (:root-filename cpu) "default")
        filename (str "resources/scripts/" root ".clj")
        inputs (get-pin-list cpu :inputs)
        outputs (get-pin-list cpu :outputs)]
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
                        (println! (.getMessage e)))))))))))))

(defn run-selected-cpu [world]
  (if-let [selected-cpu (:selected-cpu world)]
    (do
      (run-script! world selected-cpu nil)
      world)
    world))

(defn input-value-changed [world cpu-name input-name]
  (run-script! world cpu-name input-name)
  world)

(defn cpu-input-changes [world cpu-name]
  (let [cpu (get-in world [:parts cpu-name])]
    (reduce (fn [w [name input]]
              (let [old-value (get-in world [:parts cpu-name
                                             :inputs name :value])
                    new-value (get-in world [:parts name :value])]
                (if (= new-value old-value)
                  w
                  (-> w
                      (input-value-changed cpu-name name)
                      (assoc-in [:parts cpu-name
                                 :inputs name :value] new-value)))))
            world
            (:inputs cpu))))

(defn cpus-input-changes [world]
  (reduce (fn [w cpu-name]
            (cpu-input-changes w cpu-name))
          world
          (get-parts-with-type (:parts world) :cpu)))

(defn draw-selected-pin [world]
  (if-let [pin-name (:selected-pin world)]
    (let [cpu-name (:selected-cpu world)
          cpu (get-in world [:parts cpu-name])
          inputs (get-pin-list cpu :inputs)
          outputs (get-pin-list cpu :outputs)
          index (if (in? pin-name inputs)
                  (.indexOf inputs pin-name)
                  (.indexOf outputs pin-name))
          {:keys [y w h]} (:cpu-box world)
          x1 (if (in? pin-name inputs)
               (+ (* 30 index) 37)
               (+ (* 30 index) 30 (/ w 2)))
          y1 (- y (/ h 2))
          pin (get-in world [:parts pin-name])
          transform (if (= (:type pin) :track)
                      (get-tail-transform pin)
                      (:transform pin))          
          position (get-transform-position transform)
          [x2 y2] (project-point world position)]
      (draw-line! :white x1 y1 x2 y2)
      (fill-circle! :white x1 y1 3)
      (fill-circle! :white x2 y2 3))))

(defn cpu-mode-draw [world]
  (let [cpu-box (:cpu-box world)
        {:keys [x y w h]} cpu-box
        middle (int (/ window-width 2))]

    (fill-rect! :black x y w h)
    (draw-rect! :dark-gray x y (- w 14) (- h 14))
    (draw-line! :dark-gray middle (- y (/ h 2)) middle (+ y (/ h 2)))

    (if-let [cpu-name (:selected-cpu world)]
      (let [cpu (get-in world [:parts cpu-name])
            sorted-inputs (get-pin-list cpu :inputs)
            sorted-outputs (get-pin-list cpu :outputs)]
        (draw-text! :gray "inputs:" 20 480 15)
        (draw-text! :gray "outputs:" (+ middle 13) 480 15)

        (dotimes [i (count sorted-inputs)]
          (let [part-name (nth sorted-inputs i)
                color (get-in world [:parts part-name :color])]
            (fill-rect! color (+ 37 (* i 30)) 520 20 20)
            (draw-rect! :gray (+ 37 (* i 30)) 520 20 20)))

        (dotimes [i (count sorted-outputs)]
          (let [part-name (nth sorted-outputs i)
                color (get-in world [:parts part-name :color])]
            (fill-rect! color (+ middle 30 (* i 30)) 520 20 20)
            (draw-rect! :gray (+ middle 30 (* i 30)) 520 20 20))))

      (let [hw (* w 0.5)
            hh (* h 0.5)
            o (- 7)
            x1 (- x hw o)
            x2 (+ x hw o)
            y1 (- y hh o)
            y2 (+ y hh o)]
        (draw-line! :dark-gray x1 y1 x2 y2)
        (draw-line! :dark-gray x1 y2 x2 y1)))

    (draw-selected-pin world)))

(defn select-cpu [world x y]
  (if (inside-box? (:cpu-box world) x y)
    world
    (if-let [part-name (get-part-at world x y)]
      (let [part (get-in world [:parts part-name])]
        (if (= (:type part) :cpu)
          (-> world
              (assoc-in [:selected-mesh :transform] (:transform part))
              (assoc-in [:selected-cpu] part-name))
          world))
      world)))

(defn set-order [m v]
  (let [indices-map (apply merge (map (fn [a b]
                                        {a b})
                                      v
                                      (range (count v))))]
     (map-map (fn [[key value]]
                (let [new-index (get indices-map key)]
                  {key (assoc-in value [:index] new-index)}))
              m)))

(defn normalize-indices [world cpu-name which]
  (let [cpu (get-in world [:parts cpu-name])]
    (update-in world [:parts cpu-name which]
               #(set-order % (get-pin-list cpu which)))))

(defn add-remove [world cpu-name from part-name]
  (let [cpu (get-in world [:parts cpu-name])
        value (get-in world [:parts part-name :value])
        pins (get-pin-list cpu from)]
    (normalize-indices
     (if (in? part-name pins)
       (dissoc-in world [:parts cpu-name from part-name])
       (assoc-in world [:parts cpu-name from part-name]
                 {:value value
                  :index (count pins)}))
     cpu-name from)))

(defn cpu-change-part [world x y]
  (if-let [part-name (get-part-at world x y)]
    (let [cpu-name (:selected-cpu world)
          cpu (get-in world [:parts cpu-name])
          part (get-in world [:parts part-name])
          part-type (:type part)
          part-direction (get-in world [:info part-type :direction])]
      (cond
        (= part-name (:selected-cpu world)) world

        (= part-direction :input)
        (add-remove world cpu-name :inputs part-name)

        (= part-direction :output)
        (add-remove world cpu-name :outputs part-name)        

        :else world))
    world))

(defn cpu-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [selected-cpu (:selected-cpu world)]
      (if (inside-box? (:cpu-box world) x y)
        (let [mid (* (get-in world [:cpu-box :w]) 0.5)
              cpu (get-in world [:parts selected-cpu])
              inputs (get-pin-list cpu :inputs)
              outputs (get-pin-list cpu :outputs)
              index (if (< x mid)
                      (within (int (/ (- x 27) 30)) 0 (dec (count inputs)))
                      (within (int (/ (- x mid 20) 30)) 0 (dec (count outputs))))
              pin (if (< x mid)
                    (nth inputs index)
                    (nth outputs index))]
          (assoc-in world [:selected-pin] pin))
        (cpu-change-part world x y))
      (select-cpu world x y))))

(defn rearrange-selected [world event]
  (if-let [selected-pin (:selected-pin world)]
    (let [x (:x event)
          y (:y event)
          cpu-name (:selected-cpu world)
          cpu (get-in world [:parts cpu-name])
          mid (* (get-in world [:cpu-box :w]) 0.5)
          type (if (< x mid) :inputs :outputs)
          from-index (get-in cpu [type selected-pin :index])
          values (get-in world [:parts cpu-name type])
          to-index (if (= type :inputs)
                     (within (int (/ (- (:x event) 27) 30)) 0 (dec (count values)))
                     (within (int (/ (- (:x event) mid 20) 30)) 0 (dec (count values))))
          new-order (vector-insert (vector-remove (get-pin-list cpu type) from-index)
                                   selected-pin to-index)]
      (update-in world [:parts cpu-name type] #(set-order % new-order)))
    world))

(defn cpu-mode-released [world event]
  (-> world
      (rearrange-selected event)
      (dissoc-in [:selected-pin])))
