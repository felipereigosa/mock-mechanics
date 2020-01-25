
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

;; (defn map-bindings [names values]
;;   (flatten (vec (apply merge (map (fn [a b]
;;                                     (if (= a '_)
;;                                       nil
;;                                       {a b}))
;;                                   names values)))))

;; (defn process-code [code inputs outputs]
;;   (let [input-names (nth code 1)
;;         output-names (nth code 2)
;;         other (nthrest code 3)
;;         input-bindings (map-bindings input-names inputs)
;;         output-bindings (map-bindings output-names outputs)
;;         helpers '[get-value (fn [name]
;;                               (get-in @world [:parts name :value]))
;;                   activate (fn [name]
;;                              (update-thing! [] #(activate-chip % name)))
;;                   chip-active? (fn [name]
;;                                  (let [chip (get-thing! [:parts name])]
;;                                    (not (= (:time chip) (:final-time chip)))))
;;                   wait (fn [pred]
;;                          (while (pred) (sleep 50)))]]
;;     `(do
;;        (require '[temp.core :refer :all])

;;        (let [~@input-bindings
;;              ~@output-bindings
;;              ~@helpers]
;;          ~@other))))

(defn run-script! [world cpu-name pin-name]
  ;;   (let [cpu (get-in w [:parts cpu-name])
  ;;         root (or (:root-filename cpu) "default")
  ;;         filename (str "resources/scripts/" root ".clj")
  ;;         inputs (keys (:inputs cpu))
  ;;         outputs (keys (:outputs cpu))]
  ;;     (if-let [text (try
  ;;                     (read-string (slurp filename))
  ;;                     (catch Exception e
  ;;                       (println! "eof found on script")))]
  ;;       (let [code (process-code text inputs outputs)]
  ;;         (.start
  ;;          (new Thread
  ;;               (proxy [Runnable] []
  ;;                 (run []
  ;;                   (try
  ;;                     ((eval code) pin-name)
  ;;                     (catch Exception e
  ;;                       (do
  ;;                         (println! "script failed")
  ;;                             (println! (.getMessage e)))))))))))))
  )

;; (defn input-value-changed [world cpu-name input-name]
;;   (run-script! world cpu-name input-name)
;;   world)

;; (defn cpu-input-changes [world cpu-name]
;;   (let [cpu (get-in world [:parts cpu-name])]
;;     (reduce (fn [w [input-name old-value]]
;;               (let [new-value (get-in world [:parts input-name :value])]
;;                 (if (= new-value old-value)
;;                   w
;;                   (-> w
;;                       (input-value-changed cpu-name input-name)
;;                       (assoc-in [:parts cpu-name
;;                                  :inputs input-name] new-value)))))
;;             world
;;             (:inputs cpu))))

(defn cpus-input-changes [world]
  ;; (reduce (fn [w cpu-name]
  ;;           (cpu-input-changes w cpu-name))
  ;;         world
  ;;         (get-parts-with-type (:parts world) :cpu))
  world
  )

(defn run-selected-cpu [world]
  (if-let [selected-cpu (:selected-cpu world)]
    (do
      (run-script! world selected-cpu nil)
      world)
    world))

(defn draw-selected [world]
  (if-let [index (:selected-index world)]
    (let [cpu-name (:selected-cpu world)
          type (:selected-type world)
          values (get-in world [:parts cpu-name type])
          index (within index 0 (dec (count values)))
          part-name (first (nth values index))
          {:keys [y w h]} (:cpu-box world)
          x1 (if (= type :inputs)
               (+ (* 30 index) 37)
               (+ (* 30 index) 30 (/ w 2)))
          y1 (- y (/ h 2))
          part (get-in world [:parts part-name])
          
          transform (if (= (:type part) :track)
                      (get-tail-transform part)
                      (:transform part))          
          position (get-transform-position transform)
          [x2 y2] (project-point world position)]
      (draw-line! :white x1 y1 x2 y2)
      (fill-circle! :white x2 y2 5))))

(defn cpu-mode-draw [world]
  (let [cpu-box (:cpu-box world)
        {:keys [x y w h]} cpu-box
        middle (int (/ window-width 2))]

    (fill-rect! :black x y w h)
    (draw-rect! :dark-gray x y (- w 14) (- h 14))
    (draw-line! :dark-gray middle (- y (/ h 2)) middle (+ y (/ h 2)))
    (if-let [cpu-name (:selected-cpu world)]
      (let [cpu (get-in world [:parts cpu-name])]
        (draw-text! :gray "inputs:" 20 480 15)
        (draw-text! :gray "outputs:" (+ middle 13) 480 15)

        (dotimes [i (count (:outputs cpu))]
          (let [part-name (first (nth (:outputs cpu) i))
                color (get-in world [:parts part-name :color])]
            (fill-rect! color (+ middle 30 (* i 30)) 520 20 20)
            (draw-rect! :gray (+ middle 30 (* i 30)) 520 20 20)))

        (dotimes [i (count (:inputs cpu))]
          (let [part-name (first (nth (:inputs cpu) i))
                color (get-in world [:parts part-name :color])]
            (fill-rect! color (+ 37 (* i 30)) 520 20 20)
            (draw-rect! :gray (+ 37 (* i 30)) 520 20 20))))

      (let [hw (* w 0.5)
            hh (* h 0.5)
            o (- 7)
            x1 (- x hw o)
            x2 (+ x hw o)
            y1 (- y hh o)
            y2 (+ y hh o)]
        (draw-line! :dark-gray x1 y1 x2 y2)
        (draw-line! :dark-gray x1 y2 x2 y1)))

    (draw-selected world)))

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

(defn add-remove [world cpu-name from part-name]
  (let [cpu (get-in world [:parts cpu-name])
        index (.indexOf (map first (get cpu from)) part-name)
        value (get-in world [:parts part-name :value])]
    (if (= index -1)
      (update-in world [:parts cpu-name from]
                 #(vec (conj % [part-name value])))
      (update-in world [:parts cpu-name from]
                 #(vector-remove % index)))))

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
        (let [mid (* (get-in world [:cpu-box :w]) 0.5)]
          (if (< x mid)
            (-> world
                (assoc-in [:selected-index] (int (/ (- x 27) 30)))
                (assoc-in [:selected-type] :inputs))
            (-> world
                (assoc-in [:selected-index] (int (/ (- x mid 20) 30)))
                (assoc-in [:selected-type] :outputs))))
        (cpu-change-part world x y))
      (select-cpu world x y))))

(defn rearrange-selected [world event]
  (let [mid (* (get-in world [:cpu-box :w]) 0.5)
        type (:selected-type world)
        index (if (= type :inputs)
                (int (/ (- (:x event) 30) 30))
                (int (/ (- (:x event) 30 mid) 30)))
        cpu-name (:selected-cpu world)
        names (keys (get-in world [:parts cpu-name type]))
        to-index (within index 0 (dec (count names)))
        from-index (:selected-index world)
        vector (get-in world [:parts cpu-name type])]
    (assoc-in world [:parts cpu-name type]
              (vector-insert (vector-remove vector from-index)
                             (nth vector from-index) to-index))))

(defn cpu-mode-released [world event]
  (-> world
      (rearrange-selected event)
      (dissoc-in [:selected-index])
      (dissoc-in [:selected-type])))
