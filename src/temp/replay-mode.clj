
(require '[clojure.set :refer [difference]])

(do
1

(defn get-descaled-relative-transform [world part-name change]
  (let [parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        offset (if (= (:type part) :track)
                 change
                 (vector-multiply change 0.5))
        offset (vector-multiply offset -1)
        offset-transform (make-transform offset [1 0 0 0])
        relative-transform (get-in world [:parts parent-name
                                          :children part-name])]
    (combine-transforms relative-transform offset-transform)))

(defn create-part-instructions [writer world part-name]
  (let [parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        properties (vec (clojure.set/difference
                         (into #{} (keys part))
                         #{:children :transform :scale
                           :type :already-set}))
        type (:type part)
        color (get-in world [:info type :color])
        new-part (create-part type color 0 (:info world))
        scale-change (vector-subtract (:scale part) (:scale new-part))]
    (let [relative-transform (get-descaled-relative-transform
                              world part-name scale-change)
          position (get-transform-position relative-transform)
          rotation (get-transform-rotation relative-transform)]
      (.write writer (format "add %s to %s at %s %s\n"
                             (dekeyword part-name)
                             (dekeyword parent-name)
                             position
                             rotation)))
    (doseq [i (range 3)]
      (if (not (float= (nth scale-change i) 0.0))
        (.write writer (format "scale %s by %s\n"
                               (dekeyword part-name)
                               (assoc [0 0 0] i (nth scale-change i))))))
    (doseq [property properties]
      (if (not (= (get part property) (get new-part property)))
        (let [value (if (= property :color)
                      (reverse-get-color (get part property))
                      (get part property))
              value (if (keyword value)
                      (dekeyword value)
                      value)]
          (.write writer (format "set %s of %s to %s\n"
                                 (dekeyword property)
                                 (dekeyword part-name)
                                 value)))))))

(defn get-part-number [part-name]
  (parse-int (second (re-find #":[a-z]*([0-9]*)" (str part-name)))))

(defn create-instructions [world]
  (let [sorted-names (>> (:parts world)
                         (keys .)
                         (into #{} .)
                         (clojure.set/difference . #{:ground-part})
                         (vec .)
                         (sort-by get-part-number .))
        world (reduce (fn [w part-name]
                        (set-value-0-transform w part-name))
                      world
                      sorted-names)
        filename "res/decider.txt"]

    (println! "created instructions")
    (with-open [writer (clojure.java.io/writer filename )]
      (doseq [part-name sorted-names]
        (create-part-instructions writer world part-name)
        ))
    ))

;; (clear-output!)
;; (let [world @world
;;       ]
;;   (create-instructions world)
;;   nil
;;   )
)

;;----------------------------------------------------------------------;;

(defn run-add-instruction [world instruction]
  (let [[_ part-name
         _ parent-name
         _ position rotation] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        parent-name (keyword parent-name)
        type (keyword (second (re-find #":([a-z]*)" (str part-name))))
        layer (apply min (:visible-layers world))
        color (get-in world [:info type :color])
        part (create-part type color layer (:info world))
        transform (make-transform position rotation)]
    (-> world
        (assoc-in [:parts parent-name :children part-name] transform)
        (assoc-in [:parts part-name] part)    
        (compute-transforms :parts)
        (tree-changed))))

(defn run-set-instruction [world instruction]
  (let [[_ property-name
         _ part-name
         _ value] (read-string (str "[" instruction "]"))
        property-name (keyword property-name)
        part-name (keyword part-name)
        value (if (symbol? value) (keyword value) value)]
    (-> world
        (set-part-value part-name property-name (str value))
        (tree-changed))))

(defn run-scale-instruction [world instruction]
  (let [[_ part-name
         _ change] (read-string (str "[" instruction "]"))
        part-name (keyword part-name)
        parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        offset (if (= (:type part) :track)
                 change
                 (vector-multiply change 0.5))
        rotation (get-rotation-component (:transform part))
        offset (apply-transform rotation offset)
        offset-transform (make-transform offset [1 0 0 0])]
    (-> world
        (update-in [:parts part-name :scale] #(vector-add % change))
        (update-in [:parts part-name :transform]
                   #(combine-transforms % offset-transform))
        (create-relative-transform part-name parent-name)
        (tree-changed))))

(defn run-mouse-instruction [world instruction]
  (println! "move mouse" instruction)
  world)

(defn run-instruction [world instruction]
  (let [instruction-name (subs instruction 0 (.indexOf instruction " "))]
    (if-let [function (-> (str "temp.core/run-"
                               instruction-name
                               "-instruction")
                          (symbol)
                          (resolve))]
      (do
        (println! instruction)
        (function world instruction))
      (do
        (println! "invalid instruction")
        world))))

(defn replay-mode-draw [world]
  (let [width 40
        height 40        
        y-offset (+ (* (:num-lines world) 16) 10 (* height 0.5))
        y (- (:window-height world) y-offset)
        x-offset (* width 0.5)
        x (- (:window-width world) x-offset)
        count (str (:instruction-index world))]
    (fill-rect! :black x y width height)
    (draw-text! :white count  (- x 8) (+ y 5) 15)))

(defn replay-mode-entered [world]
  (-> world
      (assoc-in [:instruction-index] 0)
      (assoc-in [:replay-filename] "res/decider.txt")))
  
(defn replay-pressed [world event]
  (assoc-in world [:replay-event] event))

(defn insert-instruction! [filename index instruction]
  (let [lines (with-open [rdr (clojure.java.io/reader filename)]
                (vec (line-seq rdr)))
        new-lines (vector-insert lines instruction index)]
    (spit filename (apply str (interpose "\n" new-lines)))))
  
(defn replay-released [world event]
  (let [e1 (:replay-event world)
        e2 event
        instruction (str "mouse " (dekeyword (:button e1)) " "
                         (:x e1) " " (:y e1) " "
                         (:x e2) " " (:y e2))]
    (insert-instruction! (:replay-filename world)
                         (:instruction-index world)
                         instruction)
    world))

(defn remove-mark [instruction]
  (if (.startsWith instruction "*")
    (subs instruction 2)
    instruction))

(defn replay-forward [world]
  (let [filename (:replay-filename world)
        lines (with-open [rdr (clojure.java.io/reader filename)]
                (vec (line-seq rdr)))
        index (:instruction-index world)
        instructions (cons (nth lines index)
                           (take-while #(.startsWith % "*")
                              (nthrest lines (inc index))))
        instructions (map remove-mark instructions)
        world (reduce (fn [w instruction]
                        (run-instruction w instruction))
                      world
                      instructions)]
    (assoc-in world [:instruction-index]
              (+ (:instruction-index world) (count instructions)))))

(defn replay-back [world]
  ;; (update-in world [:instruction-index] #(max (dec %) 0))
  (-> world
      (delete-all-parts)
      (reset-camera)
      (assoc-in [:instruction-index] 0)
      (tree-changed)))
