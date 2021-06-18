
(import java.awt.Robot)
(import java.awt.event.InputEvent)

(require '[clojure.java.shell :refer [sh]])
(require '[clojure.set :refer [difference]])

;;----------------------------------------------------------------------;;
;; .obj export/import

(defn export-path [points filename]
  (let [points (read-string (str "(" points ")"))
        final-time (last (last points))
        w @window-width
        h @window-height
        points (concat [[w 0 0] [w h 0] [0 h 0]] points)
        ratio (float (/ w h))]
    (with-open [writer (clojure.java.io/writer filename)]
      (.write writer (format "# final-time %s\n" final-time))
      (.write writer (format "# width %s\n" w))
      (.write writer "o Path\n")
      (doseq [[x y t] points]
        (.write writer (format "v %s %s %s\n"
                               (/ x (float w))
                               (/ t (float final-time))
                               (/ y (float w))
                               )))
      (doseq [i (range 1 (count points))]
        (if (not= i 3)
          (.write writer (format "f %s %s\n" i (inc i))))))))

(defn import-path [filename]
  (let [lines (with-open [rdr (clojure.java.io/reader filename)]
                (vec (line-seq rdr)))
        [_ [_ _ ratio] _ & vertices]
        (map parse-line
             (filter (fn [line]
                       (.startsWith line "v "))
                     lines))

        vertices (sort-by second vertices)
        final-time (>> lines
                       (find-if #(.startsWith % "# final-time") .)
                       (subs .  13)
                       (read-string .))
        width (>> lines
                  (find-if #(.startsWith % "# width") .)
                  (subs .  8)
                  (read-string .))]
    (println!
     (map (fn [[x y z]]
            [(round (* x width))
             (round (* z width))
             (round (* y final-time))])
          vertices))))

;;----------------------------------------------------------------------;;
;; robot

(defn get-window-coordinates []
  (map parse-int
       (clojure.string/split (:out (sh "./window-coords.sh")) #"\n")))

(def robot (atom nil))

(defn robot-move [[x y]]
  (let [{:keys [robot origin]} @robot
        [ox oy] origin]
    (.mouseMove robot (+ ox x) (+ oy y))))

(defn get-button-mask [name]
  (case name
    :right InputEvent/BUTTON3_DOWN_MASK
    :left InputEvent/BUTTON1_DOWN_MASK
    :middle InputEvent/BUTTON2_DOWN_MASK))

(defn robot-set-active! [value]
  (swap! robot
         (fn [r]
           (assoc-in r [:active] value))))

(defn robot-mouse-press [button]
  (.mousePress (:robot @robot) (get-button-mask button)))

(defn robot-mouse-release [button]
  (.mouseRelease (:robot @robot) (get-button-mask button)))

;;----------------------------------------------------------------------;;
;; compile

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
        properties (concat
                    (keys (get-in world [:info type :properties]))
                    (filter #(not= % :.) (:properties world))
                    '(:color))
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
                                 value)))))
    (if (and (= type :chip)
             (not (empty? (:functions part))))
      (.write writer (format "set-functions of %s to %s\n"
                             (dekeyword part-name)
                             (:functions part))))

    (if (and (= type :motherboard)
             (not (empty? (:pins part))))
      (.write writer (format "set-graph of %s to %s\n"
                       (dekeyword part-name)
                       (select-keys part [:pins :gates :connections]))))
    ))

(defn get-part-number [part-name]
  (parse-int (second (re-find #":[a-z]*([0-9]*)" (str part-name)))))

(defn create-instructions [world filename]
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
        filename (str "res/" filename ".txt")]
    (with-open [writer (clojure.java.io/writer filename)]
      (doseq [part-name sorted-names]
        (create-part-instructions writer world part-name)))
    (println! "created instructions")
    world))

;; ;;----------------------------------------------------------------------;;
;; ;; mouse events

;; (defn insert-instruction! [filename index instruction]
;;   (let [lines (with-open [rdr (clojure.java.io/reader filename)]
;;                 (vec (line-seq rdr)))
;;         new-lines (vector-insert lines instruction index)]
;;     (spit filename (apply str (interpose "\n" new-lines)))))

;; (defn change-event [event start-time]
;;   [(int (:x event)) (int (:y event)) (- (get-current-time) start-time)])

;; (defn replay-pressed [world event]
;;   (if (:replay-filename world)
;;     ;; (if (:active @robot)
;;     ;;   world
;;     ;;   (-> world
;;     ;;       (assoc-in [:replay-button] (dekeyword (:button event)))
;;     ;;       (assoc-in [:replay-events]
;;     ;;                 [(change-event event (:press-time world))])))
;;     (do
;;       ;; (println! "pressed" event)
;;       world)
;;     world))

;; (defn replay-moved [world event]
;;   (if (:replay-filename world)
;;     ;; (if (not-nil? (:replay-events world))
;;     ;;   (update-in world [:replay-events]
;;     ;;              #(conj % (change-event event (:press-time world))))
;;     ;;   world)
;;     (do
;;       ;; (println! "moved" event)
;;       world)
;;     world))

;; (defn replay-released [world event]
;;   (if (:replay-filename world)
;;     ;; (if (:active @robot)
;;     ;;   world
;;     ;;   (let [points (conj (:replay-events world)
;;     ;;                      (change-event event (:press-time world)))
;;     ;;         button (:replay-button world)
;;     ;;         instruction (str "mouse " button " " (join " " points))]
;;     ;;     (insert-instruction! (str "res/" (:replay-filename world) ".txt")
;;     ;;                          (:instruction-index world)
;;     ;;                          instruction)
;;     ;;     (-> world
;;     ;;         (dissoc-in [:replay-events])
;;     ;;         (update-in [:instruction-index] inc))))
;;     (do
;;       ;; (println! "released" event)
;;       world)
;;     world))

;;----------------------------------------------------------------------;;
;; run

(defn update-history [world]
  (update-in world [:replay-history]
             #(conj % {:parts (:parts world)
                       :camera (:camera world)})))

(do
1

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
        (assoc-in [:mode] :add)
        (assoc-in [:add-type] type)
        (assoc-in [:parts parent-name :children part-name] transform)
        (assoc-in [:parts part-name] part)    
        (compute-transforms :parts)
        (update-history)
        (tree-changed))))

;; (defn run-set-instruction [world instruction]
;;   (let [[_ property-name
;;          _ part-name
;;          _ value] (read-string (str "[" instruction "]"))
;;         property-name (keyword property-name)
;;         part-name (keyword part-name)
;;         value (if (symbol? value) (keyword value) value)]
;;     (-> world
;;         (set-part-value part-name property-name (str value))
;;         (update-history)
;;         (tree-changed))))

(defn scale-animation [world animation]
  (let [{:keys [t start-scale final-scale
                start-transform final-transform
                part-name parent-name]} animation]
    (cond
      (float= t 0.0)
      (tree-will-change world)

      (float= t 1.0)
      (-> world
          (assoc-in [:parts part-name :scale] final-scale)
          (assoc-in [:parts part-name :transform] final-transform)
          (create-relative-transform part-name parent-name)
          (update-history)
          (tree-changed))

      :else
      (let [scale (vector-interpolate start-scale final-scale t)
            transform (interpolate-transforms start-transform
                                              final-transform t)]
        (-> world
            (assoc-in [:parts part-name :scale] scale)
            (assoc-in [:parts part-name :transform] transform))))))

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
        offset-transform (make-transform offset [1 0 0 0])
        transform (:transform part)
        final-transform (combine-transforms transform offset-transform)]
    (-> world
        (assoc-in [:mode] :edit)
        (assoc-in [:edit-subcommand] :scale)
        (assoc-in [:animation]
                  {:t 0.0
                   :time 0.3
                   :fn scale-animation
                   :part-name part-name
                   :parent-name parent-name
                   :start-scale (:scale part)
                   :final-scale (vector-add (:scale part) change)
                   :start-transform transform
                   :final-transform final-transform}))))

(defn run-set-color-instruction [world instruction]
  (let [[_ _ _ part-name _ color] (read-string (str "[" instruction "]"))
        color (keyword color)
        part-name (keyword part-name)]
    (println! part-name color)
    (-> world
      (assoc-in [:mode] :color)
      (assoc-in [:current-color] color)
      (assoc-in [:parts part-name :color] color)
      (update-history)
      (tree-changed))))

;; (defn run-set-functions-instruction [world instruction]
;;   ;; (let [[_ property-name
;;   ;;        _ part-name
;;   ;;        _ value] (read-string (str "[" instruction "]"))
;;   ;;       property-name (keyword property-name)
;;   ;;       part-name (keyword part-name)
;;   ;;       value (if (symbol? value) (keyword value) value)]
;;   ;;   (-> world
;;   ;;       (set-part-value part-name property-name (str value))
;;   ;;       (update-history)
;;   ;;       (tree-changed)))
;;   (println! "set functions")
;;   world
;;   )

;; (defn run-set-graph-instruction [world instruction]
;;   ;; (let [[_ property-name
;;   ;;        _ part-name
;;   ;;        _ value] (read-string (str "[" instruction "]"))
;;   ;;       property-name (keyword property-name)
;;   ;;       part-name (keyword part-name)
;;   ;;       value (if (symbol? value) (keyword value) value)]
;;   ;;   (-> world
;;   ;;       (set-part-value part-name property-name (str value))
;;   ;;       (update-history)
;;   ;;       (tree-changed)))
;;   (println! "set graph")
;;   world
;;   )

;; (defn interpolate-mouse [start end]
;;   (let [ts (last start)
;;         te (last end)
;;         start (take 2 start)
;;         end (take 2 end)
;;         d (distance start end)
;;         v (vector-subtract end start)
;;         interval (- te ts)
;;         dt 20.0
;;         num-steps (int (/ interval dt))
;;         extra-time (int (mod interval dt))]
;;     (robot-move start)
;;     (dotimes [i (dec num-steps)]
;;       (sleep dt)
;;       (robot-move (vector-add start (vector-multiply v (/ i num-steps)))))
;;     (sleep extra-time)
;;     (robot-move end)))

;; (defn run-mouse-instruction [world instruction]
;;   (let [[_ button & points] (read-string (str "(" instruction ")"))
;;         button (keyword button)]

;;     (.start
;;      (new Thread
;;           (proxy [Runnable] []
;;             (run []
;;               (robot-set-active! true)
;;               (robot-move (take 2 (first points)))
;;               (robot-mouse-press button)
;;               (dotimes [i (dec (count points))]
;;                 (interpolate-mouse (nth points i)
;;                                    (nth points (inc i))))
;;               (sleep 16)
;;               (robot-mouse-release button)
;;               (sleep 100) ;;##########################
;;               (robot-set-active! false)
;;               ))))
;;     world))

;; (defn test-animation [world animation] ;;##########################
;;   (let [{:keys [t]} animation]
;;     (cond
;;       (float= t 0.0)
;;       (do
;;         (println! "start")
;;         (assoc-in world [:mode] :add))

;;       (< 0.48 t 0.5)
;;       (let [part (create-part :block :white 0 (:info world))
;;             transform (make-transform [0.25 0.35 0.25] [1 0 0 0])]
;;         (println! "middle")
;;         (-> world
;;           (assoc-in [:parts :ground-part :children :block0] transform)
;;           (assoc-in [:parts :block0] part)    
;;           (compute-transforms :parts)
;;           (tree-changed)))

;;       (float= t 1.0)
;;       (let [part (create-part :block :red 0 (:info world))
;;             transform (make-transform [0 0.5 0] [1 0 0 0])]
;;         (println! "done")
;;         (-> world
;;           (assoc-in [:parts :block0 :children :block1] transform)
;;           (assoc-in [:parts :block1] part)    
;;           (compute-transforms :parts)
;;           (tree-changed)))

;;       :else
;;       (do
;;         (println! t)
;;         world
;;         ))))

;; set mode
;; wait x
;; do animation
;; wait y
;; do second animation

(defn run-instruction [world instruction]
  (let [words (split instruction #" ")
        instruction-name (if (= (first words) "set")
                           (str (first words) "-" (second words))
                           (first words))]
    (if-let [function (-> (str "temp.core/run-"
                               instruction-name
                               "-instruction")
                          (symbol)
                          (resolve))]
      (do
        (println! (subs instruction 0 (min 100 (count instruction))))
        (function world instruction))
      (do
        (println! "invalid instruction")
        world)))

  ;; (assoc-in world [:animation]
  ;;   {:t 0.0
  ;;    :time 3
  ;;    :fn test-animation
  ;;    })
  )

(defn replay-draw [world]
  (let [width 40
        height 40        
        y-offset (+ (* (:num-lines world) 16) 10 (* height 0.5))
        y (- (:window-height world) y-offset)
        x-offset (* width 0.5)
        x (- (:window-width world) x-offset)
        count (str (:instruction-index world))]
    (fill-rect! :black x y width height)
    (fill-rect! :black (- x 50) y 150 height)
    (draw-text! :white (:replay-filename world) (- x 100) (+ y 5) 15)
    (draw-text! :white count  (- x 8) (+ y 5) 15)))

(defn toggle-replay [world]
  (if (:replay-filename world)
    (dissoc-in world [:replay-filename])
    (do
      (when (nil? @robot)
        (reset! robot {:robot (new Robot)
                       :origin (get-window-coordinates)
                       :active false}))
      (read-input world
        (fn [w text]
          (-> w
            (assoc-in [:replay-filename] text)
            (assoc-in [:instruction-index] 0)
            (assoc-in [:replay-history] [])
            (update-history)))))))

(defn remove-mark [instruction]
  (if (.startsWith instruction "*")
    (subs instruction 2)
    instruction))

(defn replay-forward [world]
  (if (and
        (:replay-filename world)
        (nil? (:animation world)))
    (let [filename (str "res/" (:replay-filename world) ".txt")
          lines (with-open [rdr (clojure.java.io/reader filename)]
                  (vec (line-seq rdr)))
          index (:instruction-index world)]
      (if (< index (count lines))
        (let [instructions (cons (nth lines index)
                                 (take-while #(.startsWith % "*")
                                             (nthrest lines (inc index))))
              instructions (map remove-mark instructions)
              world (reduce (fn [w instruction]
                              (run-instruction w instruction))
                            world
                            instructions)]
          (assoc-in world [:instruction-index]
                    (+ (:instruction-index world) (count instructions))))
        world))
    world))

(defn replay-back [world]
  (if (and
        (:replay-filename world)
        (> (:instruction-index world) 0))
    (let [new-history (pop (:replay-history world))
          filename (str "res/" (:replay-filename world) ".txt")
          lines (with-open [rdr (clojure.java.io/reader filename)]
                  (vec (line-seq rdr)))
          instruction (nth lines (dec (:instruction-index world)))]
      (println! "<<" (subs instruction 0 (min 100 (count instruction))))
      (-> world
          (assoc-in [:parts] (:parts (last new-history)))
          (assoc-in [:camera] (:camera (last new-history)))
          (compute-camera)
          (assoc-in [:replay-history] new-history)
          (update-in [:instruction-index] dec)
          (tree-changed)))
    world))

;; (clear-output!)
;; nil
)
