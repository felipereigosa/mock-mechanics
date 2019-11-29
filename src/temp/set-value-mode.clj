
(ns temp.core)

(defn set-part-value [world x y]
  (if-let [part-name (get-part-at world x y)]
    (let [part (get-in world [:parts part-name])]
      (println! "set value of " part-name)
      (read-input
       world
       (fn [w text]
         (let [value (parse-float text)
               value (if (= (:type part) :wagon)
                       (/ value
                          (reduce + (:track-lengths part)))
                       value)]
           (-> w
               (assoc-in [:parts part-name :value] value)
               (prepare-tree))))))
    world))

(defn set-value-mode-draw [world]
  (draw-text! :red "set value" 10 600 20))

(do
1  

(defn set-value-mode-pressed [world event]
  ;; (set-part-value world (:x event) (:y event))
  ;; (let [x (:x event)
  ;;       y (:y event)
  ;;       {:keys [part-name point]} (get-part-collision world x y)
  ;;       part (get-in world [:parts part-name])
  ;;       transform (:transform part)
  ;;       inverse-transform (get-inverse-transform transform)
  ;;       local-point (apply-transform inverse-transform point)]
  ;;   (assoc-in world [:collision] {:part-name part-name
  ;;                                 :point local-point}))
  (println! "set value pressed")
  world
  )

(defn set-value-mode-moved [world event]
  ;; (if-let [{:keys [part-name point]} (:collision world)]
  ;;   (let [x (:x event)
  ;;         y (:y event)
  ;;         line (unproject-point world [x y])
  ;;         part (get-in world [:parts part-name])]

  ;;     (apply-force world part-name [1 0 0])
  ;;     )
  ;;   world)
  world
  )

(defn set-value-mode-released [world event]
  ;; (dissoc-in world [:collision])
  world
  )
)

