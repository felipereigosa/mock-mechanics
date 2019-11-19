
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

(defn set-value-mode-pressed [world event]
  (set-part-value world (:x event) (:y event)))
  
