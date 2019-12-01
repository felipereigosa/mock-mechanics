
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

(defn get-weld-parent-part [world part-name]
  (let [weld-groups (:weld-groups world)]
    (find-if (fn [name]
               (let [children (get-in weld-groups [name :children])]
                 (in? part-name (keys children))))
             (keys weld-groups))))

(defn get-function-segment [function t]
  (cond
    (<= t 0.0) [(nth function 0) (nth function 1)]

    (>= t 1.0)
    (let [n (count function)]
      [(nth function (- n 2)) (nth function (- n 1))])

    :else
    (let [pairs (map vector function (rest function))]
      (find-if (fn [[[t0 & _] [t1 & _]]]
                 (<= t0 t t1))
               pairs))))

(defn get-wagon-direction [world wagon-name]
  (let [wagon (get-in world [:parts wagon-name])
        loop-fn (if (:use-weld-groups world)
                  (compute-translated-loop-fn (:loop-fn wagon))
                  (:loop-fn wagon))
        parent-name (if (:use-weld-groups world)
                      (do
                        (println! "direction error")
                        (get-weld-parent-part world wagon-name))
                      
                      (get-parent-part world wagon-name))

        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        loop-fn (map (fn [[t v]]
                       [t (apply-transform parent-transform v)])
                     loop-fn)
        value (within (:value wagon) 0.0 1.0)
        [[_ p0] [_ p1]] (get-function-segment loop-fn value)]
    (vector-normalize (vector-subtract p1 p0))))

(defn set-value-mode-pressed [world event]
  ;; (set-part-value world (:x event) (:y event))
  (let [x (:x event)
        y (:y event)
        {:keys [part-name point]} (get-part-collision world x y)
        part (get-in world [:parts part-name])
        transform (:transform part)
        inverse-transform (get-inverse-transform transform)
        local-point (apply-transform inverse-transform point)
        mouse-line (unproject-point world [x y])]
    (assoc-in world [:force] {:part-name part-name
                              :velocity 0
                              :line mouse-line
                              :point local-point})))

(defn set-value-mode-moved [world event]
  (if (nil? (:force world))
    world
    (let [x (:x event)
          y (:y event)
          mouse-line (unproject-point world [x y])]
      (assoc-in world [:force :line] mouse-line))))

(defn set-value-mode-released [world event]
  (dissoc-in world [:force]))

(defn set-value-mode-pressed [world event]
  ;; (set-part-value world (:x event) (:y event))
  (let [x (:x event)
        y (:y event)
        {:keys [part-name point]} (get-part-collision world x y)
        part (get-in world [:parts part-name])
        transform (:transform part)
        inverse-transform (get-inverse-transform transform)
        local-point (apply-transform inverse-transform point)
        mouse-line (unproject-point world [x y])]
    (assoc-in world [:force] {:part-name part-name
                              :velocity 0
                              :line mouse-line
                              :point local-point})))

(defn set-value-mode-moved [world event]
  (if (nil? (:force world))
    world
    (let [x (:x event)
          y (:y event)
          mouse-line (unproject-point world [x y])]
      (assoc-in world [:force :line] mouse-line))))

(defn set-value-mode-released [world event]
  (dissoc-in world [:force]))

(defn apply-force [world elapsed]
  (if-let [{:keys [part-name velocity line point]} (:force world)]
    (let [wagon-name part-name ;;#################
          wagon (get-in world [:parts wagon-name])
          transform (:transform wagon)
          p1 (apply-transform transform point)
          p2 (point-line-projection p1 line)
          force-vector (vector-subtract p2 p1)
          track-direction (get-wagon-direction world wagon-name)
          force-component (/ (vector-dot-product force-vector track-direction)
                             (vector-length track-direction))
          acceleration (* force-component 100)
          value (get-in world [:parts part-name :value])
          dt (* elapsed 0.001)
          dv (* acceleration dt)
          dampening-factor 0.80
          velocity (* (+ velocity dv) dampening-factor)
          dvalue (* velocity dt)
          value (+ value dvalue)]
      (-> world
          (assoc-in [:parts part-name :value] value)
          (assoc-in [:force :velocity] velocity)))
    world))

(clear-output!)
)

;; (set-thing! [:parts (get-part-with-color @world :green) :value]
;;             0.25)

;; (set-thing! [:parts (get-part-with-color @world :yellow) :value]
;;             0.1)
;; (set-thing! [:force] nil)
;; (clear-output!)

;; (set-thing! [:use-weld-groups] false)


