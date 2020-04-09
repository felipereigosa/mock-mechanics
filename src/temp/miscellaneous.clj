
(ns temp.core)

(declare get-parts-with-type)
(declare create-weld-groups)
(declare compute-transforms)
(declare get-tail-transform)
(declare get-part-position)
(declare save-version)
(declare load-last-version-callback)
(declare undo!)
(declare redo!)
(declare show-hint)
(declare change-mode)

(defn update-move-plane [world]
  (assoc-in world [:move-plane]
            (get-camera-plane world (get-in world [:camera :pivot]))))

(defn get-function-value [function t interpolator]
  (let [final-time (first (last function))]
    (cond
      (<= t 0.0) (last (first function))
      (>= t final-time) (last (last function))
      :else
      (let [pairs (map vector function (rest function))
            pair (find-if (fn [[[t0 & _] [t1 & _]]]
                            (<= t0 t t1))
                          pairs)
            t0 (first (first pair))
            t1 (first (second pair))
            s (map-between-ranges t t0 t1 0 1)
            v0 (second (first pair))
            v1 (second (second pair))]
        (interpolator v0 v1 s)))))

(defn read-input [world callback]
  (-> world
      (assoc-in [:input-callback] callback)
      (assoc-in [:text-input] true)))

(defn set-pivot [world event]
  (let [x (:x event)
        y (:y event)
        part-name (get-part-at world x y)
        part (get-in world [:parts part-name])
        pos (cond
              (nil? part-name)
              (let [line (unproject-point world [x y])
                    ground-plane [[0 0 0] [1 0 0] [0 0 1]]]
                (line-plane-intersection line ground-plane))

              (= (:type part) :track)
              (get-transform-position (get-tail-transform part))
              
              :else
              (get-part-position world part-name))]
    (compute-camera (assoc-in world [:camera :pivot] pos))))

(defn new-file [world]
  ;; (let [ground-part ;; (assoc-in (:ground-part world) [:children] {})
  ;;       (:ground-part world)
  ;;       ]
  ;;   (-> world
  ;;       (assoc-in [:command] "")
  ;;       (assoc-in [:mode] :idle)
  ;;       (assoc-in [:parts] {:ground-part ground-part})
  ;;       (assoc-in [:planet] (create-planet))
  ;;       (update-in [:planet] create-ground)
  ;;       (reset-camera)
  ;;       (update-move-plane)
  ;;       (prepare-tree)
  ;;       (save-checkpoint!)))
  (let [world (create-world)]
    (set-title! "-")
    (redraw world)))

(defn place-box [world name & {:keys [rx ry wx wy ox oy]}]
  (let [{:keys [x y w h]} (get-in world [name])
        window-width (:window-width world)
        window-height (:window-height world)

        x (if (nil? rx)
            (* window-width wx)
            (let [n (neg? rx)
                  rx (abs rx)
                  x (* rx w)]
              (if n
                (- window-width x)
                x)))

        y (if (nil? ry)
            (* window-height wy)
            (let [n (neg? ry)
                  ry (abs ry)
                  y (* ry h)]
              (if n
                (- window-height y)
                y)))
        ox (or ox 0)
        oy (or oy 0)]
    (-> world
        (assoc-in [name :x] (+ x ox))
        (assoc-in [name :y] (+ y oy)))))

(defn place-elements [world]
  (-> world
      (place-box :action-menu :rx 0.6 :ry 0.5 :oy 10)
      (place-box :mode-menu :rx -0.6 :ry 0.5 :oy 10)
      (place-box :insert-menu :wx 0.5 :ry -0.6 :oy -25)
      (place-box :color-palette :wx 0.5 :ry -0.5 :oy -25)
      (place-box :edit-menu :wx 0.5 :ry -0.6 :oy -25)
      (place-box :layer-box :wx 0.5 :ry -0.5 :oy -25)
      (place-box :graph-box :wx 0.5 :ry -0.5 :oy -25)
      (place-box :cpu-box :wx 0.5 :ry -0.5 :oy -25)
      (place-box :toggle-box :wx 0.5 :ry -0.5 :oy -25)
      ))

