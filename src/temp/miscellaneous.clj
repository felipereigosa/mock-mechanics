
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

(defn create-image [filename x y w h]
  (let [document (read-xml filename)
        image (if (= w -1)
                (parse-svg-from-map-with-height document h)
                (parse-svg-from-map-with-width document w))
        menu {:x x
              :y y
              :w (get-image-width image)
              :h (get-image-height image)
              :image image}
        regions (get-absolute-svg-regions document menu)]
    (assoc-in menu [:regions] regions)))

(defn get-region-at [image x y]
  (first (find-if (fn [[name box]]
                    (inside-box? box x y))
                  (:regions image))))

(defn draw-buffer! [world]
  (let [y 693
        helper (fn [text border]
                 (fill-rect! :black 80 y 2000 25)
                 (if border
                   (draw-text! :red "=" 15 (+ y 4) 14))
                 (draw-text! :blue text 30 (+ y 4) 14))]
    (cond
      (:text-input world)
      (helper (str (:text world)) true)

      (not (empty? (:command world)))
      (helper (:command world) false))))

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
    (redraw!) ;;#################################
    world))

(defn action-menu-pressed [world x y]
  (if-let [region (get-region-at (:action-menu world) x y)]
    (case region
      :new (new-file world)
      :view (reset-camera world)
      :save (save-version world)
      :load (read-input world load-last-version-callback)
      :undo (undo! world)
      :redo (redo! world))
    world))

(defn mode-menu-pressed [world x y]
  (if-let [region (get-region-at (:mode-menu world) x y)]
    (-> world
        (change-mode region)
        (show-hint :mode region))
    world))

(defn place-box [world name & {:keys [rx ry wx wy]}]
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
              (println! (- window-height y))
              (if n
                (- window-height y)
                y)))]
    (-> world
        (assoc-in [name :x] x)
        (assoc-in [name :y] y)
        ;; (assoc-in [name :regions] regions)))
        ;; regions (get-absolute-svg-regions document menu)]
        )))
    
  
