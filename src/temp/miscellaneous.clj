
(ns temp.core)

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

(declare get-parts-with-type)

(defn set-probe-values [world]
  (let [probe-names (get-parts-with-type (:parts world) :probe)
        positions (map (fn [probe-name]
                         (let [probe (if (:use-weld-groups world)
                                       (get-in world [:weld-groups probe-name])
                                       (get-in world [:parts probe-name]))]
                           (get-transform-position (:transform probe))))
                       probe-names)
        close-pairs (mapcat (fn [i]
                              (map (fn [j]
                                     (let [p1 (nth positions i)
                                           p2 (nth positions j)
                                           d (distance p1 p2)]
                                       (if (< d 0.12)
                                         [i j]
                                         nil)))
                                   (range (inc i) (count probe-names))))
                            (range (dec (count probe-names))))
        close-indices (flatten (filter not-nil? close-pairs))
        close-probes (map #(nth probe-names %) close-indices)]
    (reduce (fn [w probe-name]
              (if (in? probe-name close-probes)
                (assoc-in w [:parts probe-name :value] 1)
                (assoc-in w [:parts probe-name :value] 0)))
            world
            probe-names)))

(defn draw-buttons! [world]
  (let [button-names (get-parts-with-type (:parts world) :button)]
    (doseq [button-name button-names]
      (let [button (get-in world [:parts button-name])
            base-transform (:transform button)
            rotation (get-transform-rotation (:transform button))
            rotation-transform (make-transform [0 0 0] rotation)
            up (apply-transform rotation-transform [0 1 0])
            offset (if (= (:value button) 1)
                     (make-transform (vector-multiply up 0.02) [1 0 0 0])
                     (make-transform (vector-multiply up 0.1) [1 0 0 0]))
            transform (combine-transforms base-transform offset)
            property (nth (get-in world [:properties])
                          (:selected-property world))
            color (if (not= (:mode world) :toggle)
                    (:color button)
                    (if (get-in button [property])
                      :red
                      :white))
            mesh (-> (:button-mesh world)
                     (assoc-in [:transform] transform)
                     (assoc-in [:color] (get-color-vector color)))]
        (draw-mesh! world mesh)))))

(defn draw-lamps! [world]
  (let [lamp-names (get-parts-with-type (:parts world) :lamp)]
    (doseq [lamp-name lamp-names]
      (let [lamp (get-in world [:parts lamp-name])
            base-transform (:transform lamp)
            property (nth (get-in world [:properties])
                          (:selected-property world))
            color (if (and (= (:value lamp) 0)
                           (in? (:mode world) [:idle :cpu]))
                    (:dark-color lamp)
                    (:color lamp))
            
            color (if (not= (:mode world) :toggle)
                    color
                    (if (get-in lamp [property])
                      :red
                      :white))
            mesh (-> (:lamp-mesh world)
                     (assoc-in [:transform] base-transform)
                     (assoc-in [:color] (get-color-vector color)))]
        (draw-mesh! world mesh)))))

(declare create-weld-groups)
(declare compute-transforms)
(declare get-tail-transform)
(declare get-part-position)

(defn prepare-tree [world]
  ;; (if (= (:mode world) :idle)
  ;;   world
    (-> world
        (compute-transforms :parts)
        (create-weld-groups)))

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

