
(ns temp.core (:gen-class))

(defn get-properties [world]
  (if-let [part-name (:selected-part world)]
    (let [part (get-in world [:parts part-name])
          properties (get-in world [:info (:type part) :properties])]
      (map (fn [property]
             (let [value (get-in part [property])
                   value (if (and
                              (= (:type part) :wagon)
                              (= property :value))
                           (* value (reduce + (:track-lengths part)))
                           value)
                   value (cond
                           (number? value) (format "%.2f" (float value))
                           (nil? value) "nil"
                           :else (str value))]
               [(name property) value]))
           (keys properties)))
    []))

(defn property-mode-entered [world]
  (assoc-in world [:selected-part] nil))

(defn property-mode-draw [world]
  (let [box (:property-box world)
        {:keys [x y w h image regions]} box
        properties (get-properties world)
        get-region (fn [name i]
                     (>> (str name i)
                         (keyword)
                         (get regions .)
                         (get-absolute-region . box)))]
    (draw-image! image x y)
    (dotimes [i (count properties)]
      (let [key-region (get-region "key" i)
            value-region (get-region "value" i)
            [key-text value-text] (nth properties i)]
        (draw-text-in-box! key-text :white 16 key-region)
        (draw-text-in-box! value-text :white 16 value-region)))))

(defn set-part-value [world key]
  (let [part-name (:selected-part world)
        part (get-in world [:parts part-name])]
    (read-input world
                (fn [w text]
                  (let [value (read-string text)
                        value (if (and (= (:type part) :wagon)
                                       (= key :value))
                                (/ value
                                   (reduce + (:track-lengths part)))
                                value)]
                    (-> w
                        (assoc-in [:parts part-name key] value)
                        (tree-changed)))))))

(defn set-property [world x y]
  (if-let [region (get-region-at (:property-box world) x y)]
    (let [index (read-string (str (last (str region))))
          properties (get-properties world)]
      (if (< index (count properties))
        (set-part-value world (keyword (first (nth properties index))))
        world))
    world))

(defn property-mode-pressed [world {:keys [x y]}]
  (if (inside-box? (:property-box world) x y)
    (set-property world x y)
    (let [{:keys [part-name point]} (get-part-collision world x y)
          part (get-in world [:parts part-name])
          world (-> world
                    (assoc-in [:selected-part] part-name)
                    (assoc-in [:press-time] (get-current-time))
                    (tree-will-change))]
      (case (:type part)
        :wagon
        (let [transform (:transform part)
              inverse-transform (get-inverse-transform transform)
              local-point (apply-transform inverse-transform point)
              mouse-line (unproject-point world [x y])]
          (assoc-in world [:force] {:part-name part-name
                                    :velocity 0
                                    :line mouse-line
                                    :point local-point}))
        :track
        (assoc-in world [:track-force] {:part-name part-name
                                        :point point
                                        :start-value (:value part)})        
        world))))

(defn special-track-moved [world x y]
  (let [{:keys [part-name point start-value]} (:track-force world)
        key (if (:use-weld-groups world)
              :weld-groups
              :parts)
        track (get-in world [key part-name])
        transform (:transform track)
        rotation (get-rotation-component transform)
        track-direction (apply-transform rotation [0 1 0])
        plane (get-camera-plane world point)
        [p0 p1 p2] plane
        v1 (vector-subtract p1 p0)
        v2 (vector-subtract p2 p0)
        plane-normal (vector-normalize (vector-cross-product v1 v2))
        line (unproject-point world [x y])
        p2 (line-plane-intersection line plane)
        side-vector (vector-normalize
                     (vector-cross-product track-direction plane-normal))
        side-line [point side-vector]
        p3 (point-line-projection p2 side-line)
        v (vector-subtract p3 point)
        s (/ (- (vector-dot-product v side-vector)) 2)
        new-value (+ start-value s)]
    (assoc-in world [:parts part-name :value] new-value)))

(defn property-mode-moved [world {:keys [x y]}]
  (cond
    (:force world)
    (let [mouse-line (unproject-point world [x y])
          part-name (get-in world [:force :part-name])
          wagon (get-in world [:parts part-name])
          new-value (:value wagon)
          loop-length (reduce + (:track-lengths wagon))]
      (-> world
          (assoc-in [:force :line] mouse-line)
          (redraw)))

    (:track-force world)
    (-> world
        (special-track-moved x y)
        (redraw))

    :else world))

(defn property-mode-released [world {:keys [x y]}]
  (let [box (:property-box world)
        world (if (and
                   (not (inside-box? box x y))
                   (< (- (get-current-time) (:press-time world)) 200))
                (select-part world (:selected-part world))
                world)]
    (-> world
        (dissoc-in [:force])
        (dissoc-in [:track-force])
        (tree-changed))))
