(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

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

(declare activate-chip)

(defn set-skin [world part-name value]
  (if (empty? value)
    (-> world
        (assoc-in [:parts part-name :skin] value)
        (dissoc-in [:parts part-name :model])
        (tree-changed))
    (-> world
        (assoc-in [:parts part-name :skin] value)
        (assoc-in [:parts part-name :model]
                  (create-model-mesh (str "res/" value ".obj")
                                     [0 0 0] [1 0 0 0] [1 1 1] nil))
        (assoc-in [:parts part-name :white-model]
                  (create-model-mesh (str "res/" value ".obj")
                                     [0 0 0] [1 0 0 0] [1 1 1] :white))
        (tree-changed))))

(defn set-part-value [world part-name key text]
  (let [part (get-in world [:parts part-name])
        value (try
                (read-string text)
                (catch Exception e ""))
        value (if (symbol? value)
                text
                value)]
    (case key
      :value
      (let [world (case (:type part)
                    :wagon
                    (assoc-in world [:parts part-name key]
                              (/ value (reduce + (:track-lengths part))))

                    :chip
                    (-> world
                        ((fn [w] (if (float= value 1.0)
                                   (activate-chip w part-name)
                                   w)))
                        (assoc-in [:parts part-name key] value))

                    :speaker
                    (-> world
                        ((fn [w]
                           (let [note (get-note (:frequency part))]
                             (if (float= value 1.0)
                               (note-on note)
                               (note-off note)))
                           w))
                        (assoc-in [:parts part-name key] value))

                    (assoc-in world [:parts part-name key] value))]
        (-> world
            (enforce-gears part-name)
            (tree-changed)))

      :skin (set-skin world part-name value)

      :free
      (-> world
          (assoc-in [:parts part-name :free] value)
          (tree-changed))

      (assoc-in world [:parts part-name key] value))))

(defn set-property [world x y]
  (if-let [region (get-region-at (:property-box world) x y)]
    (let [index (read-string (str (last (str region))))
          properties (get-properties world)]
      (if (< index (count properties))
        (let [part-name (:selected-part world)
              key (keyword (first (nth properties index)))]
          (read-input world #(set-part-value % part-name key %2)))
        world))
    world))

(defn special-track-moved [world spec]
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
        v1 (vector/subtract p1 p0)
        v2 (vector/subtract p2 p0)
        plane-normal (vector/normalize (vector/cross-product v1 v2))
        line (get-spec-line world spec)
        p2 (line-plane-intersection line plane)
        side-vector (vector/normalize
                      (vector/cross-product track-direction plane-normal))
        side-line [point side-vector]
        p3 (point-line-projection p2 side-line)
        v (vector/subtract p3 point)
        s (/ (- (vector/dot-product v side-vector)) 2)
        new-value (+ start-value s)]
    (assoc-in world [:parts part-name :value] new-value)))

(defn property-mode-pressed [world {:keys [x y] :as event}]
  (if (inside-box? (:property-box world) x y)
    (set-property world x y)
    (let [{:keys [part-name point]} (get-part-collision world event)
          part (get-in world [:parts part-name])
          world (-> world
                    (assoc-in [:selected-part] part-name)
                    (assoc-in [:press-time] (get-current-time))
                    (tree-will-change))]
      (case (:type part)
        :wagon
        (let [transform (:transform part)
              inverse-transform (get-inverse-transform transform)
              local-point (apply-transform inverse-transform point)]
          (assoc-in world [:mouse-force]
                    {:part-name part-name
                     :local-point local-point
                     :line (get-spec-line world event)}))

        :track
        (assoc-in world [:track-force] {:part-name part-name
                                        :point point
                                        :start-value (:value part)})
        world))))

(defn property-mode-moved [world event]
  (cond
    (:mouse-force world)
    (-> world
        (assoc-in [:mouse-force :line] (get-spec-line world event))
        (redraw))

    (:track-force world)
    (-> world
        (special-track-moved event)
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
        (dissoc-in [:mouse-force])
        (dissoc-in [:track-force])
        (tree-changed))))
