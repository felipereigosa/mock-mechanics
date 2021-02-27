
(ns temp.core (:gen-class))

(declare get-body-transform)
(declare make-sphere)

(defn get-simple-transform [transform]
  {:position (get-transform-position transform)
   :rotation (get-transform-rotation transform)})

(defn get-simple-color [color]
  (if (keyword? color)
    color
    [(.getRed color)
     (.getGreen color)
     (.getBlue color)]))

(defn get-simple-texture [texture]
  (let [image (:image texture)
        width (/ (get-image-width image) 20)
        height (/ (get-image-height image) 20)]
    (vec (map (fn [y]
                (vec (map (fn [x]
                            (let [color (get-pixel image (* x 20) (* y 20))]
                              (get-color-vector color)))
                          (range width))))
              (range height)))))

(defn modify-field [object key function]
  (if-let [value (get-in object [key])]
    (update-in object [key] function)
    object))

(defn get-simple-part [part]
  (let [children (map-map (fn [[name transform]]
                            {name (get-simple-transform transform)})
                          (:children part))]
    (-> part
        (dissoc-in [:transform])
        (dissoc-in [:model])
        (modify-field :color get-simple-color)
        (modify-field :dark-color get-simple-color)
        (modify-field :texture get-simple-texture)
        (assoc-in [:children] children))))

(defn get-complex-transform [transform]
  (make-transform (:position transform) (:rotation transform)))

(defn get-complex-color [color]
  (if (vector? color)
    (apply make-color color)
    color))

(defn get-complex-texture [texture colors]
  (let [width (count (first colors))
        height (count colors)]
    (dotimes [x width]
      (dotimes [y height]
        (let [cs (get-in colors [y x])
              [r g b _] (map #(int (* 255 %)) cs)]
        (fill-rect (:image texture) (make-color r g b)
                   (+ 10 (* x 20)) (+ 10 (* y 20))
                   20 20))))
    (reset-texture texture)))

(defn get-complex-part [part info]
  (let [children (map-map (fn [[name transform]]
                            {name (get-complex-transform transform)})
                          (:children part))
        properties (get-in info [(:type part) :properties])
        part (merge-with (fn [a b] a) part properties)
        part (if (= (:type part) :chip)
               (assoc-in part [:time] 10000)
               part)
        part (if (= (:type part) :display)
               (let [colors (:texture part)]
                 (-> part
                     (create-display-texture)
                     (modify-field :texture #(get-complex-texture % colors))))
               part)]
    (-> part
        (assoc-in [:transform] (make-transform [0 0 0] [1 0 0 0]))
        (modify-field :dark-color get-complex-color)
        (modify-field :color get-complex-color)
        (assoc-in [:children] children))))

(defn extract-number [name]
  (let [r-index (.indexOf name "_")
        l-index (.indexOf name "." (inc r-index))]
    (if (= l-index -1)
      nil
      (parse-int (subs name (inc r-index) l-index)))))

(defn get-last-version-filename [root-name]
  (let [all-filenames (get-files-at "machines/")
        filenames (filter #(.startsWith % (str root-name "_"))
                          all-filenames)
        number (->> (map extract-number filenames)
                    (apply max))]
    (str "machines/" root-name "_" (format "%03d" number) ".mch")))

(defn increment-filename [filename]
  (let [number (extract-number filename)
        prefix (subs filename 0 (.lastIndexOf filename "_"))]
    (str prefix "_" (format "%03d" (inc number)) ".mch")))

(defn save-machine [world text]
  (let [filename (if (:last-saved-machine world)
                   (increment-filename (get-last-version-filename text))
                   (str "machines/" text "_000.mch"))
        parts (map-map (fn [[name part]]
                         {name (get-simple-part part)})
                       (:parts world))
        sphere-transforms (vec (map (comp get-simple-transform get-body-transform)
                                    (:spheres world)))]
    (spit filename {:parts parts
                    :camera (:camera world)
                    :gears (:gears world)
                    :visible-layers (:visible-layers world)
                    :layer-names (:layer-names world)
                    :sphere-transforms sphere-transforms})
    (user-message! "saved " filename)
    (set-title! text)
    (assoc-in world [:last-saved-machine] text)))

(defn save-machine-version [world]
  (if-let [root-name (:last-saved-machine world)]
    (save-machine world root-name)
    (read-input world #(save-machine %1 %2))))

(defn create-spheres [world sphere-transforms]
  (assoc-in world [:spheres]
            (vec (map (fn [{:keys [position rotation]}]
                        (make-sphere world position rotation))
                      sphere-transforms))))

(declare add-gear-models)
(declare add-gear-and-rack-models)

(defn recreate-gears [world entry]
  (let [[a b] (first entry)
        {:keys [radius-1 radius-2 radius
                length angle-offset ratio]} (second entry)]
    (if (nil? length)
      (add-gear-models
       world ratio a radius-1 b radius-2 angle-offset)
      (add-gear-and-rack-models
       world a radius b length angle-offset))))

(defn set-layer-info [world visible-layers layer-names]
  (let [world (if visible-layers
                (assoc-in world [:visible-layers] visible-layers)
                world)
        world (if layer-names
                (assoc-in world [:layer-names] layer-names)
                world)]
    world))

(defn open-machine [world text]
  (try
    (let [filename (get-last-version-filename text)
          {:keys [parts camera
                  visible-layers layer-names gears
                  sphere-transforms]} (read-string (slurp filename))
          parts (map-map (fn [[name part]]
                           {name (get-complex-part part (:info world))})
                         parts)
          world (-> world
                    (new-file)
                    (assoc-in [:parts] parts)
                    (create-spheres sphere-transforms)
                    (assoc-in [:parts :ground-part :transform] (make-transform [0 -0.1 0] [1 0 0 0]))
                    (assoc-in [:camera] camera)
                    (assoc-in [:gears] gears)
                    (set-layer-info visible-layers layer-names)
                    (#(reduce recreate-gears % gears))
                    (compute-camera)
                    (create-weld-groups)
                    (save-checkpoint!)
                    (assoc-in [:use-weld-groups] true)
                    (assoc-in [:last-saved-machine] text))]
      (user-message! "opened " filename)
      (set-title! text)
      world)
    (catch Exception e
      (user-message! "cannot open" text)
      world)))

(defn open-machine-version [world]
  (read-input world #(open-machine %1 %2)))

(defn import-machine [world text]
  (try
    (let [filename (get-last-version-filename text)
          parts (:parts (read-string (slurp filename)))
          parts (map-map (fn [[name part]]
                           {name (get-complex-part part (:info world))})
                         parts)
          ground-children (get-in parts [:ground-part :children])
          parts (dissoc-in parts [:ground-part])
          world (-> world
                    (update-in [:parts] #(merge % parts))
                    (update-in [:parts :ground-part :children]
                               #(merge % ground-children))
                    (create-weld-groups)
                    (save-checkpoint!)
                    (assoc-in [:use-weld-groups] true))]
      (user-message! "opened " filename)
      world)
    (catch Exception e
      (user-message! "cannot open" text)
      (println! e)
      world)))

(defn import-machine-version [world]
  (read-input world #(import-machine %1 %2)))
