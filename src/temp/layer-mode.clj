
(ns temp.core (:gen-class))

(defn create-layer-info [world]
  (-> world
      (assoc-in [:visible-layers] [0])
      (assoc-in [:layer-names] (vec (cons "main" (repeat 14 ""))))))

(defn layer-mode-entered [world]
  (dissoc-in world [:selected-part]))

(defn set-layer [world number]
  (if (:shift-pressed world)
    (let [layers (:visible-layers world)
          new-layers (if (in? number layers)
                       (vec (remove #(= % number) layers))
                       (conj layers number))]
      (if (empty? new-layers)
        world
        (assoc-in world [:visible-layers] new-layers)))
    (assoc-in world [:visible-layers] [number])))

(defn layer-mode-draw [world]
  (let [box (:layer-box world)
        {:keys [x y w h image regions]} box
        used-layers (->> (:parts world)
                         (vals)
                         (map :layer)
                         (remove-nil)
                         (into #{}))
        get-region (fn [name i]
                     (>> (str name i)
                         (keyword)
                         (get regions .)
                         (get-absolute-region . box)))]
    (draw-image! image x y)

    (dotimes [i 30]
      (let [hi (int (/ i 2))
            text-region (get-region "text" hi)
            layer-region (if (= (mod i 2) 0)
                           (get-region "a" hi)
                           (get-region "b" hi))
            layer-color (if (in? i (:visible-layers world))
                          :white
                          :dark-gray)
            names (:layer-names world)]
        
        (when (and (= (mod i 2) 0)
                   (< hi (count names)))
          (draw-text-in-box! (get-in names [hi])
                             :white 20 text-region))

        (draw-rect! layer-color
                    (:x layer-region)
                    (:y layer-region)
                    (:w layer-region)
                    (:h layer-region))

        (if (in? i used-layers)
          (fill-circle! :white
                        (:x layer-region)
                        (:y layer-region) 4))))))

(defn layer-name->number [name]
  (let [s (str name)
        n (* 2 (parse-int (subs s 2)))]
    (if (.startsWith s ":b")
      (inc n)
      n)))

(defn set-layer-name-callback [world region name]
  (let [index (parse-int (subs (str region) 5))]
    (assoc-in world [:layer-names index] (str name))))

(defn layer-mode-pressed [world {:keys [x y] :as event}]
  (let [layer-box (:layer-box world)
        region (get-region-at layer-box x y)]
    (if (inside-box? layer-box x y)
      (if (not-nil? region)
        (if (.startsWith (str region) ":text")
          (read-input world #(set-layer-name-callback %1 region %2))
          (set-layer world (layer-name->number region)))
        world)
      (assoc-in world [:selected-part] (get-part-at world event)))))
                  
(defn move-parts-to-layer [world root-name layer-index]
  (let [names (if (:shift-pressed world)
                (get-limited-tree (:parts world) root-name [])
                [root-name])]
    (reduce (fn [w name]
              (assoc-in w [:parts name :layer] layer-index))
            world
            names)))

(defn layer-mode-released [world {:keys [x y]}]
  (let [layer-box (:layer-box world)
        region (get-region-at layer-box x y)
        world (if (and (not-nil? (:selected-part world))
                       (inside-box? layer-box x y)
                       (not-nil? region)
                       (not (.startsWith (str region) ":text")))
                (move-parts-to-layer world
                                     (:selected-part world)
                                     (layer-name->number (str region)))
                world)
        world (dissoc-in world [:selected-part])]
    (tree-changed world)))
