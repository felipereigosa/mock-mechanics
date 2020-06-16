
(ns temp.core)

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
  (let [layer-box (:layer-box world)
        {:keys [x y w h]} layer-box
        used-layers (->> (:parts world)
                         (vals)
                         (map :layer)
                         (remove-nil)
                         (into #{}))]
    (fill-rect! :black x y w h)

    (dotimes [i 8]
      (let [cx (+ (* i 60) 30 (- x (/ w 2)))]
        (fill-rect! :dark-gray cx y 50 50)
        (draw-text! :black (str (inc i)) (- cx 20) (- y 10) 15)
        (when (in? (inc i) (:visible-layers world))
          (draw-rect! :white cx y 50 50))

        (if (in? (inc i) used-layers)
          (fill-circle! :white cx y 4))))))

(defn get-layer-index [box x]
  (inc (int (/ (- x (- (:x box) (/ (:w box) 2))) 60))))

(defn layer-mode-pressed [world {:keys [x y]}]
  (let [layer-box (:layer-box world)]
    (if (inside-box? layer-box x y)
      (set-layer world (get-layer-index layer-box x))
      (-> world
          (assoc-in [:selected-part] (get-part-at world x y))
          (assoc-in [:indicator] 1)))))

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
        world (if (and (not (nil? (:selected-part world)))
                       (inside-box? layer-box x y))
                (let [index (get-layer-index layer-box x)]
                  (-> world
                      (move-parts-to-layer (:selected-part world) index)
                      (dissoc-in [:selected-part])))
                (dissoc-in world [:selected-part]))]
    (tree-changed world)))
