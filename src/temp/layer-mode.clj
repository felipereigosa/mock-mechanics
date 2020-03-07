
(ns temp.core)

(do
1

(defn layer-mode-entered [world]
  (dissoc-in world [:selected-part]))

(defn number-pressed [world number]
  (if-let [part-name (:selected-part world)]
    (-> world
        (assoc-in [:parts part-name :layer] number)
        (dissoc-in [:selected-part]))
    (set-layer world number)))

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
        {:keys [x y w h]} layer-box]    
    (fill-rect! :black x y w h)
    
    (when (:selected-part world)
      (draw-rect! :white x y w h))
    
    (dotimes [i 8]
      (let [cx (+ (* i 60) 30 (- x (/ w 2)))]
        (fill-rect! :dark-gray cx y 50 50)
        (draw-text! :black (str (inc i)) (- cx 20) (- y 10) 15)
        (when (in? (inc i) (:visible-layers world))
          (draw-rect! :white cx y 50 50))))))

(defn layer-mode-pressed [world event]
  (let [layer-box (:layer-box world)
        px (:x event)
        py (:y event)]    
    (if (inside-box? layer-box px py)
      (let [{:keys [x y w h]} layer-box
            index (inc (int (/ (- px (- x (/ w 2))) 60)))]
        (set-layer world index))
      (assoc-in world [:selected-part] (get-part-at world px py)))))

(clear-output!)
(redraw!)
)

