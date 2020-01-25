
(ns temp.core)

(defn pivot-mode-pressed [world event]
  (if (= (:button event) :left)
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
      (compute-camera (assoc-in world [:camera :pivot] pos)))
    world))

(defn pivot-mode-draw [world]
  (let [[x y] (project-point world (get-in world [:camera :pivot]))]
    (println! "redraw")
    (draw-line! :white x (- y 30) x (+ y 30))
    (draw-line! :white (- x 30) y (+ x 30) y)
    (draw-circle! :white x y 10)))
