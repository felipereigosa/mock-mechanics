
[lever down-chip new-part-chip copy-button pointer]

(defn shape-collision? []
  (let [blocks (get-children (get-part pointer))
        points (map #(vector-add
                      (get-part-position %)
                      [-0.2 -0.5 0]) blocks)
        collisions (remove-nil (map #(get-part [% [-1 0 0]]) points))]
    (some #(not (in? % blocks)) collisions)))

(fn [part-name]
  (while (on? lever)
    (activate down-chip)
    (when (shape-collision?)
      (press-button copy-button)
      (dotimes [i (inc (rand-int 5))]
        (activate new-part-chip)))))
