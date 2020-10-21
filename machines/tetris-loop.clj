
[lever down-chip reset-chip copy-button pointer row-tester]

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
      (activate copy-button)
      (activate row-tester)
      (dotimes [i (inc (rand-int 5))]
        (activate reset-chip)))))
