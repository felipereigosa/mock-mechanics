(defn point-on-block-surface? [world part-name point]
  (let [part (get-in world [:parts part-name])]
    (if (= (:type part) :block)
      (let [mesh (get-in world [:info :block :model])
            transform (:transform part)
            scale (:scale part)
            triangles (get-mesh-triangles mesh transform scale)]
        (some #(point-inside-triangle point %) triangles))
      false)))
