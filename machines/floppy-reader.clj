
[button pointer x y z wagon]

(fn [part-pressed]
  (if (on? button)
    (let [name (get-attribute (get-part pointer) :data)
          voxels (read-string (slurp (str "machines/" name ".txt")))
          coordinates (vec (map (comp round #(* 10 %) get-value) [x y z]))
          color-value (get voxels coordinates 10)]
      (set-value wagon (* color-value 0.1)))))
