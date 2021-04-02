
[button pointer x y z wagon]

(fn [part-pressed]
  (when (on? button)
    (let [name (get-attribute (get-part pointer) :data)
          voxels (read-string (slurp (str "machines/" name ".txt")))
          colors [:black :gray :white :pink :purple
                  :orange :blue :yellow :green :red :blank]
          coordinates (vec (map (comp round #(* 10 %) get-value) [x y z]))
          color-name (keyword (get voxels coordinates :blank))
          color-value (get-index color-name colors)]
      (set-value wagon (* color-value 0.1)))))
