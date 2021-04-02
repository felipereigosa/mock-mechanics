
[button platform]

(fn [part-name]
  (if (on? button)
    (let [world @world
          voxel-names (rest (get-tree-with-root (:parts world) platform))
          offset (get-part-position platform)
          voxel-map
          (apply merge
                 (map (fn [voxel]
                        (let [coords (>> (get-part-position voxel)
                                         (vector-subtract . offset)
                                         (vector-subtract . [-0.4 0.1 -0.4])
                                         (map #(round (* 10 %)) .))
                              color (reverse-get-color (get-color voxel))]
                          {(vec coords) color}))
                      voxel-names))
          filename (get-attribute platform :data)
          colors [:blank :black :medium-gray :white :purple
                  :orange :blue :yellow :dark-green :red]
          ]
      (with-open [w (clojure.java.io/writer (str "machines/" filename ".txt"))]
        (doseq [y (range 0 15)]
          (doseq [z (range 0 9)]
            (doseq [x (range 0 9)]
              (let [color (get voxel-map [x y z] :blank)]
                (.write w (str (get-index color colors) " "))))
            (.write w "\n"))
          (.write w "\n")))
      (println! "Voxels written to" (str filename  ".txt"))
      )))

