
(def z (atom 0))
(do
1

(clear-output!)
(let [world @world
      chip-names (get-parts-with-type (:parts world) :chip)
      ]
  (doseq [chip-name chip-names]
    (let [function-names (keys (get-in world [:parts chip-name :functions]))]
      (doseq [function-name function-names]
        (remove-thing! [:parts chip-name
                        :functions function-name :final-points])
        (set-thing! [:parts chip-name
                     :functions function-name :z] @z)
        (swap! z inc)
        (println! (get-thing! [:parts chip-name
                               :functions function-name]))
    )))))
