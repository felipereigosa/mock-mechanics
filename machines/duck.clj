
[button x z y wagon]

(fn [part-pressed]
  (let [voxels (read-string (slurp "machines/duck.txt"))
        x (int (* (get-value x) 10))
        z (int (* (get-value z) 10))
        y (- (int (* (get-value y) 10)) 4)
        colors [:red :dark-green :yellow :blue :orange
                :purple :pink :white :dark-gray :black]
        color (or (get voxels [x z y]) :black)]
    (if (on? button)
      (set-value wagon (- 0.9 (* 0.1 (get-index color colors)))))))
