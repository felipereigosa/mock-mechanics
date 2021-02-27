
[button pointer wagon]

(fn [part-pressed]
  (if (on? button)
    (let [color (get-pointed-color pointer)
          colors [:red :dark-green :yellow :blue :orange
                  :purple :pink :white :dark-gray :black]]
      (set-value wagon (- 0.9 (* 0.1 (get-index color colors)))))))
