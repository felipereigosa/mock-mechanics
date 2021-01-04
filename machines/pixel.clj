
[x-wagon y-wagon display]

(fn [part-name]
  (let [x-value (round (* (get-value x-wagon) 9))
        y-value (round (* (get-value y-wagon) 5))]
    (clear-display display :black)
    (set-pixel display x-value y-value :blue)))




