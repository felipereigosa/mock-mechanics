
[button probe chip1 chip2]
[]

(fn [part-name]
  (let [helper (fn [x]
                 (+ x 1))]
    (println! (helper 100) (get-value part-name))))
