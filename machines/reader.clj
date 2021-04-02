
[button pointer input output]

(fn [part-name]
  (when (on? button)
    (let [filename (get-attribute (get-part pointer) :data)
          data (read-string (str "[" (slurp (str "machines/" filename ".txt")) "]"))
          input-value (get-value input)
          index (round (* 10 input-value))]
      (set-value output (* (nth data index) 0.1))
      (set-value input (+ input-value 0.1)))))
