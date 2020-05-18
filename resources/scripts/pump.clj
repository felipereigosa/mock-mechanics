
[button]
[up dump undump down open close]

(let [wait-after (fn [chip-name ms]
                   (activate chip-name)
                   (wait #(chip-active? chip-name))
                   (sleep ms))]
  (fn [part-name]

    (when (and (= part-name button)
               (= (get-value button) 1))

      (wait-after open 500)
      (wait-after close 200)
      (wait-after up 200)
      (wait-after dump 500)
      (wait-after undump 200)
      (activate down)
      )))
