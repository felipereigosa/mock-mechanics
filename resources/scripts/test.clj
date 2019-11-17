
(script
 []
 [chip-a chip-b]

 (defn foo [x]
   (+ x 1))

 (defn bar []
   3.14)

 (fn [part-name]
   (activate chip-a)
   (wait #(chip-active? chip-a))
   (activate chip-b)
   )
 )
