
(def saved-parts (atom (:parts @world)))

;; (set-thing! [:parts] @saved-parts)

(reset-world!)
(create-graph! :yellow)
(println! (:wave-editor @world))

(do
1  

(clear-output!)
(enforce-cable-lenght @world (get-thing! [:cables :cable8989]))
nil)


(println! (compute-cable-length @world (get-thing! [:cables :cable8989])))

(do
1

(set-thing! [:parts :block8984 :value] 0.89)
(update-thing! [] compute-transforms))
(reset-world!)

(do
(set-thing! [:parts (get-part-with-color @world :green) :value] 0.0)
(update-thing! [] compute-transforms)
)

(get-part-with-color @world :green)

(set-thing! [:cables :cable12634 :length] 3.0)

(update-thing! [:wave-editor :functions] #(dissoc-in % [:block12613]))

(set-thing! [:cables] {})

(reset-world!)

