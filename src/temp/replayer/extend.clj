
;; (do
;; 1

(defn update-variable [key value result variables]
  (if (= (get-in variables [key]) value)
    [result variables]
    [(conj result (str "set variable " (dekeyword key)
                    " to " (dekeyword value)))
     (assoc-in variables [key] value)]))

(defmacro change-mode-submode [mode submode-name submode]
  (let [r (gensym 'results)
        v (gensym 'variables)]
    `(let [[~r ~v] (update-variable :mode ~mode ~'result ~'variables)
           [~r ~v] (update-variable ~submode-name ~submode ~r ~v)
           ~r (conj ~r ~'instruction)]
       (~'extend-instructions-helper ~r (rest ~'instructions) ~v))))

(defn extend-instructions-helper [result instructions variables]
  (if (empty? instructions)
    result
    (let [instruction (first instructions)
          atoms (split instruction #" ")]
      (cond
        (.startsWith instruction "add")
        (let [type (get-part-type (keyword (second atoms)))]
          (change-mode-submode :add :add-type type))

        (.startsWith instruction "scale")
        (let [subcommand (keyword (first atoms))]
          (change-mode-submode :edit :edit-subcommand subcommand))

        (.startsWith instruction "set color")
        (let [color (keyword (last atoms))]
          (change-mode-submode :color :current-color color))

        (.startsWith instruction "set value")
        (let [selected (keyword (nth atoms 3))]
          (change-mode-submode :property :selected-part selected))

        (.startsWith instruction "set chip")
        (let [[_ chip _ part points relative] (read-string (str "[" instruction "]"))
              chip (keyword chip)
              part (keyword part)
              [r v] (update-variable :mode :graph result variables)
              [r v] (update-variable :selected-chip chip r v)
              part-name (dekeyword part)
              chip-name (dekeyword chip)
              r (conj r (format "put %s in %s"
                          part-name chip-name))

              r (if (not (vector= (first points) [0 0]))
                  (conj r "move [0 0] point")
                  r)

              r (if (not (vector= (last points) [1 1]))
                  (conj r (format "move point 1 of function %s of %s to %s"
                            part-name chip-name (last points)))
                            
                  r)
              
              r (reduce (fn [rt point]
                          (conj rt (format "add point %s to function %s of %s"
                                     point part-name chip-name)))
                  r
                  (butlast (rest points)))              
              
              ;; r (conj r "~~~~~~~~~~~~~~~~~~")
              ]
          (extend-instructions-helper r (rest instructions) v))

        :else
        (recur (conj result instruction)
          (rest instructions) variables)))))

(defn extend-instructions [instructions]
  (let [variables {:mode :simulation
                   :add-type :block
                   :edit-subcommand :move
                   :current-color :red}]
    (extend-instructions-helper [] instructions variables)))

(defn reset-variables [world]
  (-> world
    (assoc-in [:mode] :simulation)
    (assoc-in [:add-type] :block)
    (assoc-in [:edit-subcommand] :move)
    (assoc-in [:selected-part] nil)
    (assoc-in [:selected-chip] nil)
    (assoc-in [:current-color] :red)))

;; (clear-output!)
;; (let [world @world
;;       instructions (read-lines "res/test.txt")
;;       instructions (extend-instructions instructions)
;;       ]

;;   (println! "----")
;;   (doseq [instruction instructions]
;;     (println! instruction)
;;   nil
;;   )))

