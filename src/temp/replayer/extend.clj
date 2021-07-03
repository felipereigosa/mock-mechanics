
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

(defn get-bounding-viewbox [points]
  (let [xs (map first points)
        ys (map second points)
        x1 (first xs)
        x2 (last xs)
        y1 (reduce min ys)
        y2 (reduce max ys)
        zoom-x (/ 1.0 (- x2 x1))
        zoom-y (/ 1.0 (- y2 y1))
        x (* x1 zoom-x -1)
        y (* y1 zoom-y -1)]
    [x y zoom-x zoom-y]))

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

              r (conj r (format "set view of %s to %s"
                          chip-name (get-bounding-viewbox points)))

              r (if (not (vector= (first points) [0 0]))
                  (conj r (format "move point 0 of function %s of %s to %s"
                            part-name chip-name (first points)))
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

              r (conj r (str "activate " chip-name))]
          (extend-instructions-helper r (rest instructions) v))

        (.startsWith instruction "set motherboard")
        (let [[_ motherboard _ tab-num & elements] (read-string (str "[" instruction "]"))
              [r v] (update-variable :mode :motherboard result variables)
              [r v] (update-variable :selected-motherboard (keyword motherboard) r v)
              get-element-type (fn [element]
                                 (let [[a b] (take-last 2 element)]
                                   (cond
                                     (and (number? a) (number? b)) :gate
                                     (or (number? b) (= b 'true)) :pin
                                     :else :connection)))

              r (conj r (format "select %s tab %s"
                          motherboard tab-num))

              r (reduce (fn [rt element]
                          (if (= (get-element-type element) :connection)
                            (let [[connection-name & element] element
                                  rt (conj rt (format "set %s connection %s %s"
                                                motherboard
                                                connection-name
                                                (vec (take 2 element))))]
                              (reduce (fn [rtt point]
                                        (conj rtt (format "add %s %s point %s"
                                                    motherboard
                                                    connection-name
                                                    point)))
                                rt
                                (nthrest element 2)))
                            (let [rt (conj rt (format "set %s %s %s"
                                                motherboard
                                                (dekeyword (get-element-type element))
                                                element))]
                              (if (= (last element) 'true)
                                (conj rt (format "toggle %s pin %s"
                                           motherboard (first element)))
                                rt))))
                  r
                  elements)]
          (extend-instructions-helper r (rest instructions) v))

        :else
        (recur (conj result instruction)
          (rest instructions) variables)))))

(def start-variables {:mode :simulation
                      :add-type :block
                      :edit-subcommand :move
                      :selected-part nil
                      :selected-chip nil
                      :current-color :red
                      })

(defn extend-instructions [instructions]
  (extend-instructions-helper [] instructions start-variables))

(defn reset-variables [world]
  (merge world start-variables))
