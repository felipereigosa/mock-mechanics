
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

(defn extend-graph-instruction [instruction]
  (let [[_ chip-name _ part-name
         points relative] (read-string (str "[" instruction "]"))
        r ["set variable mode to graph"
           (str "set variable selected-chip to " chip-name)]
        r (conj r (format "put %s in %s" part-name chip-name))
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
        r (if relative
            (conj r (format "toggle %s function %s"
                      chip-name part-name))
            r)
        r (conj r (str "activate " chip-name))]
    r))

(defn extend-motherboard-instruction [instruction]
  (let [[_ motherboard-name _
         tab-num & elements] (read-string (str "[" instruction "]"))
        get-element-type (fn [element]
                           (let [[a b] (take-last 2 element)]
                             (cond
                               (and (number? a) (number? b)) :gate
                               (or (number? b) (= b 'true)) :pin
                               :else :connection)))
        r ["set variable mode to motherboard"
           (str "set variable selected-motherboard to " motherboard-name)]
        r (conj r (format "select %s tab %s" motherboard-name tab-num))
        r (reduce (fn [rt element]
                    (if (= (get-element-type element) :connection)
                      (let [[connection-name & element] element
                            rt (conj rt (format "set %s connection %s %s"
                                          motherboard-name
                                          connection-name
                                          (vec (take 2 element))))]
                        (reduce (fn [rtt point]
                                  (conj rtt (format "add %s %s point %s"
                                              motherboard-name
                                              connection-name
                                              point)))
                          rt
                          (nthrest element 2)))
                      (let [rt (conj rt (format "set %s %s %s"
                                          motherboard-name
                                          (dekeyword (get-element-type element))
                                          element))]
                        (if (= (last element) 'true)
                          (conj rt (format "toggle %s pin %s"
                                     motherboard-name (first element)))
                          rt))))
            r
            elements)]
    r))

(defn extend-instruction [instruction]
  (let [atoms (split instruction #" ")
        change-mode-submode (fn [mode submode-name submode]
                              [(str "set variable mode to " mode)
                               (format "set variable %s to %s"
                                 submode-name submode)])]
    (cond
      (.startsWith instruction "add")
      (let [type (get-part-type (keyword (second atoms)))]
        (conj (change-mode-submode "add" "add-type" (dekeyword type))
          instruction))

      (.startsWith instruction "scale")
      (conj (change-mode-submode "edit" "edit-subcommand" (first atoms))
        instruction)

      (.startsWith instruction "set color")
      (conj (change-mode-submode "color" "current-color" (last atoms))
        instruction)

      (.startsWith instruction "set value")
      (conj (change-mode-submode "property" "selected-part" (nth atoms 3))
        instruction)

      (.startsWith instruction "set chip")
      (extend-graph-instruction instruction)

      (.startsWith instruction "set motherboard")
      (extend-motherboard-instruction instruction)

      :else [instruction])))

(defn extend-instructions [instructions]
  (apply concat (map extend-instruction instructions)))
