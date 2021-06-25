
(defn get-descaled-relative-transform [world part-name change]
  (let [parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        offset (if (= (:type part) :track)
                 change
                 (vector-multiply change 0.5))
        offset (vector-multiply offset -1)
        offset-transform (make-transform offset [1 0 0 0])
        relative-transform (get-in world [:parts parent-name
                                          :children part-name])]
    (combine-transforms offset-transform relative-transform)))

(defn create-part-instructions [writer world part-name]
  (let [parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        type (:type part)
        properties (concat
                    (keys (get-in world [:info type :properties]))
                    (filter #(not= % :.) (:properties world))
                    '(:color))
        color (get-in world [:info type :color])
        new-part (create-part type color 0 (:info world))
        scale-change (vector-subtract (:scale part) (:scale new-part))
        property= (fn [a b]
                    (if (number? a)
                      (float= a b)
                      (= a b)))]

    (let [relative-transform (get-descaled-relative-transform
                              world part-name scale-change)
          position (get-transform-position relative-transform)
          rotation (get-transform-rotation relative-transform)]
      (.write writer (format "add %s to %s at %s %s\n"
                             (dekeyword part-name)
                             (dekeyword parent-name)
                             position
                             rotation)))
    (doseq [i (range 3)]
      (if (not (float= (nth scale-change i) 0.0))
        (.write writer (format "scale %s by %s\n"
                               (dekeyword part-name)
                               (assoc [0 0 0] i (nth scale-change i))))))
    
    (doseq [property properties]
      (if (not (property= (get part property) (get new-part property)))
        (let [value (if (= property :color)
                      (reverse-get-color (get part property))
                      (get part property))
              value (if (keyword value)
                      (dekeyword value)
                      value)
              value (if (= type :wagon)
                      (* value (reduce + (:track-lengths part)))
                      value)]
          (.write writer (format "set %s of %s to %s\n"
                                 (dekeyword property)
                                 (dekeyword part-name)
                                 value)))))
    (when (= type :chip)
      (doseq [[function-name function] (:functions part)]
        (let [{:keys [points relative]} function]
          (.write writer (format "set %s function %s %s %s\n"
                           (dekeyword part-name)
                           (dekeyword function-name) 
                           points relative))
      )))

    ;; (if (and (= type :motherboard)
    ;;          (not (empty? (:pins part))))
    ;;   (.write writer (format "set-graph of %s to %s\n"
    ;;                    (dekeyword part-name)
    ;;                    (select-keys part [:pins :gates :connections]))))
    ))

(defn get-part-number [part-name]
  (parse-int (second (re-find #":[a-z]*([0-9]*)" (str part-name)))))

(defn build-parents-map [mp parts part-name]
  (reduce (fn [m child-name]
            (-> m
              (assoc-in [child-name] part-name)
              (build-parents-map parts child-name)))
    mp
    (keys (:children (get-in parts [part-name])))))

(defn ancestor? [mp parent child]
  (->> child
    (iterate #(get mp %))
    (take-while not-nil?)
    (get-index parent)
    (boolean)))

(defn create-instructions [world filename]
  (let [sorted-names (>> (:parts world)
                         (keys .)
                         (into #{} .)
                         (clojure.set/difference . #{:ground-part})
                         (vec .)
                         (sort-by get-part-number .))
        parent-map (build-parents-map {} (:parts world) :ground-part)
        sorted-names (sort-by identity (partial ancestor? parent-map)
                       sorted-names)
        world (reduce (fn [w part-name]
                        (set-value-0-transform w part-name))
                      world
                      sorted-names)
        filename (str "res/" filename ".txt")]
    (with-open [writer (clojure.java.io/writer filename)]
      (doseq [part-name sorted-names]
        (create-part-instructions writer world part-name)))
    world))
