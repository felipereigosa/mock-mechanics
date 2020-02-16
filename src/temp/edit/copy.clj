
(ns temp.core)

(defn change-keys [map suffix]
  (map-map (fn [[name value]]
             {(join-keywords name suffix)
              value})
           map))

(defn copy-part [parts part-name suffix]
  (let [copy-name (join-keywords part-name suffix)
        part (-> (get-in parts [part-name])
                 (update-in [:children] #(change-keys % suffix))
                 (update-in [:functions] #(change-keys % suffix))
                 (update-in [:inputs] #(change-keys % suffix))
                 (update-in [:outputs] #(change-keys % suffix)))]
    (assoc-in parts [copy-name] part)))

(defn copy-tree [parts part-name suffix]
  (let [copy-name (join-keywords part-name suffix)
        parts (copy-part parts part-name suffix)
        part (get-in parts [part-name])
        parts (reduce (fn [ps child-name]
                        (first (copy-tree ps child-name suffix)))
                      parts
                      (keys (:children part)))]
    [parts copy-name]))

(defn clean-map [map valid-keys]
  (map-map (fn [[key value]]
             (if (in? key valid-keys)
               {key value}
               (let [index (.indexOf (str key) "-")
                     old-key (keyword (subs (str key) 1 index))]
                 {old-key value})))
           map))

(defn change-missing-references [parts]
  (let [part-names (keys parts)]
    (map-map (fn [[name part]]
               {name
                (-> part
                    (update-in [:functions] #(clean-map % part-names))
                    (update-in [:inputs] #(clean-map % part-names))
                    (update-in [:outputs] #(clean-map % part-names)))})
             parts)))

(defn copy-mode-pressed [world {:keys [x y]}]
  (if-let [collision (get-collision world x y)]
    (if (:control-pressed world)
      (assoc-in world [:selected-part] (:part-name collision))
      (if-let [selected-part (:selected-part world)]
        (let [suffix (gen-keyword :copy)
              [parts copy-part-name] (copy-tree (:parts world) selected-part suffix)
              parts (change-missing-references parts)]
          (-> world
              (assoc-in [:parts] parts)
              (place-part-at copy-part-name collision)
              (move-part-pressed copy-part-name nil)))
        world))
    world))

(defn copy-mode-moved [world event]
  (move-part-moved world event))

(defn copy-mode-released [world event]
  (move-part-released world event))
