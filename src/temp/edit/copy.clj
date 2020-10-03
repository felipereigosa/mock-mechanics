
(ns temp.core (:gen-class))

(defn change-keys
  ([map suffix]
   (change-keys map suffix nil))
  ([map suffix pred]
   (map-map (fn [[key value]]
              (let [key (if (or (nil? pred)
                                (pred key))
                          (join-keywords key suffix)
                          key)]
                {key value}))
            map)))

(defn copy-part [parts part-name suffix]
  (let [copy-name (join-keywords part-name suffix)
        part (get-in parts [part-name])
        part (update-in part [:children] #(change-keys % suffix))]
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

(defn fix-chip-references [chip copied-parts suffix]
  (let [pred #(in? % copied-parts)]
    (update-in chip [:functions] #(change-keys % suffix pred))))

(defn fix-connections [connections copied-parts suffix]
  (map-map (fn [[connection-name connection]]
             (let [new-points (map (fn [point]
                                     (if (in? point copied-parts)
                                       (join-keywords point suffix)
                                       point))
                                   (:points connection))]
               
               {connection-name
                (assoc-in connection [:points] new-points)}))
           connections))

(defn fix-motherboard-references [motherboard copied-parts suffix]
  (let [pred #(in? % copied-parts)]
    (-> motherboard
        (update-in [:pins] #(change-keys % suffix pred))
        (update-in [:connections]
                   #(fix-connections % copied-parts suffix)))))

(defn fix-references [parts old-names suffix]
  (let [new-names (map (fn [part-name]
                         (join-keywords part-name suffix))
                       old-names)]
    (reduce (fn [ps new-name]
              (let [part (get-in ps [new-name])]
                (case (:type part)
                  :motherboard (update-in ps [new-name]
                           #(fix-motherboard-references % old-names suffix))
                  :chip (update-in ps [new-name]
                           #(fix-chip-references % old-names suffix))
                  ps)))
            parts new-names)))

(declare get-tree-with-root)

(def copy-name (atom nil))

(defn copy-mode-pressed [world event]
  (if-let [collision (get-collision world event)]
    (if (:control-pressed world)
      (let [part-name (:part-name collision)]
        (-> world
            (assoc-in [:selected-part] part-name)
            (select-part part-name)))
      (if-let [selected-part (:selected-part world)]
        (if (can-place-part-at? world collision)
          (let [suffix (gen-keyword :copy)
                [parts copy-part-name] (copy-tree (:parts world) selected-part suffix)
                copied-parts (get-tree-with-root parts selected-part)
                parts (fix-references parts copied-parts suffix)
                new-parent-name (:part-name collision)
                new-parent (get-in world [:parts new-parent-name])]
            (reset! copy-name copy-part-name)
            (-> world
                (assoc-in [:parts] parts)
                (place-part-at copy-part-name collision)
                (move-part-pressed copy-part-name nil)))
          world)
        world))
    world))

(defn copy-mode-moved [world event]
  (move-part-moved world event))

(defn copy-mode-released [world event]
  (move-part-released world event))
