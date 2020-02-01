
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

(defn copy-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [part-name (:part-name (get-part-collision world x y))]
      (assoc-in world [:edited-part] part-name)
      world)))

(defn translate-mode-released [world event] ;;##################
  (if-let [part-name (:edited-part world)]
    (let [x (:x event)
          y (:y event)
          part (get-in world [:parts part-name])
          anchor (get-anchor-point world x y)
          new-parent-name (:part anchor)
          old-parent-name (get-parent-part world part-name)
          offset (get-part-offset part)
          parent (get-in world [:parts new-parent-name])
          transform (anchor->transform offset anchor parent)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (dissoc-in [:parts old-parent-name
                      :children part-name])
          (dissoc-in [:edited-part])
          (create-relative-transform part-name new-parent-name)))
    world))

(defn copy-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [suffix (gen-keyword :copy)
          [parts copy-part-name] (copy-tree (:parts world) part-name suffix)
          parts (change-missing-references parts)
          x (:x event)
          y (:y event)
          part (get-in world [:parts part-name])
          anchor (get-anchor-point world x y)
          parent-name (:part anchor)
          offset (get-part-offset part)
          parent (get-in world [:parts parent-name])
          transform (anchor->transform offset anchor parent)]
      (-> world
          (assoc-in [:parts] parts)
          (assoc-in [:parts copy-part-name :transform] transform)
          (create-relative-transform copy-part-name parent-name)
          (dissoc-in [:edited-part])))
    world))
