
(ns temp.core)

(def undo-ring (atom nil))
(def undo-index (atom nil))
(def undo-fields (atom nil))

(defn reset-undo! [world fields]
  (reset! undo-ring (vec (repeat 10 world)))
  (reset! undo-index 1)
  (reset! undo-fields fields)
  world)

(defn different-checkpoint? [world]
  (let [index (mod (- @undo-index 1) (count @undo-ring))
        saved-world (nth @undo-ring index)]
    (find-if (fn [field]
               (not= (get world field)
                     (get saved-world field)))
             @undo-fields)))

(defn save-checkpoint! [world]
  (when (different-checkpoint? world)
    (swap! undo-ring (fn [ring]
                       (let [index (mod @undo-index (count @undo-ring))]
                         (assoc-in ring [index] world)
                         )))
    (swap! undo-index inc))
  world)

(declare prepare-tree)

(defn copy-world [dest source fields]
  (prepare-tree (reduce (fn [w field]
                          (assoc-in w [field] (get-in source [field])))
                        dest
                        fields)))

(defn undo! [world]
  (let [index (mod (- @undo-index 2) (count @undo-ring))
        saved-world (nth @undo-ring index)]
    (swap! undo-index dec)
    (copy-world world saved-world @undo-fields)))

(defn redo! [world]
  (let [index (mod @undo-index (count @undo-ring))
        saved-world (nth @undo-ring index)]
    (swap! undo-index inc)
    (copy-world world saved-world @undo-fields)))
