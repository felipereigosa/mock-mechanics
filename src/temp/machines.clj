
(ns temp.core)

(defn get-simple-transform [transform]
  {:position (get-transform-position transform)
   :rotation (get-transform-rotation transform)})

(defn get-simple-part [part]
  (let [children (map-map (fn [[name transform]]
                            {name (get-simple-transform transform)})
                          (:children part))]
    (-> part
        (dissoc-in [:transform])
        (assoc-in [:children] children))))

(defn get-complex-transform [transform]
  (make-transform (:position transform) (:rotation transform)))

(defn get-complex-part [part]
  (let [children (map-map (fn [[name transform]]
                            {name (get-complex-transform transform)})
                          (:children part))]
    (-> part
        (assoc-in [:transform] (make-transform [0 0 0] [1 0 0 0]))
        (assoc-in [:children] children))))

;;---

(defn save-machine! [world filename]
  (let [ground-children (map-map (fn [[name transform]]
                                   {name (get-simple-transform transform)})
                                 (:ground-children world))
        parts (map-map (fn [[name part]]
                         {name (get-simple-part part)})
                       (:parts world))]
    (spit filename {:ground-children ground-children
                    :parts parts})))

(defn load-machine [world filename]
  (let [{:keys [ground-children parts graph-box]} (read-string (slurp filename))
        ground-children (map-map (fn [[name transform]]
                                   {name (get-complex-transform transform)})
                                 ground-children)
        parts (map-map (fn [[name part]]
                         {name (get-complex-part part)})
                       parts)]
    (-> world
        (assoc-in [:ground-children] ground-children)
        (assoc-in [:parts] parts)
        (compute-transforms :parts)
        (create-weld-groups))))

(defn save-machine-callback [world text]
  (save-machine! world (str "resources/machines/" text ".clj"))
  world)

(defn load-machine-callback [world text]
  (load-machine (create-world) (str "resources/machines/" text ".clj")))
