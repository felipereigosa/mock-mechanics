
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
  (println! "saved machine" text)
  (assoc-in world [:last-saved-machine] text))

(defn load-machine-callback [world text]
  (println! "loaded machine" text)
  (-> (create-world)
      (load-machine (str "resources/machines/" text ".clj"))
      (assoc-in [:last-saved-machine] text)))

(defn save-version [world]
  (if-let [last-saved (:last-saved-machine world)]
    (let [[root number] (split last-saved #"\.")
          number (if (nil? number)
                   0
                   (parse-int number))
          new-name (str root "." (format "%03d" (inc number)))]
      (save-machine-callback world new-name))
    (do
      (println! "you need to save first!")
      world)))

(defn extract-number [name]
  (let [r-index (.indexOf name ".")
        l-index (.indexOf name "." (inc r-index))]
    (if (= l-index -1)
      nil
      (parse-int (subs name (inc r-index) l-index)))))

(defn load-last-version-callback [world text]
  (let [all-files (get-files-at "resources/machines")
        text-files (filter #(.startsWith % text) all-files)
        number (if (= (count text-files) 1)
                 ""
                 (let [number (->> (map extract-number text-files)
                                               (filter not-nil?)
                                               (apply max))]
                   (str "." (format "%03d" number))))]
    (load-machine-callback world (str text number))))
