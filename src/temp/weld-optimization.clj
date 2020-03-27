
(ns temp.core)

(do
1

(declare get-parts-with-type)
(declare get-tail-transform)
(declare compute-transforms)

(defn get-limited-tree [parts root-name all-root-names]
  (let [root (get-in parts [root-name])
        children (filter (fn [name]
                           (not (in? name all-root-names)))
                         (keys (get-in root [:children])))
        descendents (map #(get-limited-tree parts % all-root-names)
                         children)]
    (vec (apply concat [root-name] descendents))))

(defn get-root-parts [world]
  (let [chip-names (get-parts-with-type (:parts world) :chip)
        chip-children (apply concat (map (fn [chip-name]
                                           (let [chip (get-in world [:parts chip-name])]
                                             (keys (:functions chip))))
                                         chip-names))
        probes (get-parts-with-type (:parts world) :probe)
        free-parts (filter (fn [part-name]
                             (get-in world [:parts part-name :free]))
                           (keys (:parts world)))
        force-part (get-in world [:force :part-name])
        track-force-part (get-in world [:track-force :part-name])
        roots (concat [:ground-part] chip-children free-parts probes
                      (filter not-nil? [force-part track-force-part]))]
    (vec (into #{} roots))))

(defn segregate-parts [world]
  (let [roots (get-root-parts world)]
    (vec (map (fn [root]
                (get-limited-tree (:parts world) root roots))
              roots))))

(defn change-whites [colors other]
  (map (fn [color]
         (if (vector= color [1 1 1 1])
           other
           color))
       colors))

(defn bake-part [world part]
  (if (not (in? (:layer part) (:visible-layers world)))
    {}
    (let [type (:type part)
          model (get-in world [:info type :model])
          transform (if (= type :track)
                      (get-tail-transform part)
                      (:transform part))
          scale (:scale part)
          vertices (map (fn [v]
                          (let [sv (map (fn [a b]
                                          (* a b)) v scale)]
                            (apply-transform transform sv)))
                        (vec (partition 3 (:vertices model))))
          rotation-transform (get-rotation-component transform)
          normals (map #(apply-transform rotation-transform %)
                       (vec (partition 3 (:normals model))))
          model (get-in world [:info (:type part) :model])
          part-color (let [color (get-color (:color part))
                           r (/ (get-red color) 255.0)
                           g (/ (get-green color) 255.0)
                           b (/ (get-blue color) 255.0)]
                       [r g b 1])
          part-color [1 0 0 1]
          colors (if (= (:mode world) :toggle)
                   (let [property-index (:selected-property world)
                         property (get-in world [:properties property-index])
                         color (if (get-in part [property])
                                 [1 0 0 1]
                                 [1 1 1 0])]
                     (repeat (count vertices) color))
                   (if (empty? (:colors model))
                     (repeat (count vertices) part-color)
                     (change-whites (partition 4 (:colors model))
                                    part-color)))]
      {:vertices vertices
       :normals normals
       :colors colors})))

(defn create-mesh-from-parts [world names]
  (let [edited-part (:edited-part world)
        parts (:parts world)
        baked-parts (map (fn [name]
                           (if (= name edited-part)
                             nil
                             (bake-part world (get-in parts [name]))))
                         names)
        {:keys [vertices colors normals]} (reduce (fn [a b]
                                                    (merge-with (comp vec concat) a b))
                                                  baked-parts)
        root (get-in parts [(first names)])
        root-transform (:transform root)
        inverse-transform (get-inverse-transform root-transform)
        vertices (vec (flatten (map #(apply-transform
                                      inverse-transform %) vertices)))
        ;; rotation-transform (get-rotation-component root-transform)
        ;; inverse-transform (get-inverse-transform root-transform)
        ;; normals (vec (flatten (map #(apply-transform
        ;;                              inverse-transform %) normals)))
        colors (vec (flatten colors))]
    (create-mesh vertices [0 0 0] [1 0 0 0]
                   [1 1 1] colors [] normals)))

(defn is-child-group? [parts parent-names child-names]
  (some (fn [parent-name]
          (let [children (get-in parts [parent-name :children])]
            (in? (first child-names) (keys children))))
        parent-names))

(defn get-group-children [parts group groups]
  (map-map (fn [other-group]
             (if (is-child-group? parts group other-group)
               (let [parent (get-in parts [(first group)])
                     child (get-in parts [(first other-group)])
                     child-transform (:transform child)
                     parent-transform (:transform parent)
                     relative-transform (remove-transform child-transform
                                                          parent-transform)]
                 {(first other-group) relative-transform})))
           groups))

(defn reset-part-values [world]
  (let [roots (get-root-parts world)
        parts (map-map (fn [[name part]]
                         (if (in? name roots)
                           {name (assoc-in part [:value] 0.0)}
                           {name part}))
                       (:parts world))]
    (assoc-in world [:parts] parts)))

(defn create-weld-groups [world]
  (let [groups (segregate-parts world)
        parts (:parts (compute-transforms
                       (reset-part-values world)
                       :parts))
        info (:info world)
        property (if (= (:mode world) :toggle)
                   (nth (get-in world [:properties])
                        (:selected-property world))
                   nil)
        weld-groups (map-map (fn [names]
                               (let [children (get-group-children parts names groups)
                                     mesh (-> (create-mesh-from-parts world names)
                                              (assoc-in [:children] children)
                                              (assoc-in [:parts] names))]
                                 {(first names) mesh}))
                             groups)]
    (-> world
        (assoc-in [:weld-groups] weld-groups)
        (compute-transforms :weld-groups))))

;; (clear-output!)
(set-thing! [:use-weld-groups] false)
;; (update-thing! [] create-weld-groups)

)

;; (remove-thing! [:weld-groups])
;; (keys (:weld-group @world))
