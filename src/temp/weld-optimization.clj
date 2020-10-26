
(ns temp.core (:gen-class))

(declare get-parts-with-type)
(declare get-tail-transform)
(declare compute-transforms)
(declare create-part-bodies)

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
        free-parts (filter (fn [part-name]
                             (get-in world [:parts part-name :free]))
                           (keys (:parts world)))
        roots (concat [:ground-part] chip-children free-parts)]
    (vec (into #{} roots))))

(defn segregate-parts [world]
  (let [roots (get-root-parts world)]
    (vec (map (fn [root]
                (get-limited-tree (:parts world) root roots))
              roots))))

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
          part-color (let [color (get-color (:color part))
                           ;; color (get-color :purple) ;;##########
                           r (/ (get-red color) 255.0)
                           g (/ (get-green color) 255.0)
                           b (/ (get-blue color) 255.0)]
                       [r g b 1])
          colors (if (= (:mode world) :toggle)
                   (let [property-index (:selected-property world)
                         property (get-in world [:properties property-index])
                         color (if (get-in part [property])
                                 [1 0 0 1]
                                 [1 1 1 0])]
                     (repeat (count vertices) color))
                   (if (empty? (:colors model))
                     (repeat (count vertices) part-color)
                     (partition 4 (:colors model))))]
      (if (not= (Thread/currentThread) @the-thread)
        (throw (new Exception)))
      {:vertices vertices
       :normals normals
       :colors colors})))

(defn create-mesh-from-parts [world names]
  (let [parts (:parts world)
        baked-parts (map (fn [part-name]
                           (bake-part world (get-in parts [part-name])))
                         names)
        {:keys [vertices colors normals]} (reduce (fn [a b]
                                                    (merge-with (comp vec concat) a b))
                                                  baked-parts)
        root (get-in parts [(first names)])
        root-transform (:transform root)
        inverse-transform (get-inverse-transform root-transform)
        vertices (vec (flatten (map #(apply-transform
                                      inverse-transform %) vertices)))
        rotation-transform (get-rotation-component inverse-transform)
        normals (vec (flatten (map #(apply-transform
                                     rotation-transform %) normals)))
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

(defn get-relative-transform [part-name parts groups]
  (let [part (get-in parts [part-name])
        root-name (first (find-if #(in? part-name %) groups))
        root (get-in parts [root-name])
        part-transform (:transform part)
        root-transform (:transform root)
        relative-transform (remove-transform part-transform
                                             root-transform)]
    {part-name {:transform relative-transform
                :root-name root-name
                :type (:type part)}}))

(defn compute-root-relative-transforms [world parts groups]
  (let [part-names
        (map first (filter (fn [[name part]]
                             (or
                              (in? (:type part) [:probe :lamp :button])
                              (and (= (:type part) :block)
                                   (:collision part))
                              ))
                           parts))
        rrt (apply merge (map #(get-relative-transform % parts groups)
                            part-names))]
    (assoc-in world [:root-relative-transforms] rrt)))

(defn use-root-relative-transform [world part-name]
  (if (:use-weld-groups world)
    (let [{:keys [root-name transform]}
          (get-in world [:root-relative-transforms part-name])
          root (get-in world [:weld-groups root-name])
          root-transform (:transform root)]
      (combine-transforms transform root-transform))
    (get-in world [:parts part-name :transform])))


(defn create-weld-groups [world]
  (reset! the-thread (Thread/currentThread))
  
  (let [groups (segregate-parts world)
        world (compute-transforms world :parts)
        parts (:parts (compute-transforms (reset-part-values world) :parts))
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
    ;; (dotimes [i 20]
    ;;   (println! "recomputing..." i)
    ;;   (if (not= (Thread/currentThread) @the-thread)
    ;;     (throw (new Exception)))
    ;;   (sleep 100))

    (-> world
        (compute-root-relative-transforms parts groups)
        (assoc-in [:weld-groups] weld-groups)
        (create-part-bodies parts groups)
        (compute-transforms :weld-groups)
        )))
