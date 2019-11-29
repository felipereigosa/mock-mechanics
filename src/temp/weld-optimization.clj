
(ns temp.core)

(defn get-limited-tree [parts root-name all-root-names]
  (let [root (get-in parts [root-name])
        children (filter (fn [name]
                           (not (in? name all-root-names)))
                         (keys (get-in root [:children])))
        descendents (map #(get-limited-tree parts % all-root-names) children)]
    (vec (apply concat [root-name] descendents))))

(declare get-parts-with-type)
(declare get-tail-transform)
(declare compute-transforms)
(declare create-kinematic-bodies)

(defn get-root-parts [world]
  (let [chip-names (get-parts-with-type (:parts world) :chip)
        ground-children (keys (:ground-children world))
        chip-children (apply concat (map (fn [chip-name]
                                           (let [chip (get-in world [:parts chip-name])]
                                             (keys (:functions chip))))
                                         chip-names))
        cpu-names (get-parts-with-type (:parts world) :cpu)
        cpu-children (apply concat (map (fn [cpu-name]
                                          (let [cpu (get-in world [:parts cpu-name])]
                                            (concat (keys (:inputs cpu))
                                                    (keys (:outputs cpu)))))
                                        cpu-names))
        roots (concat chip-children
                      cpu-children)

        roots (filter (fn [name]
                        (let [part (get-in world [:parts name])]
                          (not (in? (:type part) [:chip :button]))))
                      roots)

        roots (concat ground-children roots)]
    (into [] (into #{} roots))))

(defn segregate-parts [world]
  (let [roots (get-root-parts world)]
    (vec (map (fn [root]
                (get-limited-tree (:parts world) root roots))
              roots))))

(defn bake-mesh [mesh transform scale color-name]
  (let [vertices (map (fn [v]
                        (let [sv (map (fn [a b]
                                        (* a b)) v scale)]
                          (apply-transform transform sv)))
                      (vec (partition 3 (:vertices mesh))))
        color (let [color (get-color color-name)
                    r (/ (get-red color) 255.0)
                    g (/ (get-green color) 255.0)
                    b (/ (get-blue color) 255.0)]
                [r g b 1.0])]
    {:vertices (vec (flatten vertices))
     :colors (vec (flatten (repeat (count vertices) color)))}))

(defn bake-part [info part]
  (let [model (get-in info [(:type part) :model])
        transform (if (= (:type part) :track)
                    (get-tail-transform part)
                    (:transform part))]
    (bake-mesh model transform (:scale part) (:color part))))

(defn create-mesh-from-parts [parts names info edited-part]
  (let [baked-parts (map (fn [name]
                           (if (= name edited-part)
                             {:vertices []
                              :colors []}
                             (bake-part info (get-in parts [name]))))
                         names)
        {:keys [vertices colors]} (reduce (fn [a b]
                                            (merge-with (comp vec concat) a b))
                                          baked-parts)
        root (get-in parts [(first names)])
        root-transform (:transform root)
        inverse-transform (get-inverse-transform root-transform)
        vertices (vec (flatten (map #(apply-transform inverse-transform %)
                                    (partition 3 vertices))))]
    {:vertices (into-array Double/TYPE (map double vertices))
     :vertices-buffer (get-float-buffer vertices)
     :normals-buffer (get-float-buffer (into [] (compute-normals vertices)))
     :colors-buffer (get-float-buffer colors)
     :transform root-transform
     :draw-fn draw-colored-mesh!
     :scale [1 1 1]
     :program :colored}))

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
        weld-groups (map-map (fn [names]
                               (let [children (get-group-children parts names groups)
                                     mesh (-> (create-mesh-from-parts parts names info (:edited-part world))
                                              (assoc-in [:children] children)
                                              (assoc-in [:parts] names))]
                                 {(first names) mesh}))
                             groups)]
    (-> world
        (create-kinematic-bodies parts groups)
        (assoc-in [:weld-groups] weld-groups)
        (compute-transforms :weld-groups))))
