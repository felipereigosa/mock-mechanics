
(ns temp.core)

(defn get-limited-tree [parts root-name all-root-names]
  (let [root (get-in parts [root-name])
        children (filter (fn [name]
                           (not (in? name all-root-names)))
                         (keys (get-in root [:children])))
        descendents (map #(get-limited-tree parts % all-root-names)
                         children)]
    (vec (apply concat [root-name] descendents))))

(declare get-parts-with-type)
(declare get-tail-transform)
(declare compute-transforms)
(declare create-kinematic-bodies)

(defn get-root-parts [world]
  (let [ground-children (keys (:ground-children world))
        
        chip-names (get-parts-with-type (:parts world) :chip)
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
        roots (concat ground-children chip-children free-parts probes
                      (filter not-nil? [force-part track-force-part]))]
    (vec (into #{} roots))))

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

(defn get-tail-transform [track]
  (let [track-transform (:transform track)
        y-offset (* -0.5 (second (:scale track)))]
    (combine-transforms
     (make-transform [0 y-offset 0] [1 0 0 0])
     track-transform)))

(defn bake-part [info part property]
  (let [type (:type part)
        model (get-in info [type :model])
        color (if (or (= type :ground)
                      (nil? property))
                (if (:hidden part)
                  nil
                  (:color part))
                (if (get-in part [property])
                  :red
                  :white))
        transform (if (= type :track)
                    (get-tail-transform part)
                    (:transform part))]
    (if (nil? color)
      {:vertices []
       :colors []}
      (bake-mesh model transform (:scale part) color))))

(defn create-mesh-from-parts [parts names info edited-part property]
  (let [baked-parts (map (fn [name]
                           (if (= name edited-part)
                               {:vertices []
                                :colors []}
                             (bake-part info (get-in parts [name]) property)))
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
        property (if (= (:mode world) :toggle)
                   (nth (get-in world [:properties])
                        (:selected-property world))
                   nil)
        weld-groups (map-map (fn [names]
                               (let [children (get-group-children parts names groups)
                                     mesh (-> (create-mesh-from-parts parts names info (:edited-part world) property)
                                              (assoc-in [:children] children)
                                              (assoc-in [:parts] names))]
                                 {(first names) mesh}))
                             groups)]
     (-> world
        (create-kinematic-bodies parts groups)
        (assoc-in [:weld-groups] weld-groups)
        (compute-transforms :weld-groups))))
