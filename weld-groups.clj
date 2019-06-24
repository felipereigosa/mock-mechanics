;; -------------------------------------------------------------------------------;;
;; weld groups

(defn get-limited-tree [parts root-name all-root-names]
  (let [root (get-in parts [root-name])
        children (filter (fn [name]
                           (not (in? name all-root-names)))
                         (keys (get-in root [:children])))
        descendents (map #(get-limited-tree parts % all-root-names) children)]
    (vec (apply concat [root-name] descendents))))

(defn segregate-parts [world]
  (let [roots (concat (keys (:ground-children world))
                      (keys (get-in world [:wave-editor :functions]))
                       ;; + cable dof parts
                      )]
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

(defn bake-arm [mesh transform direction color-name]
  (let [offset (vector-multiply direction 0.25)
        [x y z] direction
        rotation (cond
                   (not (zero? x)) [0 0 1 90]
                   (not (zero? y)) [1 0 0 0]
                   (not (zero? z)) [1 0 0 90])
        arm-transform (make-transform offset rotation)
        final-transform (combine-transforms arm-transform transform)]
    (bake-mesh mesh final-transform (:scale mesh) color-name)))

(defn opposite-directions? [directions]
  (some #(vector-= % [0 0 0])
        (mapcat (fn [a]
                  (map (fn [b]
                         (vector-add a b))
                       directions))
                directions)))

(defn bake-part [info part]
  (if (= (:type part) :block)
    (let [model (get-in info [(:type part) :model])]
      (bake-mesh model (:transform part) (:scale model) (:color part)))
    (let [directions (:directions part)
          transform (:transform part)
          arm (get-in info [:track :arm])          
          baked-arms (map #(bake-arm arm transform % (:color part)) directions)
          baked (if (opposite-directions? directions)
                  baked-arms
                  (let [middle (get-in info [:block :model])
                        baked-middle (bake-mesh middle transform
                                                [0.2 0.2 0.2] (:color part))]
                    (cons baked-middle baked-arms)))]          
      (reduce (fn [a b]
                (merge-with (comp vec concat) a b))
              baked))))

(defn create-mesh-from-parts [parts names info]
  (let [baked-parts (map (fn [name]
                           (bake-part info (get-in parts [name])))
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
  (apply merge
         (map (fn [other-group]
                (if (is-child-group? parts group other-group)
                  (let [parent (get-in parts [(first group)])
                        child (get-in parts [(first other-group)])
                        child-transform (:transform child)
                        parent-transform (:transform parent)
                        relative-transform (remove-transform child-transform
                                                             parent-transform)]
                  {(first other-group) relative-transform})))
              groups)))

(defn reset-part-values [world]
  (let [parts (apply merge (map (fn [[name part]]
                                  {name (assoc-in part [:value] 0.0)})
                                (:parts world)))]
    (assoc-in world [:parts] parts)))

(declare compute-transforms)

(defn create-weld-groups [world]
  (let [groups (segregate-parts world)
        parts (:parts (compute-transforms (reset-part-values world) :parts))
        info (:info world)
        weld-groups (apply merge
                      (map (fn [names]
                             (let [mesh (create-mesh-from-parts parts names info)
                                   children (get-group-children parts names groups)
                                   mesh (assoc-in mesh [:children] children)]
                               {(first names) mesh}))
                           groups))]
    (assoc-in world [:weld-groups] weld-groups)))

