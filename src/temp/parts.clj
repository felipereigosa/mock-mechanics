
(ns temp.core)

(defn create-info []
  {:ground
   {:model (create-plane-mesh [0 0 0] [1 0 0 0]
                              [1 1 1] :white)
    :points []
    :direction nil
    }

   :block
   {:model (create-model-mesh "resources/cube.obj"
                              [0 0 0] [1 0 0 0]
                              [1 1 1] :white)
    :points [[0.5 0 0] [-0.5 0 0]
             [0 0.5 0] [0 -0.5 0]
             [0 0 0.5] [0 0 -0.5]]
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :white
    }

   :cylinder
   {:model (create-model-mesh "resources/cylinder.obj"
                              [0 0 0] [1 0 0 0]
                              [1 1 1] :white)
    :points [[0 0.5 0] [0 -0.5 0]]
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :orange
    }

   :sphere
   {:model (create-model-mesh "resources/sphere.obj"
                              [0 0 0] [1 0 0 0]
                              [1 1 1] :white)
    :points [[0 0.5 0] [0 -0.5 0]]
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :blue
    }

   :cone
   {:model (create-model-mesh "resources/cone.obj"
                              [0 0 0] [1 0 0 0]
                              [1 1 1] :white)
    :points []
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :green
    }

   :wagon
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points [[0.5 0 0] [-0.5 0 0]
             [0 0.5 0] [0 -0.5 0]
             [0 0 0.5] [0 0 -0.5]]
    :scale [0.15 0.15 0.15]
    :direction :output
    :color :yellow
    }

   :probe
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points []
    :scale [0.1 0.1 0.1]
    :direction :input
    :color :purple
    }

   :track
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points [[0.2 0 0] [-0.2 0 0]
             [0 0.2 0]
             [0 0 0.2] [0 0 -0.2]
             ]
    :scale [0.1 1 0.1]
    :direction :output
    :color :red
    }

   :chip
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points []
    :scale [0.3 0.1 0.3]
    :direction :output
    :color :gray
    }

   :cpu
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points []
    :scale [0.3 0.1 0.3]
    :direction nil
    :color :blue
    }

   :button
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points []
    :scale [0.5 0.2 0.5]
    :direction :input
    :color :red
    }

   :lamp
   {:model (create-cylinder-mesh [0 0 0] [1 0 0 0]
                                 [1 1 1] :white)
    :points []
    :scale [0.2 0.2 0.2]
    :direction :output
    :color :red
    }
   })

(defn create-part [type color layer info]
  (let [part {:type type
              :color color
              :value 0
              :layer layer
              :transform (make-transform [0 0.5 0] [0 1 0 0])
              :scale (get-in info [type :scale])}

        part (if (= type :lamp)
               (assoc-in part [:dark-color] (get-dark-color color))
               part)

        part (if (= type :cpu)
               (assoc-in part [:tab] 0)
               part)

        part (if (= type :chip)
               (-> part
                   (assoc-in [:functions] {})
                   (assoc-in [:time] 1.0)
                   (assoc-in [:final-time] 0.0)
                   (assoc-in [:view] {:offset [0 0]
                                      :zoom 1}))
               part)]
    part))

(defn create-relative-transform [world child-name parent-name]
  (if (= parent-name :ground)
    (assoc-in world [:ground-children child-name]
              (get-in world [:parts child-name :transform]))
    (let [parent (get-in world [:parts parent-name])
          parent-transform (:transform parent)
          child (get-in world [:parts child-name])
          child-transform (:transform child)
          final-transform (remove-transform child-transform
                                            parent-transform)]
      (assoc-in world [:parts parent-name :children child-name]
                final-transform))))

(defn create-ground-part [world]
  (let [color (make-color 40 40 40)
        part (create-part :ground color nil (:info world))
        name :ground-part
        transform (make-transform [0 0 0] [1 0 0 -90])
        scale [12 12 1]]
      (-> world
          (assoc-in [:parts name] part)
          (assoc-in [:parts name :transform] transform)
          (assoc-in [:parts name :scale] scale)
          (create-relative-transform name :ground))))

(defn get-part-with-color [world color]
  (first (find-if (fn [[name part]]
                    (= (:color part) color))
                  (:parts world))))

(defn get-part-position [world name]
  (let [transform (get-in world [:parts name :transform])]
    (get-transform-position transform)))

(defn set-part-position [part position]
  (let [transform (:transform part)
        rotation (get-transform-rotation transform)]
    (assoc-in part [:transform]
              (make-transform position rotation))))

(defn get-parent-part [world child-name]
  (if (in? child-name (keys (:ground-children world)))
    :ground
    (find-if (fn [name]
               (let [parent (get-in world [:parts name])]
                 (in? child-name (keys (:children parent)))))
             (keys (:parts world)))))

(defn get-parts-with-type [parts type]
  (map first (filter (fn [[name part]]
                       (= (:type part) type))
                     parts)))

(defn print-parts! []
  (println! (keys (:parts @world))))

(defn get-tail-transform [track]
  (let [track-transform (:transform track)
        y-offset (* -0.5 (second (:scale track)))]
    (combine-transforms
     (make-transform [0 y-offset 0] [1 0 0 0])
     track-transform)))

(defn draw-part! [world part]
  (let [info (get-in world [:info (:type part)])
        transform (if (= (:type part) :track)
                    (get-tail-transform part)
                    (:transform part))
        mesh (-> (:model info)
                 (assoc-in [:transform] transform)
                 (assoc-in [:scale] (:scale part))
                 (set-mesh-color (:color part)))]
    (draw-mesh! world mesh)))

(defn get-part-offset [part]
  (if (= (:type part) :track)
    (second (:scale part))
    (* 0.5 (second (:scale part)))))

(defn set-value-0-transform [world part-name]
  (let [parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        relative-transform (get-in parent [:children part-name])
        transform (combine-transforms relative-transform parent-transform)]
    (assoc-in world [:parts part-name :transform] transform)))

(defn show-selected-part [world part-name]
  (let [part (get-in world [:parts part-name])]
     (assoc-in world [:selected-mesh :transform] (:transform part))))

