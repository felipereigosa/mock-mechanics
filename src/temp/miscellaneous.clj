
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
    :scale [0.5 0.5 0.5]
    :direction nil
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
    :direction :output
    :color :blue
    }

   :button
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points []
    :scale [0.5 0.2 0.5]
    :direction :input
    :color :black
    }

   :lamp
   {:model (create-cylinder-mesh [0 0 0] [1 0 0 0]
                                 [1 1 1] :white)
    :points []
    :scale [0.2 0.2 0.2]
    :direction :output
    :color :black
    }
   })

(defn create-part [type color info]
  (let [part {:type type
              :color color
              :value 0
              :transform (make-transform [0 0.5 0] [0 1 0 0])
              :scale (get-in info [type :scale])}]
    (if (= type :chip)
      (-> part
          (assoc-in [:functions] {})
          (assoc-in [:time] 1.0)
          (assoc-in [:final-time] 0.0)
          (assoc-in [:view] {:offset [0 0]
                             :zoom 1}))
      part)))

(defn create-relative-transform [world child-name parent-name]
  (let [child (get-in world [:parts child-name])]
    (if (= parent-name :ground)
      (assoc-in world [:ground-children child-name] (:transform child))
      (let [parent (get-in world [:parts parent-name])
            child-transform (:transform child)
            parent-transform (:transform parent)
            final-transform (remove-transform child-transform parent-transform)]
        (assoc-in world [:parts parent-name :children child-name]
                  final-transform)))))

(defn create-ground-part [world]
  (let [color (make-color 40 40 40)
        part (create-part :ground color (:info world))
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

(defn get-parent-part [world child-name]
  (if (in? child-name (keys (:ground-children world)))
    :ground
    (find-if (fn [name]
               (let [parent (get-in world [:parts name])]
                 (in? child-name (keys (:children parent)))))
             (keys (:parts world)))))

(defn set-value-0-transform [world part-name]
  (let [parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        relative-transform (get-in parent [:children part-name])
        transform (combine-transforms relative-transform parent-transform)]
    (assoc-in world [:parts part-name :transform] transform)))

(defn update-move-plane [world]
  (assoc-in world [:move-plane]
            (get-camera-plane world (get-in world [:camera :pivot]))))

(declare get-part-at)

(defn get-function-value [function t interpolator]
  (let [final-time (first (last function))]
    (cond
      (<= t 0.0) (last (first function))
      (>= t final-time) (last (last function))
      :else
      (let [pairs (map vector function (rest function))
            pair (find-if (fn [[[t0 & _] [t1 & _]]]
                            (<= t0 t t1))
                          pairs)
            t0 (first (first pair))
            t1 (first (second pair))
            s (map-between-ranges t t0 t1 0 1)
            v0 (second (first pair))
            v1 (second (second pair))]
        (interpolator v0 v1 s)))))

(defn create-image [filename x y w h]
  (let [document (read-xml filename)
        image (if (= w -1)
                (parse-svg-from-map-with-height document h)
                (parse-svg-from-map-with-width document w))
        menu {:x x
              :y y
              :w (get-image-width image)
              :h (get-image-height image)
              :image image}
        regions (get-absolute-svg-regions document menu)]
    (assoc-in menu [:regions] regions)))

(defn get-region-at [image x y]
  (first (find-if (fn [[name box]]
                    (inside-box? box x y))
                  (:regions image))))

(defn draw-text-box! [world]
  (let [[text color] (if (:text-input world)
                       [(str (:text world)) :dark-gray]
                       [(:command world) :black])]
    (fill-rect! color 80 13 150 25)
    (draw-text! :green text 15 17 14)))

(defn read-input [world callback]
  (println! "listening for input:")
  (-> world
      (assoc-in [:input-callback] callback)
      (assoc-in [:text-input] true)))

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

(defn set-probe-values [world]
  (let [probe-names (get-parts-with-type (:parts world) :probe)
        positions (map (fn [probe-name]
                         (let [probe (if (:use-weld-groups world)
                                       (get-in world [:weld-groups probe-name])
                                       (get-in world [:parts probe-name]))]
                           (get-transform-position (:transform probe))))
                       probe-names)
        close-pairs (mapcat (fn [i]
                              (map (fn [j]
                                     (let [p1 (nth positions i)
                                           p2 (nth positions j)
                                           d (distance p1 p2)]
                                       (if (< d 0.12)
                                         [i j]
                                         nil)))
                                   (range (inc i) (count probe-names))))
                            (range (dec (count probe-names))))
        close-indices (flatten (filter not-nil? close-pairs))
        close-probes (map #(nth probe-names %) close-indices)]
    (reduce (fn [w probe-name]
              (if (in? probe-name close-probes)
                (assoc-in w [:parts probe-name :value] 1)
                (assoc-in w [:parts probe-name :value] 0)))
            world
            probe-names)))

(defn draw-buttons! [world]
  (let [button-names (get-parts-with-type (:parts world) :button)]
    (doseq [button-name button-names]
      (let [button (get-in world [:parts button-name])
            base-transform (:transform button)
            rotation (get-transform-rotation (:transform button))
            rotation-transform (make-transform [0 0 0] rotation)
            up (apply-transform rotation-transform [0 1 0])
            offset (if (= (:value button) 1)
                     (make-transform (vector-multiply up 0.02) [1 0 0 0])
                     (make-transform (vector-multiply up 0.1) [1 0 0 0]))
            transform (combine-transforms base-transform offset)
            property (nth (get-in world [:properties])
                          (:selected-property world))
            color (if (not= (:mode world) :toggle)
                    (or (:other-color button) [1 0 0 1])
                    (if (get-in button [property])
                      [1 0 0 1]
                      [1 1 1 1]))
            mesh (-> (:button-mesh world)
                     (assoc-in [:transform] transform)
                     (assoc-in [:color] color))]
        (draw-mesh! world mesh)))))

(defn draw-lamps! [world]
  (let [lamp-names (get-parts-with-type (:parts world) :lamp)]
    (doseq [lamp-name lamp-names]
      (let [lamp (get-in world [:parts lamp-name])
            base-transform (:transform lamp)
            property (nth (get-in world [:properties])
                          (:selected-property world))
            other-color (or (:other-color lamp) [1 0 0 1])
            other-color (if (= (:value lamp) 1)
                          other-color
                          (darker-color other-color))
            color (if (not= (:mode world) :toggle)
                    other-color                      
                    (if (get-in lamp [property])
                      [1 0 0 1]
                      [1 1 1 1]))
            mesh (-> (:lamp-mesh world)
                     (assoc-in [:transform] base-transform)
                     (assoc-in [:color] color))]
        (draw-mesh! world mesh)))))

(declare create-weld-groups)
(declare compute-transforms)

(defn prepare-tree [world]
  (if (= (:mode world) :idle)
    world
    (-> world
        (compute-transforms :parts)
        (create-weld-groups))))

(defn get-part-offset [part]
  (if (= (:type part) :track)
    (second (:scale part))
    (* 0.5 (second (:scale part)))))

(defn set-pivot [world event]
  (let [x (:x event)
        y (:y event)
        part-name (get-part-at world x y)
        part (get-in world [:parts part-name])
        pos (cond
              (nil? part-name)
              (let [line (unproject-point world [x y])
                    ground-plane [[0 0 0] [1 0 0] [0 0 1]]]
                (line-plane-intersection line ground-plane))

              (= (:type part) :track)
              (get-transform-position (get-tail-transform part))
              
              :else
              (get-part-position world part-name))]
    (compute-camera (assoc-in world [:camera :pivot] pos))))



