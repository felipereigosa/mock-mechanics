
(ns temp.core)

(do
1

(defn create-info []
  {:ground
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :direction nil
    }

   :block
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points [[0.5 0 0] [-0.5 0 0]
             [0 0.5 0] [0 -0.5 0]
             [0 0 0.5] [0 0 -0.5]]
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :white
    :properties {:value 0}
    }

   :cylinder
   {:model (create-model-mesh "resources/cylinder.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points [[0 0.5 0] [0 -0.5 0]]
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :orange
    :properties {:value 0}
    }

   :sphere
   {:model (create-model-mesh "resources/sphere.obj"
                              [0 0 0] [1 0 0 0]
                              [1 1 1] :white)
    :points [[0 0.5 0] [0 -0.5 0]]
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :blue
    :properties {:value 0}
    }

   :cone
   {:model (create-model-mesh "resources/cone.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.5 0.5 0.5]
    :direction :input
    :color :green
    :properties {:value 0}
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
    :properties {:value 0
                 :snap 0}
    }

   :probe
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.1 0.1 0.1]
    :direction :input
    :color :purple
    :properties {:value 0}
    }

   :track
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points [[0.2 0 0] [-0.2 0 0]
             [0 0.2 0]
             [0 0 0.2] [0 0 -0.2]]
    :scale [0.1 1 0.1]
    :direction :output
    :color :red
    :properties {:value 0
                 :snap 0}
    }

   :chip
   {:model (create-model-mesh "resources/chip.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "resources/chip.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :poinpts []
    :scale [0.3 0.07 0.3]
    :direction :output
    :color :dark-gray
    }
   
   :speaker
   {:model (create-model-mesh "resources/speaker.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "resources/speaker.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.4 0.1 0.4]
    :direction :output
    :color :gray
    :properties {:value 0
                 :frequency 440}
    }

   :cpu
   {:model (create-model-mesh "resources/cpu.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "resources/cpu.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.3 0.07 0.3]
    :direction nil
    :color :blue
    }

   :button
   {:model (create-model-mesh "resources/button.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "resources/button.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :cap (create-model-mesh "resources/cylinder.obj"
                            [0 0 0] [1 0 0 0] [0.4 0.2 0.4] :red)
    :points []
    :scale [0.5 0.2 0.5]
    :direction :input
    :color :red
    :properties {:value 0}
    }

   :lamp
   {:model (create-model-mesh "resources/lamp-base.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "resources/lamp-base.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :bulb (create-model-mesh "resources/bulb.obj"
                            [0 0 0] [1 0 0 0] [0.3 0.3 0.3] :red)
    :points []
    :scale [0.4 0.2 0.4]
    :direction :output
    :color :red
    :properties {:value 0}
    }
   })
(set-thing! [:info] (create-info))
)

(defn create-ground-part []
  {:type :ground
   :transform (make-transform [0 -0.1 0] [1 0 0 0])
   :color :dark-gray
   :scale [12 0.2 12]
   :children {}})

(defn create-part [type color layer info]
  (let [type-info (get-in info [type])
        part {:type type
              :color color
              :layer layer
              :transform (make-transform [0 0.5 0] [0 1 0 0])
              :scale (:scale type-info)}

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
                   (assoc-in [:view] {:offset [0.025 0.1]
                                      :zoom-x 0.5
                                      :zoom-y 0.5}))
               part)]
    (merge part (:properties type-info))))

(defn create-relative-transform [world child-name parent-name]
  (let [parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        child (get-in world [:parts child-name])
        child-transform (:transform child)
        final-transform (if (= (:type child) :wagon)
                          (make-transform [0 0 0] [1 0 0 0])
                          (remove-transform child-transform
                                            parent-transform))]
    (assoc-in world [:parts parent-name :children child-name]
              final-transform)))

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
  (find-if (fn [name]
             (let [parent (get-in world [:parts name])]
               (in? child-name (keys (:children parent)))))
           (keys (:parts world))))

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

(defn set-probe-values [world]
  (let [probe-relative-transforms
        (filter (fn [[name value]]
                  (= (:type value) :probe))
                (:root-relative-transforms world))
        probe-names (keys probe-relative-transforms)
        positions (map (fn [[probe-name {:keys [root-name transform]}]]
                         (let [root (get-in world [:weld-groups root-name])
                               root-transform (:transform root)
                               transform (combine-transforms transform root-transform)]
                           (get-transform-position transform)))
                       probe-relative-transforms)
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

(declare use-root-relative-transform)

(defn draw-buttons! [world]
  (let [button-names (get-parts-with-type (:parts world) :button)]
    (doseq [button-name button-names]
      (let [button (get-in world [:parts button-name])]
        (if (in? (:layer button) (:visible-layers world))
          (let [base-transform (use-root-relative-transform world button-name)
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
                        (:color button)
                        (if (get-in button [property])
                          :red
                          :white))
                button-mesh (get-in world [:info :button :cap])
                mesh (-> button-mesh
                         (assoc-in [:transform] transform)
                         (assoc-in [:color] (get-color-vector color)))]
            (draw-mesh! world mesh)))))))

(defn draw-lamps! [world]
  (let [lamp-names (get-parts-with-type (:parts world) :lamp)]
    (doseq [lamp-name lamp-names]
      (let [lamp (get-in world [:parts lamp-name])]
        (if (in? (:layer lamp) (:visible-layers world))
          (let [base-transform (use-root-relative-transform world lamp-name)
                property (nth (get-in world [:properties])
                              (:selected-property world))
                color (if (and (float= (:value lamp) 0)
                               (in? (:mode world) [:simulation :cpu :set-value]))
                        (:dark-color lamp)
                        (:color lamp))
                
                color (if (not= (:mode world) :toggle)
                        color
                        (if (get-in lamp [property])
                          :red
                          :white))
                bulb-mesh (get-in world [:info :lamp :bulb])
                mesh (-> bulb-mesh
                         (assoc-in [:transform] base-transform)
                         (assoc-in [:color] (get-color-vector color)))]
            (draw-mesh! world mesh)))))))

(declare save-checkpoint!)

(defn prepare-tree [world]
  (if (= (:mode world) :simulation)
    world
    (-> world
        (compute-transforms :parts)
        (create-weld-groups)
        (save-checkpoint!))))

(defn select-part [world part-name]
  (let [part (get-in world [:parts part-name])
        type (:type part)
        scale (vector-multiply (:scale part) 1.01)
        transform (if (= type :track)
                    (get-tail-transform part)
                    (:transform part))
        color (if (color= (:color part) :yellow)
                [0 0 1 0]
                [1 1 0 0])
        mesh (or
              (get-in world [:info type :white-model])
              (get-in world [:info type :model]))]
    (if (= type :ground)
      world
      (-> world
          (assoc-in [:selection :time] (get-current-time))
          (assoc-in [:selection :mesh] mesh)
          (assoc-in [:selection :mesh :color] color)
          (assoc-in [:selection :mesh :scale] scale)
          (assoc-in [:selection :mesh :transform] transform)))))

(defn draw-selection! [world]
  (if-let [{:keys [mesh time]} (:selection world)]
    (if (< (- (get-current-time) time) 300)
      (draw-mesh! world mesh))))

(defn snap-part [world part-name]
  (let [part (get-in world [:parts part-name])]
    (if (float= (:snap part) 0.0)
      world
      (let [snap (if (= (:type part) :wagon)
                   (/ (:snap part) (reduce + (:track-lengths part)))
                   (:snap part))]
        (assoc-in world [:parts part-name :value] 
                  (snap-value (:value part) snap))))))
