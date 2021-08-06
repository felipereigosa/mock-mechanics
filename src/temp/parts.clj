
(defn create-info []
  {:ground
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    }

   :block
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :cage (create-model-mesh "res/cage.obj"
                             [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points [[0.5 0 0] [-0.5 0 0]
             [0 0.5 0] [0 -0.5 0]
             [0 0 0.5] [0 0 -0.5]]
    :scale [0.5 0.5 0.5]
    :color :white
    :properties {:value 0 :data "" :skin ""}
    }

   :cylinder
   {:model (create-model-mesh "res/cylinder.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] :white)
    :collision-model (create-model-mesh "res/cylinder-collision.obj"
                                        [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points [[0 0.5 0] [0 -0.5 0]]
    :scale [0.5 0.5 0.5]
    :color :orange
    :properties {:value 0 :data ""}
    }

   :cone
   {:model (create-model-mesh "res/cone.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] :white)
    :collision-model (create-model-mesh "res/cone-collision.obj"
                                        [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.5 0.5 0.5]
    :color :dark-green
    :properties {:value 0 :data ""}
    }

   :sphere
   {:model (create-model-mesh "res/sphere.obj"
                              [0 0 0] [1 0 0 0]
                              [1 1 1] :white)
    :collision-model (create-model-mesh "res/sphere-collision.obj"
                                        [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.5 0.5 0.5]
    :color :blue
    :properties {:value 0 :data ""}
    }

   :gear
   {:model (create-model-mesh "res/cylinder.obj" ;;################
                              [0 0 0] [1 0 0 0] [1 1 1] :white)
    :collision-model (create-model-mesh "res/cylinder-collision.obj"
                                        [0 0 0] [1 0 0 0] [1 1 1] :white)

    :body (create-model-mesh "res/gear-body.obj"
                             [0 0 0] [1 0 0 0] [1 1 1] :gray)

    :tooth (create-model-mesh "res/gear-tooth.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] :gray)

    :points []
    :scale [0.5 0.5 0.5]
    :properties {}
    }

   :rack
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.5 0.5 0.5]
    :properties {}
    }

   :wagon
   {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                             [1 1 1] :white)
    :points [[0.5 0 0] [-0.5 0 0]
             [0 0.5 0] [0 -0.5 0]
             [0 0 0.5] [0 0 -0.5]]
    :scale [0.15 0.15 0.15]
    :color :yellow
    :properties {:value 0
                 :snap nil}
    }

   :probe
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.1 0.1 0.1]
    :color :purple
    :properties {:value 0}
    }

   :track
   {:model (create-model-mesh "res/track.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points [[0.2 0 0] [-0.2 0 0]
             [0 0.2 0]
             [0 0 0.2] [0 0 -0.2]]
    :scale [0.1 1 0.1]
    :color :red
    :properties {:value 0
                 :free false
                 :snap nil
                 :max-angle nil}
    }

   :chip
   {:model (create-model-mesh "res/chip.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "res/chip.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :poinpts []
    :scale [0.3 0.07 0.3]
    :color :dark-gray
    :properties {:value 0}
    }
   
   :speaker
   {:model (create-model-mesh "res/speaker.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "res/speaker.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :collision-model (create-model-mesh "res/cylinder-collision.obj"
                                        [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.4 0.1 0.4]
    :color :gray
    :properties {:value 0
                 :frequency 440}
    }

   :motherboard
   {:model (create-model-mesh "res/motherboard.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "res/motherboard.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.3 0.07 0.3]
    
    :color :blue
    }

   :button
   {:model (create-model-mesh "res/button.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "res/button.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :cap (create-model-mesh "res/cylinder.obj"
                            [0 0 0] [1 0 0 0] [0.4 0.2 0.4] :red)
    :points []
    :scale [0.5 0.2 0.5]
    :color :red
    :properties {:value 0}
    }

   :lamp
   {:model (create-model-mesh "res/lamp-base.obj"
                              [0 0 0] [1 0 0 0] [1 1 1] nil)
    :white-model (create-model-mesh "res/lamp-base.obj"
                                    [0 0 0] [1 0 0 0] [1 1 1] :white)
    :bulb (create-model-mesh "res/bulb.obj"
                             [0 0 0] [1 0 0 0] [0.3 0.3 0.3] :red)
    :collision-model (create-model-mesh "res/cylinder-collision.obj"
                                        [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.4 0.2 0.4]
    :color :red
    :properties {:value 0}
    }

   :display
   {:model (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
    :points []
    :scale [0.5 0.05 0.3]
    :color :black
    :properties {}
    }
   })

(defn create-ground-part []
  {:type :ground
   :transform (make-transform [0 -0.1 0] [1 0 0 0])
   :color :dark-gray
   :scale [12 0.2 12]
   :children {}})

(defn create-bitmap [width height]
  (let [bitmap (new-image width height)]
    (clear bitmap :black)
    bitmap))

(defn create-display-texture [display]
  (let [vertices (vec (flatten
                       [-0.5 0 0.5
                        0.5 0 0.5
                        -0.5 0 -0.5
                        -0.5 0 -0.5
                        0.5 0 0.5
                        0.5 0 -0.5]))
        position [0 0 0]
        rotation [1 0 0 0] 
        scale (:scale display)
        [sx _ sz] scale
        tex-coords [0 1 1 1 0 0 0 0 1 1 1 0]
        normals (vec (compute-normals vertices))
        base-mesh {:vertices vertices
                   :vertices-buffer (get-float-buffer vertices)
                   :normals normals
                   :normals-buffer (get-float-buffer normals)
                   :transform (make-transform position rotation)
                   :scale scale}
        texture-id (GL11/glGenTextures)
        tex-coords (vec (flatten tex-coords))
        width (* 400 sx)
        height (* 400 sz)
        texture (-> base-mesh
                    (assoc-in [:draw-fn] draw-textured-mesh!)
                    (assoc-in [:program] :textured)
                    (assoc-in [:image] (create-bitmap width height))
                    (assoc-in [:texture-coordinates] tex-coords)
                    (assoc-in [:texture-coordinates-buffer]
                              (get-float-buffer tex-coords))
                    (assoc-in [:texture-id] texture-id)
                    (set-texture))]
    (assoc-in display [:texture] texture)))

(defn resize-display-texture [world display-name]
  (let [display (get-in world [:parts display-name])
        old-image (get-in display [:texture :image])
        display (create-display-texture display)
        texture (:texture display)
        new-image (:image texture)]
    (draw-image new-image old-image 0 0 true)
    (-> world
        (assoc-in [:parts display-name] display)
        (update-in [:parts display-name :texture] reset-texture))))

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

        part (if (= type :wagon)
               (assoc-in part [:free] true)
               part)

        part (if (= type :motherboard)
               (-> part
                 (assoc-in [:script] (slurp "res/default-script.clj"))
                 (assoc-in [:tab] 0))
               part)

        part (if (= type :display)
               (create-display-texture part)                         
               part)

        part (if (= type :chip)
               (-> part
                   (assoc-in [:functions] {})
                   (assoc-in [:time] 1000.0)
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

(defn move-part [world part-name offset]
  (let [part (get-in world [:parts part-name])
        transform (:transform part)
        position (get-transform-position transform)
        parent-name (get-parent-part world part-name)]
    (-> world
        (update-in [:parts part-name]
                      #(set-part-position % (vector-add position offset)))
        (create-relative-transform part-name parent-name))))

(defn get-parts-with-type [parts type]
  (map first (filter (fn [[name part]]
                       (= (:type part) type))
                     parts)))

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
        mesh (-> (or (:model part)
                     (:model info))
                 (set-mesh-color (:color part))
                 (assoc-in [:transform] transform)
                 (assoc-in [:scale] (:scale part)))]
    (draw-mesh! world mesh)))

(defn get-part-offset [part]
  (case (:type part)
    :track (second (:scale part))
    :sphere 0
    (* 0.5 (second (:scale part)))))

(defn set-value-0-transform [world part-name]
  (let [parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        relative-transform (get-in parent [:children part-name])
        transform (combine-transforms relative-transform parent-transform)]
    (assoc-in world [:parts part-name :transform] transform)))

(declare use-root-relative-transform)

(defn set-probe-values [world]
  (let [probe-names (get-parts-with-type (:parts world) :probe)
        positions (map (fn [probe-name]
                         (let [transform (use-root-relative-transform world probe-name)]
                           (get-transform-position transform)))
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
                button-mesh (get-in world [:info :button :cap])
                mesh (-> button-mesh
                         (assoc-in [:transform] transform)
                         (assoc-in [:color] (get-color-vector (:color button))))]
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
                               (in? (:mode world) [:simulation :motherboard :property :avatar]))
                        (:dark-color lamp)
                        (:color lamp))
                bulb-mesh (get-in world [:info :lamp :bulb])
                mesh (-> bulb-mesh
                         (assoc-in [:transform] base-transform)
                         (assoc-in [:color] (get-color-vector color)))]
            (draw-mesh! world mesh)))))))

(defn draw-displays! [world]
  (let [display-names (get-parts-with-type (:parts world) :display)]
    (doseq [display-name display-names]
      (let [display (get-in world [:parts display-name])]
        (if (in? (:layer display) (:visible-layers world))
          (let [base-transform (use-root-relative-transform world display-name)
                rotation (get-transform-rotation (:transform display))
                rotation-transform (make-transform [0 0 0] rotation)
                up (apply-transform rotation-transform [0 1 0])
                offset (make-transform (vector-multiply up 0.026) [1 0 0 0])
                transform (combine-transforms base-transform offset)
                mesh (-> (:texture display)
                         (assoc-in [:transform] transform))]
            (draw-mesh! world mesh)))))))

(defn draw-textured-parts! [world]
  (doseq [[part-name part] (:parts world)]
    (if-let [model (:model part)]
      (if (:texture-coordinates model)
        (let [transform (use-root-relative-transform world part-name)
              model (-> model
                        (assoc-in [:scale] (:scale part))
                        (assoc-in [:transform] transform))]
          (draw-mesh! world model))))))

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
      (do
        (do-later redraw! 350)
        (-> world
            (assoc-in [:selection :time] (get-current-time))
            (assoc-in [:selection :mesh] mesh)
            (assoc-in [:selection :mesh :color] color)
            (assoc-in [:selection :mesh :scale] scale)
            (assoc-in [:selection :mesh :transform] transform))))))

(defn draw-selection! [world]
  (if-let [{:keys [mesh time]} (:selection world)]
    (if (< (- (get-current-time) time) 300)
      (draw-mesh! world mesh))))

(defn snap-part [world part-name]
  (let [part (get-in world [:parts part-name])]
    (if (or (nil? (:snap part))
            (float= (:snap part) 0.0))
      world
      (let [snap (if (= (:type part) :wagon)
                   (/ (:snap part) (reduce + (:track-lengths part)))
                   (:snap part))]
        (assoc-in world [:parts part-name :value] 
                  (snap-value (:value part) snap))))))

(declare create-weld-groups)

(def the-thread (atom nil))

(defn tree-will-change [world]
  (reset! the-thread nil)
  (assoc-in world [:use-weld-groups] false))

(defn async-create-weld-groups [w]
  (.start
   (new Thread
        (proxy [Runnable] []
          (run []
            (try
              (let [fast-world (create-weld-groups w)
                    ]
                (set-thing! [:weld-groups]
                            (:weld-groups fast-world))

                (set-thing! [:root-relative-transforms]
                            (:root-relative-transforms fast-world))

                (set-thing! [:bodies] (:bodies fast-world))
                
                (set-thing! [:use-weld-groups] true)
                (redraw!))
              (catch Exception e))))))
  w)

(declare reset-wagons)

(defn tree-changed [world]
  (-> world
      (reset-wagons)
      (assoc-in [:use-weld-groups] false)
      (async-create-weld-groups)))

(defn get-track-direction [track]
  (let [rotation (get-rotation-component (:transform track))]
    (apply-transform rotation [0 1 0])))
