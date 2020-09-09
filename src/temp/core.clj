
(ns temp.core (:gen-class))

(load "util")
(load "world")
(load "vector")
(load "matrix")
(load "analytic-geometry")
(load "camera")
(load "transforms")
(load "window")
(load "xml")
(load "picture")
(load "keymap")
(load "debug")

(load "synthesizer")
(load "miscellaneous")
(load "output")
(load "parts")
(load "collision")
(load "weld-optimization")
(load "mechanical-tree")
(load "undo")
(load "persistence")
(load "value-force")
(load "value-collision")
(load "modes")
(load "commands")
(load "track-loop")
(load "hints")

(do
1

(defn create-world []
  (-> (create-base-world)
      (assoc-in [:small-gear] ;;###################################
                (create-model-mesh "res/small-gear.obj"
                                   [0 1 0] [1 0 0 0] 0.25 :gray))
      (assoc-in [:large-gear] ;;###################################
                (create-model-mesh "res/large-gear.obj"
                                   [0.75 1 0] [0 1 0 10] 0.25
                                   (make-color 50 50 50)))
            
      (merge (read-string (str "{" (slurp "settings.clj") "}")))
      (assoc-in [:num-lines] 1)
      (assoc-in [:background-meshes :grid] (create-grid-mesh 24 0.5))
      (assoc-in [:info] (create-info))
      (assoc-in [:parts] {})
      (assoc-in [:parts :ground-part] (create-ground-part))

      (assoc-in [:other-ground]
                (create-cube-mesh [0 -0.1 0] [1 0 0 0] [12 0.2 12]
                                  (make-color 40 40 40)))
      (assoc-in [:graph-box] {:x 343 :y 530
                              :w 685 :h 150
                              :buffer (new-image 685 150)
                              })
      (assoc-in [:motherboard-box] {:x 343 :y 530
                                    :w 685 :h 150
                                    :buffer (new-image 685 150)})
      (assoc-in [:property-box]
                (create-picture "property-menu" 240 340 -1 60))
      (assoc-in [:layer-box] {:x 343 :y 575 :w 480 :h 60})
      (assoc-in [:toggle-box] {:x 343 :y 575 :w 500 :h 60})
      (assoc-in [:visible-layers] [1])
      (assoc-in [:command] "")
      (assoc-in [:bindings] (get-bindings))
      (assoc-in [:current-color] :red)
      
      (assoc-in [:action-menu]
                (create-picture "action-menu" 240 340 40 -1))
      (assoc-in [:mode-menu]
                (create-picture "mode-menu" 240 340 40 -1))
      
      (assoc-in [:color-palette]
                (create-picture "colors" 340 585 -1 40))
      (assoc-in [:add-menu]
                (create-picture "add-menu" 726 675 -1 50))
      (assoc-in [:add-type] :block)

      (assoc-in [:edit-menu]
                (create-picture "edit-menu" 210 575 -1 50))
      (assoc-in [:edit-subcommand] :move)

      (assoc-in [:use-weld-groups] true)
      (assoc-in [:graph-snap-value] 0.05)

      (assoc-in [:graph-menu]
                (create-picture "graph-menu" 210 575 -1 30))

      (assoc-in [:motherboard-menu]
                (create-picture "motherboard-menu" 210 575 -1 30))
      (assoc-in [:selected-property] 0)
      (assoc-in [:properties] [:free :physics :collision :.])

      (create-physics-world)
      (reset-undo! [:parts])
      (assoc-in [:mode] :simulation)

      (assoc-in [:track-head-model]
                (create-cube-mesh [0 -10000 0] [1 0 0 0] 0.2 :white))
      (place-elements)

      ;; (assoc-in [:update-cube] ;;###################################
      ;;           (create-cube-mesh [0 0 0] [1 0 0 0] 0.1 :red))

      (create-weld-groups)
      ))
(reset-world!)
)

(defn draw-cable! [world part-a part-b]
  (let [mesh (get-in world [:info :cylinder :model])
        position-a (get-transform-position
                    (get-in world [:parts part-a :transform]))
        position-b (get-transform-position
                    (get-in world [:parts part-b :transform]))
        mid-point (vector-multiply (vector-add position-a position-b) 0.5)
        v (vector-subtract position-b position-a)
        mesh (set-mesh-position mesh mid-point)
        mesh (point-mesh-towards mesh v)
        l (vector-length v)
        mesh (assoc-in mesh [:scale] [0.05 l 0.05])
        mesh (assoc-in mesh [:color] [0.6 0.6 0.6 1])]
    (draw-mesh! world mesh)))

(set-thing! [:mode] :debug)
(defn update-world [world elapsed]
  (cond
    (in? (:mode world) [:simulation :graph :motherboard])
    (let [elapsed 16 ;;######################
          ;; v  (get-in world [:parts :wagon9806 :value])
          v (+ 0.05 (* 2 (- 1 (get-in world [:parts :track9247-copy9249 :value]))))
          
          world (-> world
                    (set-probe-values)
                    (save-values)
                    (run-chips elapsed)
                    (apply-force elapsed)
                    (compute-transforms (if (:use-weld-groups world)
                                          :weld-groups
                                          :parts))
                    (assoc-in [:parts :track9247 :value] v)
                    (reverse-collisions)
                    (update-motherboards))]
      (recompute-body-transforms! world)
      (step-simulation! (:planet world) elapsed)
      world)

    (= (:mode world) :property)
    (-> world
        (apply-force elapsed)
        (compute-transforms (if (:use-weld-groups world)
                              :weld-groups
                              :parts)))
    :else world)
  )

(defn draw-update-cube! [world] ;;#########################
  (if-let [mesh (:update-cube world)]
    (let [green-value (if (float= (second (:color mesh)) 1.0)
                        0.0
                        1.0)]
      (set-thing! [:update-cube :color 1] green-value)
      (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
      (draw-mesh! world mesh)))
  )

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))

  ;; (draw-cable! world :block9823 :sphere9816)
  ;; (draw-cable! world :sphere9816 :block9823-copy9824)

  (if-let [transform (get-in world [:weld-groups :track9247 :transform])]
    (let [mesh (:small-gear world)
          t (combine-transforms
             transform
             (make-transform [0 -0.8 0] [1 0 0 0]))
          mesh (assoc-in mesh [:transform] t)]
      (draw-mesh! world mesh)))
  
  (if-let [transform (get-in world [:weld-groups :track9247-copy9249 :transform])]
    (let [mesh (:large-gear world)
          t (combine-transforms
             transform
             (make-transform [0 -0.5 0] [1 0 0 0]))
          mesh (assoc-in mesh [:transform] t)]
      (draw-mesh! world mesh)))

  ;; (draw-mesh! world (:small-gear world))
  ;; (draw-mesh! world (:large-gear world))
  
  ;; (doseq [mesh (vals (:meshes world))] ;;##################
  ;;   (draw-mesh! world mesh))

  (if (> (get-in world [:camera :x-angle]) 0)
    (draw-mesh! world (:other-ground world)))

  (draw-spheres! world)
  
  (if (:use-weld-groups world)
    (doseq [group (vals (:weld-groups world))]
      (draw-mesh! world group))
    (doseq [[name part] (:parts world)]
      (if (or (= name :ground-part)
              (not (in? (:layer part) (:visible-layers world))))
        nil
        (draw-part! world part))))

  (draw-track-head! world)

  (draw-selection! world)
  (draw-buttons! world)
  (draw-lamps! world)
  (draw-debug-meshes!) ;;##############################
  ;; (draw-update-cube! world) ;;#########################
  )

(do
1

(defn show-buttons? [world]
  (or
   (= (:show-buttons world) :always)
   (and
    (= (:show-buttons world) :no-sim)
    (not (= (:mode world) :simulation)))))

(defn draw-2d! [world]
  (clear!)

  (when (show-buttons? world)
    (let [{:keys [image x y w h]} (:action-menu world)]
      (fill-rect! (make-color 70 70 70) x y (+ 20 w) (+ 20 h))
      (draw-image! image x y))

    (let [{:keys [image x y w h]} (:mode-menu world)]
      (fill-rect! (make-color 70 70 70) x y (+ 20 w) (+ 20 h))
      (draw-image! image x y))
    )
  
  (if-let [fun (get-function (:mode world) :draw)]
    (fun world))

  (draw-buffer! world)
  (draw-hint! world)
  )
(redraw!))

(defn mouse-scrolled [world event]
  (if (and (= (:mode world) :graph)
           (inside-box? (:graph-box world) (:x event) (:y event)))
    (graph-mode-scrolled world event)
    (let [amount (+ 1 (* (:amount event) -0.05))]
      (zoom-camera world amount))))

(defn action-menu-pressed [world x y]
  (if-let [region (get-region-at (:action-menu world) x y)]
    (let [world (case region
                  :new (-> world
                           (new-file)
                           (tree-changed))
                  :view (view-all-parts world)
                  :save (save-machine-version world)
                  :open (open-machine-version world)
                  :undo (undo! world)
                  :redo (redo! world)
                  :cancel (cancel-action world))]
      (show-hint world :action region))
    world))

(defn mode-menu-pressed [world x y]
  (if-let [region (get-region-at (:mode-menu world) x y)]
    (-> world
        (change-mode region)
        (show-hint :mode region))
    world))

(defn mouse-pressed [world event]
  (let [x (:x event)
        y (:y event)
        world (-> world
                  (assoc-in [:press-time] (get-current-time))
                  (assoc-in [:press-point] [x y]))]
    (cond
      (and
       (show-buttons? world)
       (inside-box? (:action-menu world) x y))
      (action-menu-pressed world x y)

      (and
       (show-buttons? world)
       (inside-box? (:mode-menu world) x y))
      (mode-menu-pressed world x y)

      (and
       (in? (:button event) [:middle :right])
       (not (and (= (:mode world) :graph)
                 (inside-box? (:graph-box world) x y))))
      (assoc-in world [:last-point] [x y])

      :else
      (mode-mouse-pressed world event))))

(defn mouse-moved [world event]
  (cond
    (not-nil? (:last-point world))
    (cond
      (= (:button event) :right) (mouse-rotate world event)
      (= (:button event) :middle) (mouse-pan world event)
      :else world)
    :else
    (mode-mouse-moved world event)))

(defn mouse-released [world event]
  (let [elapsed (- (get-current-time) (:press-time world))
        world (if (and (< elapsed 200)
                       (= (:button event) :right)
                       (< (distance (:press-point world)
                                    [(:x event) (:y event)]) 10))
                (set-pivot world event)
                world)]
    (if (not-nil? (:last-point world))
      (dissoc-in world [:last-point])
      (mode-mouse-released world event))))

(defn window-changed [world event]
  (let [{:keys [width height]} event]
    (-> world
        (recompute-viewport width height)
        (place-elements))))
