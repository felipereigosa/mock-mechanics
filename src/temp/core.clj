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
(load "gears")
(load "undo")
(load "persistence")
(load "forces")
(load "modes")
(load "replayer")
(load "commands")
(load "track-loop")
(load "hints")
(load "input-indicator")

(do
1

(defn create-world []
  (delete-temp-files!)
  
  (-> (create-base-world)
      (merge (read-string (str "{" (slurp "settings.clj") "}")))
      (assoc-in [:num-lines] 6)
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
      (assoc-in [:layer-box]
                (create-picture "layer-menu" 240 340 -1 200))
      (assoc-in [:toggle-box] {:x 343 :y 575 :w 500 :h 60})
      (create-layer-info)
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
      (assoc-in [:add-offset] 0)

      (assoc-in [:edit-menu]
                (create-picture "edit-menu" 210 575 -1 50))
      (assoc-in [:edit-subcommand] :move)

      (assoc-in [:use-weld-groups] true)
      (assoc-in [:graph-snap-value] 0.05)

      (assoc-in [:graph-menu]
        (create-picture "graph-menu" 210 575 -1 30))
      (assoc-in [:graph-subcommand] :move)

      (assoc-in [:motherboard-menu]
        (create-picture "motherboard-menu" 210 575 -1 30))
      (assoc-in [:motherboard-subcommand] :move)

      (assoc-in [:selected-property] 0)
      (assoc-in [:properties] [:free :solid :.])

      (create-physics-world)
      (reset-undo! [:parts :gears])
      (assoc-in [:mode] :simulation)

      (assoc-in [:track-head-model]
                (create-cube-mesh [0 -10000 0] [1 0 0 0] 0.2 :white))

      (assoc-in [:cage] (create-wireframe-cube [0 0 0] [1 0 0 0]
                                               [1 1 1] :white))

      (reset-avatar)
      
      (place-elements)
      (create-weld-groups)
      (create-update-cube)

      (start-replay "decider")
      ))
(reset-world!)
)

(defn update-world [world elapsed]
  (let [world (run-animation world elapsed)]
    (if (in? (:mode world) [:simulation :graph :motherboard
                            :property :avatar])
      (let [world (-> world
                      (set-probe-values)
                      (apply-forces elapsed)
                      (run-chips elapsed)
                      (enforce-gears)
                      (compute-transforms (if (:use-weld-groups world)
                                            :weld-groups
                                            :parts))
                      (update-motherboards)
                      )]
        (recompute-body-transforms! world)
        (step-simulation! (:planet world) elapsed)
        (if (= (:mode world) :avatar)
          (avatar-mode-update world elapsed)
          world))
      world)))

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))
  
  (if (> (get-in world [:camera :x-angle]) 0)
    (draw-mesh! world (:other-ground world)))

  (draw-spheres! world)

  (if-let [fun (get-function (:mode world) :draw-3d)]
    (fun world))
  
  (if (:use-weld-groups world)
    (do
      (doseq [group (vals (:weld-groups world))]
        (draw-mesh! world group))
      (draw-textured-parts! world))
    (doseq [[name part] (:parts world)]
      (if (or (= name :ground-part)
              (not (in? (:layer part) (:visible-layers world))))
        nil
        (draw-part! world part))))

  (draw-track-head! world)
  (draw-selection! world)
  (draw-buttons! world)
  (draw-lamps! world)
  (draw-displays! world)

  (if (:draw-update-cube world)
    (draw-update-cube! world))
  )

(defn show-buttons? [world]
  (or
   (= (:show-buttons world) :always)
   (and
    (= (:show-buttons world) :no-sim)
    (not (= (:mode world) :simulation)))))

(do
1

(defn draw-2d! [world]
  (clear!)

  (when (show-buttons? world)
    (let [{:keys [image x y w h]} (:action-menu world)]
      (fill-rect! (make-color 70 70 70) x y (+ 20 w) (+ 20 h))
      (draw-image! image x y))

    (let [{:keys [image x y w h]} (:mode-menu world)]
      (fill-rect! (make-color 70 70 70) x y (+ 20 w) (+ 20 h))
      (draw-image! image x y))

    (if-let [fun (get-function (:mode world) :draw)]
      (fun world)))

  (draw-buffer! world)
  (draw-hint! world)
  (draw-input-indicator! world)

  (replay-draw world)
  )
(redraw!))

(defn mouse-scrolled [world event]
  (let [world (-> world
                  (input-indicator-mouse-scrolled event)
                  (redraw))
        world (replay-zoomed world event)]
    (cond
      (and (= (:mode world) :graph)
           (inside-box? (:graph-box world) (:x event) (:y event)))
      (graph-mode-scrolled world event)

      :else
      (let [amount (+ 1 (* (:amount event) -0.05))]
        (zoom-camera world amount)))))

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
                  (assoc-in [:press-point] [x y])
                  (input-indicator-mouse-pressed event)
                  (redraw))
        world (replay-pressed world event)]
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
  (let [world (replay-moved world event)]
    (cond
      (not-nil? (:last-point world))
      (cond
        (= (:button event) :right) (mouse-rotate world event)
        (= (:button event) :middle) (mouse-pan world event)
        :else world)
      :else
      (mode-mouse-moved world event))))

(defn mouse-released [world event]
  (let [elapsed (- (get-current-time) (:press-time world))
        world (if (and (< elapsed 200)
                       (= (:button event) :right)
                       (< (distance (:press-point world)
                                    [(:x event) (:y event)]) 10))
                (assoc-in world [:animation]
                          (create-pivot-animation world event))
                world)
        world (-> world
                  (dissoc-in [:press-point])
                  (input-indicator-mouse-released event)
                  (redraw))

        world (if (= (:mode world) :avatar)
                (mode-mouse-released world event)
                world)
        world (replay-released world event)]
    (if (not-nil? (:last-point world))
      (dissoc-in world [:last-point])
      (mode-mouse-released world event))))

(defn window-changed [world event]
  (let [{:keys [width height]} event]
    (-> world
        (recompute-viewport width height)
        (place-elements))))

(defn window-focused [world focused]
  (if focused
    (-> world
      (update-scripts)
      (redraw))
    world))

;;----------------------------------------------------------------------;;
