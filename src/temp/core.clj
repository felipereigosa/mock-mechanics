
(ns temp.core)

(load "util")
(load "world")
(load "vector")
(load "matrix")
(load "analytic-geometry")
(load "camera")
(load "transforms")
(load "window")
(load "xml")
(load "svg")
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
      (assoc-in [:cpu-box] {:x 343 :y 530
                            :w 685 :h 150
                            :buffer (new-image 685 150)})
      (assoc-in [:set-value-box]
                (create-picture "resources/set-value-menu.svg" 240 340 -1 60))
      (assoc-in [:layer-box] {:x 343 :y 575 :w 480 :h 60})
      (assoc-in [:toggle-box] {:x 343 :y 575 :w 500 :h 60})
      (assoc-in [:visible-layers] [1])
      (assoc-in [:command] "")
      (assoc-in [:bindings] (get-bindings))
      (assoc-in [:current-color] :red)
      
      (assoc-in [:action-menu]
                (create-picture "resources/action-menu.svg" 240 340 40 -1))
      (assoc-in [:mode-menu]
                (create-picture "resources/mode-menu.svg" 240 340 40 -1))
      
      (assoc-in [:color-palette]
                (create-picture "resources/colors.svg" 340 585 -1 40))
      (assoc-in [:add-menu]
                (create-picture "resources/add-menu.svg" 726 675 -1 50))
      (assoc-in [:add-type] :block)

      (assoc-in [:edit-menu]
                (create-picture "resources/edit-menu.svg" 210 575 -1 50))
      (assoc-in [:edit-subcommand] :move)

      (assoc-in [:use-weld-groups] true)
      (assoc-in [:graph-snap-value] 0.05)

      (assoc-in [:graph-menu]
                (create-picture "resources/graph-menu.svg" 210 575 -1 30))

      (assoc-in [:cpu-menu]
                (create-picture "resources/cpu-menu.svg" 210 575 -1 30))
      (assoc-in [:selected-property] 0)
      (assoc-in [:properties] [:free :physics :collision :.])

      (create-physics-world)
      (reset-undo! [:parts])
      (prepare-tree)
      (assoc-in [:mode] :simulation)

      (place-elements)
      ))
(reset-world!)
)

(defn update-world [world elapsed]
  (cond
    (in? (:mode world) [:simulation :graph :cpu])
    (let [world (-> world
                    ;; (set-probe-values)
                    (save-values)
                    (run-chips elapsed)
                    (apply-force elapsed)
                    (compute-transforms (if (:use-weld-groups world)
                                          :weld-groups
                                          :parts))
                    ;; (reverse-collisions)
                    (update-cpus)
                    )]
      (recompute-body-transforms! world)
      (step-simulation! (:planet world) elapsed)
      world)

    (= (:mode world) :set-value)
    (-> world
        (apply-force elapsed)
        (compute-transforms (if (:use-weld-groups world)
                              :weld-groups
                              :parts)))
    :else world))

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))

  (doseq [mesh (vals (:meshes world))]
    (draw-mesh! world mesh))

  (if (> (get-in world [:camera :x-angle]) 0)
    (draw-mesh! world (:other-ground world)))

  (draw-spheres! world)
  
  (if (:use-weld-groups world)
    (doseq [group (vals (:weld-groups world))]
      (draw-mesh! world group))
    ;; (doseq [[name part] (:parts world)]
    ;;   (if (or (= name :ground-part)
    ;;           (not (in? (:layer part) (:visible-layers world))))
    ;;     nil
    ;;     (draw-part! world part)))
    )

  (if-let [edited-part (:edited-part world)]
    (let [part (get-in world [:parts edited-part])
          part (if (in? (:type part) [:button :lamp])
                 (assoc-in part [:color] :black)
                 part)]
      (draw-part! world part)))

  (draw-selection! world)

  (draw-buttons! world)
  (draw-lamps! world)

  (draw-debug-meshes!)
  )

(do
1

(defn draw-2d! [world]
  (clear!)

  (let [{:keys [image x y w h]} (:action-menu world)]
    (fill-rect! (make-color 70 70 70) x y (+ 20 w) (+ 20 h))
    (draw-image! image x y))

  (let [{:keys [image x y w h]} (:mode-menu world)]
    (fill-rect! (make-color 70 70 70) x y (+ 20 w) (+ 20 h))
    (draw-image! image x y))
  
  (if-let [fun (get-function (:mode world) :draw)]
    (fun world))

  (draw-buffer! world)

  (if-let [hint (:hint world)]
    (draw-hint world hint))

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
                  :new (new-file world)
                  :view (view-all-parts world)
                  :save (save-version world)
                  :load (read-input world load-last-version-callback)
                  :undo (undo! world)
                  :redo (redo! world))]
      (show-hint world :action region))
    world))

(defn mode-menu-pressed [world x y]
  (if-let [region (get-region-at (:mode-menu world) x y)]
    (-> world
        (change-mode region)
        (prepare-tree)
        (show-hint :mode region))
    world))

(defn mouse-pressed [world event]
  (let [x (:x event)
        y (:y event)
        world (-> world
                  (assoc-in [:press-time] (get-current-time))
                  (assoc-in [:press-point] [x y]))]
    (cond
      (inside-box? (:action-menu world) x y)
      (action-menu-pressed world x y)

      (inside-box? (:mode-menu world) x y)
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
