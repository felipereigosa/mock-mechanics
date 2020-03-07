(ns temp.core)

(load "world")
(load "util")
(load "vector")
(load "matrix")
(load "analytic-geometry")
(load "xml")
(load "svg")
(load "transforms")
(load "keymap")
(load "window")
(load "debug")
(load "meshes")

(load "miscellaneous")
(load "parts")
(load "collision")
(load "physics")
(load "weld-optimization")
(load "mechanical-tree")
(load "undo")
(load "persistence")
(load "value-force")
(load "modes")
(load "commands")
(load "track-loop")

(do
1

(defn create-world []
  (let [world (create-gl-world)
        r 0.2]
    (create-debug-meshes!)
    (-> world
        (assoc-in [:background-meshes :grid] (create-grid-mesh 24 0.5))
        (assoc-in [:info] (create-info))
        (create-ground-part)
        (assoc-in [:walls]
                  (create-cube-mesh [0 -0.1 0] [1 0 0 0] [12 0.2 12]
                                    (make-color 40 40 40)))
        (assoc-in [:graph-box] {:x 343 :y 530
                                :w 685 :h 150
                                :buffer (new-image 685 150)
                                })
        (assoc-in [:cpu-box] {:x 343 :y 530 :w 685 :h 150})
        (assoc-in [:layer-box] {:x 343 :y 575 :w 480 :h 60})
        (assoc-in [:visible-layers] [1])
        (update-move-plane)
        (assoc-in [:command] "")
        (assoc-in [:mode] :idle)
        (assoc-in [:bindings] (get-bindings))
        (assoc-in [:current-color] :red)
        (assoc-in [:color-palette]
                  (create-image "resources/colors.svg" 340 585 -1 40))
        (assoc-in [:insert-menu]
                  (create-image "resources/insert-menu.svg" 326 575 -1 50))
        (assoc-in [:insert-type] :block)

        (assoc-in [:edit-menu]
                  (create-image "resources/edit-menu.svg" 210 575 -1 50))
        (assoc-in [:edit-subcommand] :move)

        (assoc-in [:action-menu]
                  (create-image "resources/action-menu.svg" 140 25 -1 40))

        (assoc-in [:mode-menu]
                  (create-image "resources/mode-menu.svg" 480 25 -1 40))

        (assoc-in [:selected-mesh]
                  (create-wireframe-cube [0 0.52 0] [1 0 0 0]
                                         [0.3001 0.1001 0.3001] :white))
        (assoc-in [:use-weld-groups] true)

        (assoc-in [:planet] (create-planet))
        (update-in [:planet] create-ground)
        (assoc-in [:sphere-radius] r)
        (assoc-in [:sphere-mesh] (create-sphere-mesh [0 0 0] [1 0 0 0]
                                                     [r r r] :blue))
        (assoc-in [:spheres] [])
        (assoc-in [:button-mesh] (create-cylinder-mesh [0 0 0] [1 0 0 0]
                                                       [0.2 0.2 0.2] :red))

        (assoc-in [:lamp-mesh] (create-model-mesh "resources/lamp.obj"
                                                  [0 0 0] [1 0 0 0]
                                                  [0.3 0.3 0.3] :red))

        (assoc-in [:graph-snap-value] 0.1)

        (assoc-in [:selected-property] 0)
        (assoc-in [:properties] [:free :hidden :physics])

        (reset-undo! [:ground-children :planet :spheres :parts])
        (prepare-tree)
    )))
(reset-world!)
)

(defn update-world [world elapsed]
  (if (in? (:mode world) [:insert :edit])
    world
    (let [world (-> world
                    (set-probe-values)
                    (run-chips elapsed)
                    (apply-force elapsed)
                    (compute-transforms (if (:use-weld-groups world)
                                          :weld-groups
                                          :parts))
                    (update-cpus)
                    )]
      ;; (recompute-body-transforms! world)
      ;; (step-simulation! (:planet world) elapsed)
      world)))

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))

  (doseq [mesh (vals (:meshes world))]
    (draw-mesh! world mesh))

  (if (> (get-in world [:camera :x-angle]) 0)
    (draw-mesh! world (:walls world)))

  (if (:use-weld-groups world)
    (doseq [group (vals (:weld-groups world))]
      (draw-mesh! world group))
    (doseq [part (vals (:parts world))]
      (draw-part! world part))
    )

  (if-let [edited-part (:edited-part world)]
    (let [part (get-in world [:parts edited-part])
          part (if (in? (:type part) [:button :lamp])
                 (assoc-in part [:color] :black)
                 part)
          part (assoc-in part [:color] :yellow)
          ]
      (draw-part! world part)))

  (draw-buttons! world)
  (draw-lamps! world)
  ;; (draw-spheres! world)

  (if (or (and
           (= (:mode world) :graph)
           (:selected-chip world))
          (and
           (= (:mode world) :cpu)
           (:selected-cpu world)))
    (draw-mesh! world (:selected-mesh world)))

  (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
  (draw-debug-meshes!)
  )

(defn show-hint [world menu action]
  (let [texts {:action {}
               :mode {:insert "Ctrl + a,Add"
                      :edit "Ctrl + e,Edit"
                      }
               :insert {}
               :edit {}
               :cpu {}
               :chip {}
               }
        world (assoc-in world [:hint]
                        {:text (get-in texts [menu action])
                         :time (get-current-time)})]
    (.start
     (new Thread
          (proxy [Runnable] []
            (run []
              (try
                (sleep 2500)
                (redraw!)
                (catch Exception e))))))
    (redraw!)
    world))

(do
1

(defn draw-hint [hint]
  (let [elapsed (- (get-current-time) (:time hint))
        [command description] (split (:text hint) #",")]
    (if (< elapsed 2000)
      (let [x (/ window-width 2)
            y (- (/ window-height 2) 50)]
        (fill-rect! :black x y 300 150)
        (draw-text! :white command (+ (- x 150) 40) y 20)
        (draw-text! :red description (+ x 30) y 20)))))

(defn draw-2d! [world]
  (clear!)

  (let [{:keys [image x y]} (:action-menu world)]
    (fill-rect! (make-color 70 70 70) 330 y 800 50)
    (draw-image! image x y))

  (let [{:keys [image x y]} (:mode-menu world)]
    (draw-image! image x y))
  
  (if-let [fun (get-function (:mode world) :draw)]
    (fun world))
  (draw-output!)
  (draw-buffer! world)

  (if-let [hint (:hint world)]
    (draw-hint hint))
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
    (case region
      :new (new-file world)
      :view (reset-camera world)
      :save (save-version world)
      :load (read-input world load-last-version-callback)
      :undo (undo! world)
      :redo (redo! world))
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
      (inside-box? (:action-menu world) x y)
      (action-menu-pressed world x y)

      (inside-box? (:mode-menu world) x y)
      (mode-menu-pressed world x y)
      
      (in? (:button event) [:middle :right])
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
                (do
                  (println! "set pivot" elapsed (:press-point world)
                            [(:x event) (:y event)])
                  (set-pivot world event))
                world)
        world (cond
                (not-nil? (:last-point world))
                (-> world
                    (dissoc-in [:last-point])
                    (update-move-plane))
                :else
                (mode-mouse-released world event))]
    (redraw!)
    (-> world
        (prepare-tree)
        (save-checkpoint!))))

