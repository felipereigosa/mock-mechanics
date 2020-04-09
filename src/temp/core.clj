
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
(load "output")

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
(load "hints")

(do
1

(defn create-world []
  (-> (create-base-world)
      (assoc-in [:background-meshes :grid] (create-grid-mesh 24 0.5))
      (assoc-in [:info] (create-info))
      (assoc-in [:parts] {:ground-part
                          {:type :ground
                           :transform (make-transform [0 -0.1 0] [1 0 0 0])
                           :color :dark-gray
                           :scale [12 0.2 12]
                           :children {}}})
      (assoc-in [:other-ground]
                (create-cube-mesh [0 -0.1 0] [1 0 0 0] [12 0.2 12]
                                  (make-color 40 40 40)))
      (assoc-in [:graph-box] {:x 343 :y 530
                              :w 685 :h 150
                              :buffer (new-image 685 150)
                              })
      (assoc-in [:cpu-box] {:x 343 :y 530 :w 685 :h 150})
      (assoc-in [:layer-box] {:x 343 :y 575 :w 480 :h 60})
      (assoc-in [:toggle-box] {:x 343 :y 575 :w 500 :h 60})
      (assoc-in [:visible-layers] [1])
      (update-move-plane)
      (assoc-in [:command] "")
      (assoc-in [:mode] :idle)
      (assoc-in [:bindings] (get-bindings))
      (assoc-in [:current-color] :red)
      
      (assoc-in [:action-menu]
                (create-picture "resources/action-menu.svg" 240 340 40 -1))
      (assoc-in [:mode-menu]
                (create-picture "resources/mode-menu.svg" 240 340 40 -1))
      
      (assoc-in [:color-palette]
                (create-picture "resources/colors.svg" 340 585 -1 40))
      (assoc-in [:insert-menu]
                (create-picture "resources/insert-menu.svg" 726 675 -1 50))
      (assoc-in [:insert-type] :block)

      (assoc-in [:edit-menu]
                (create-picture "resources/edit-menu.svg" 210 575 -1 50))
      (assoc-in [:edit-subcommand] :move)

      (assoc-in [:use-weld-groups] false) ;;######################
      (assoc-in [:graph-snap-value] 0.1)
      (assoc-in [:selected-property] 0)
      (assoc-in [:properties] [:free :physics :heat :size])

      (reset-undo! [:parts])
      (prepare-tree)
      (place-elements)
      ))
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
                    (update-cpus))]
      world)))

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))

  (doseq [mesh (vals (:meshes world))]
    (draw-mesh! world mesh))

  (if (> (get-in world [:camera :x-angle]) 0)
    (draw-mesh! world (:other-ground world)))

  (if (:use-weld-groups world)
    (doseq [group (vals (:weld-groups world))]
      (draw-mesh! world group))
    (doseq [[name part] (:parts world)]
      (if (or (= name :ground-part)
              (not (in? (:layer part) (:visible-layers world))))
        nil
        (draw-part! world part))))

  (if-let [edited-part (:edited-part world)]
    (let [part (get-in world [:parts edited-part])
          part (if (in? (:type part) [:button :lamp])
                 (assoc-in part [:color] :blarck)
                 part)
          ;; part (assoc-in part [:color] :yellow)
          ]
      (draw-part! world part)))

  (draw-buttons! world)
  (draw-lamps! world)

  (if (or (and
           (= (:mode world) :graph)
           (:selected-chip world))
          (and
           (= (:mode world) :cpu)
           (:selected-cpu world)))
    (draw-mesh! world (:selected-mesh world)))
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
      :view (create-camera world [0 0 1] 40 25 -35)
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

      (and
       (in? (:button event) [:middle :right])
       (not (inside-box? (:graph-box world) x y)))
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
                world)
        world (cond
                (not-nil? (:last-point world))
                (-> world
                    (dissoc-in [:last-point])
                    (update-move-plane))
                :else
                (mode-mouse-released world event))]
    (-> world
        (redraw)
        (prepare-tree)
        (save-checkpoint!))))

(defn window-changed [world event]
  (let [{:keys [width height]} event]
    (-> world
        (recompute-viewport width height)
        (place-elements))))
