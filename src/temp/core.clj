
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

(defn my-draw-textured-mesh! [world mesh transform]
  ;; (let [num-vertices (/ (.capacity (:vertices-buffer mesh)) 3)
  ;;       program (get-in world [:programs (:program mesh)])
  ;;       program-index (:index program)
  ;;       attributes (:attributes program)
  ;;       uniforms (:uniforms program)
  ;;       model-matrix (multiply-matrices
  ;;                     (apply get-scale-matrix (:scale mesh))
  ;;                     (get-transform-matrix transform))
  ;;       view-matrix (:view-matrix world)
  ;;       projection-matrix (:projection-matrix world)
  ;;       mv-matrix (multiply-matrices model-matrix view-matrix)
  ;;       mvp-matrix (multiply-matrices mv-matrix projection-matrix)
  ;;       itmv-matrix (get-transpose-matrix (get-inverse-matrix mv-matrix))]
  ;;   (GL20/glUseProgram program-index)
  ;;   (GL20/glUniformMatrix4fv (:itmv-matrix uniforms) false
  ;;                            (get-float-buffer itmv-matrix))

  ;;   (GL20/glUniformMatrix4fv (:mvp-matrix uniforms) false
  ;;                            (get-float-buffer mvp-matrix))

  ;;   (GL20/glVertexAttribPointer (:position attributes) 3 GL11/GL_FLOAT
  ;;                               false 0 (:vertices-buffer mesh))
  ;;   (GL20/glEnableVertexAttribArray (:position attributes))

  ;;   (GL20/glVertexAttribPointer (:normal attributes) 3 GL11/GL_FLOAT
  ;;                               false 0 (:normals-buffer mesh))
  ;;   (GL20/glEnableVertexAttribArray (:normal attributes))

  ;;   (GL20/glVertexAttribPointer
  ;;    (:texture-coordinates attributes) 2 GL11/GL_FLOAT
  ;;    false 0 (:texture-coordinates-buffer mesh))
  ;;   (GL20/glEnableVertexAttribArray (:texture-coordinates attributes))
  ;;   (GL13/glActiveTexture GL13/GL_TEXTURE0)
  ;;   (GL11/glBindTexture GL11/GL_TEXTURE_2D (:texture-id mesh))
  ;;   (GL20/glUniform1i (:texture-diffuse uniforms) 0)

  ;;   (GL11/glDrawArrays GL11/GL_TRIANGLES 0 num-vertices)
  ;;   )
    (let [num-vertices (/ (.capacity (:vertices-buffer mesh)) 3)
        program (get-in world [:programs (:program mesh)])
        program-index (:index program)
        attributes (:attributes program)
        uniforms (:uniforms program)
        model-matrix (multiply-matrices
                      (apply get-scale-matrix (:scale mesh))
                      (get-transform-matrix transform))
        view-matrix (:view-matrix world)
        projection-matrix (:projection-matrix world)
        mv-matrix (multiply-matrices model-matrix view-matrix)
        mvp-matrix (multiply-matrices mv-matrix projection-matrix)
        itmv-matrix (get-transpose-matrix (get-inverse-matrix mv-matrix))]

    (GL20/glUseProgram program-index)
    (GL20/glUniformMatrix4fv (:itmv-matrix uniforms) false
                             (get-float-buffer itmv-matrix))

    (GL20/glUniformMatrix4fv (:mvp-matrix uniforms) false
                             (get-float-buffer mvp-matrix))

    (GL20/glVertexAttribPointer (:position attributes) 3 GL11/GL_FLOAT
                                false 0 (:vertices-buffer mesh))

    (GL20/glEnableVertexAttribArray (:position attributes))

    (GL20/glVertexAttribPointer (:normal attributes) 3 GL11/GL_FLOAT
                                false 0 (:normals-buffer mesh))
    (GL20/glEnableVertexAttribArray (:normal attributes))

    (if-let [[r g b a] (:color mesh)]
      (GL20/glUniform4f (:material-color uniforms) r g b a)
      (do
        (GL20/glVertexAttribPointer (:texture-coordinates attributes) 2 GL11/GL_FLOAT
                                    false 0 (:texture-coordinates-buffer mesh))
        (GL20/glEnableVertexAttribArray (:texture-coordinates attributes))
        (GL13/glActiveTexture GL13/GL_TEXTURE0)
        (GL11/glBindTexture GL11/GL_TEXTURE_2D (:texture-id mesh))
        (GL20/glUniform1i (:texture-diffuse uniforms) 0)))

    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 num-vertices))
  )

(defn my-create-mesh [vertices position rotation
                   scale skin tex-coords normals]
  (let [scale (if (vector? scale)
                scale
                (vec (repeat 3 scale)))
        vertices (vec (flatten vertices))
        normals (if (empty? normals)
                  (vec (compute-normals vertices))
                  (vec (flatten normals)))
        base-mesh {:vertices vertices
                   :vertices-buffer (get-float-buffer vertices)
                   :normals normals
                   :normals-buffer (get-float-buffer normals)
                   :transform (make-transform position rotation)
                   :scale scale}]
    (cond
      (string? skin)
      (let [texture-id (GL11/glGenTextures)
            tex-coords (vec (flatten tex-coords))
            ]
        (-> base-mesh
            (assoc-in [:color] [1 0 0 1])
            (assoc-in [:draw-fn] my-draw-textured-mesh!)
            (assoc-in [:program] :textured)
            (assoc-in [:image] (open-image skin))
            (assoc-in [:texture-file] skin)
            (assoc-in [:texture-coordinates] tex-coords)
            (assoc-in [:texture-coordinates-buffer]
                      (get-float-buffer tex-coords))
            (assoc-in [:texture-id] texture-id)
            (set-texture)
            ))        

      (sequential? skin)
      (let [colors (vec (flatten skin))]
        (-> base-mesh
            (assoc-in [:colors] colors)
            (assoc-in [:colors-buffer] (get-float-buffer colors))
            (assoc-in [:draw-fn] draw-colored-mesh!)
            (assoc-in [:program] :colored)))

      :else
      (let [color (get-color skin)
            r (/ (get-red color) 255.0)
            g (/ (get-green color) 255.0)
            b (/ (get-blue color) 255.0)]
        (-> base-mesh
            (assoc-in [:color] [r g b 1.0])
            (assoc-in [:draw-fn] draw-lighted-mesh!)
            (assoc-in [:program] :flat))))))

(defn create-world []
  (-> (create-base-world)
      (assoc-in [:dial] ;;###################################
                ;; (create-model-mesh "res/dial.obj"
                ;;               [0 1 0] [1 0 0 0] 0.5 nil)

                (my-create-mesh [[0 0 0]
                                 [1 0 0]
                                 [0 1 0]]
                                [0 0 0]
                                [1 0 0 0]
                                1
                                "res/numbers.png"
                                [[0 0]
                                 [1 0]
                                 [0 1]]
                                [[0 0 1]
                                 [0 0 1]
                                 [0 0 1]]))
            
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

(defn update-world [world elapsed]
  (cond
    (in? (:mode world) [:simulation :graph :motherboard])
    (let [elapsed 16 ;;######################
          world (-> world
                    (set-probe-values)
                    (save-values)
                    (run-chips elapsed)
                    (apply-force elapsed)
                    (compute-transforms (if (:use-weld-groups world)
                                          :weld-groups
                                          :parts))
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

  (draw-mesh! world (:dial world))
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

  (if-let [track-head-name (:track-head world)]
    (let [transform (get-in world [:parts track-head-name :transform])
          mesh (:track-head-model world)
          mesh (assoc-in mesh [:transform] transform)]
      (draw-mesh! world mesh)))

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

