
(ns temp.core (:gen-class))

(declare get-part-at)
(declare create-line-mesh)

(defn debug-mode-draw [world]
  (let [w (:window-width world)
        h (:window-height world)
        hw (* w 0.5)
        hh (* h 0.5)]
    (draw-rect! :red hw hh (- w 100) (- h 100))))

(do
1  

(defn debug-mode-pressed [world event]
  (let [{:keys [x y]} event
        ;; collision (get-part-collision world event)
        part-name (get-part-at world event)
        part (get-in world [:parts part-name])
        ]        
    (println! x y)
    (println! part-name)
    (print-transform (:transform part))
    world))

(defn debug-mode-moved [world event]
  world)

(defn debug-mode-released [world event]
  world)
)

(def debug-meshes (atom nil))

(defn create-debug-meshes! []
  (reset! debug-meshes
          {:points {:red (create-cube-mesh
                          [-100 0 0] [1 0 0 0]
                          [0.05 0.05 0.05] :red)
                    :green (create-cube-mesh
                             [-100 0 0] [1 0 0 0]
                             [0.05 0.05 0.05] :green)
                    :blue (create-cube-mesh
                             [-100 0 0] [1 0 0 0]
                             [0.05 0.05 0.05] :blue)
                    :yellow (create-cube-mesh
                             [-100 0 0] [1 0 0 0]
                             [0.05 0.05 0.05] :yellow)
                    }
           :lines {:red (create-line-mesh [-1000 0 0] [-1000 1 0] :red)
                   :green (create-line-mesh [-1000 0 0] [-1000 1 0] :green)
                   :blue (create-line-mesh [-1000 0 0] [-1000 1 0] :blue)
                   :yellow (create-line-mesh [-1000 0 0] [-1000 1 0] :yellow)
                   }
           }))

(defn draw-debug-meshes! []
  (let [points (:points @debug-meshes)]
    (doseq [mesh (vals points)]
      (draw-mesh! @world mesh)))

  (let [points (:lines @debug-meshes)]
    (doseq [mesh (vals points)]
      (draw-mesh! @world mesh))))
  
(defn set-point! [color position]
  (swap! debug-meshes
         (fn [dm]
           (update-in dm [:points color]
                      #(set-mesh-position % position))))
  nil)

(defn set-line! [color p1 p2]
  (swap! debug-meshes
         (fn [dm]
           (assoc-in dm [:lines color]
                     (create-line-mesh p1 p2 color))))
  nil)

(defn hide-debug-meshes! []
  (set-point! :red [-1000 0 0])
  (set-point! :green [-1000 0 0])
  (set-point! :blue [-1000 0 0])
  (set-point! :yellow [-1000 0 0])

  (set-line! :red [-1000 0 0] [-1000 1 0])
  (set-line! :green [-1000 0 0] [-1000 1 0])
  (set-line! :blue [-1000 0 0] [-1000 1 0])
  (set-line! :yellow [-1000 0 0] [-1000 1 0]))

(defn add-layer [parts]
  (map-map (fn [[name value]]
             {name (assoc-in value [:layer] 1)})
           parts))

(defn get-num-vertices [world part-name]
  (let [part (get-in world [:parts part-name])
        part-type (:type part)
        ]
    (if (= (:layer part) 1)
      (/ (count (get-in world [:info part-type :model :vertices])) 3)
      0)))

;; (defn draw-update-cube! [world] ;;#########################
;;   (if-let [mesh (:update-cube world)]
;;     (let [green-value (if (float= (second (:color mesh)) 1.0)
;;                         0.0
;;                         1.0)]
;;       (set-thing! [:update-cube :color 1] green-value)
;;       (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
;;       (draw-mesh! world mesh))))
