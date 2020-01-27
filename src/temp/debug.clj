
(ns temp.core)

(declare get-part-at)
(declare create-line-mesh)

(defn debug-mode-draw [world]
  (draw-rect! :red 340 300 670 550))

(do
1  

(defn debug-mode-pressed [world event]
  (let [{:keys [x y]} event]
    (println! (get-part-at world x y) x y)
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
