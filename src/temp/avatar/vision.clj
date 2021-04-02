
(ns temp.core (:gen-class))

(defn vision-press-part [world]
  (let [avatar (:avatar world)
        position (vector-add (:position avatar) [0 0.1 0])
        y-angle (:angle avatar)
        direction (vector-rotate [0 0 1] [0 1 0] y-angle)
        x-axis (vector-rotate [1 0 0] [0 1 0] y-angle)
        direction (vector-rotate direction x-axis (:head-angle avatar))
        spec {:x 100000
              :y 100000
              :line [position direction]}]
    (if-let [{:keys [part-name distance]}
             (get-part-collision (compute-transforms world :parts) spec)]
      (let [part (get-in world [:parts part-name])]
        (if (in? (:type part) [:button :block :cylinder :cone :sphere])
          (-> world
              (assoc-in [:parts part-name :value] 1)
              (assoc-in [:pressed-part] part-name))
          world))
      world)))

(defn vision-handle-keys [world]
  (cond
    (avatar-key-pressed? world "q")
    (change-state world :running)
    
    (avatar-key-on? world "d")
    (update-in world [:avatar :angle] #(- % 0.5))

    (avatar-key-on? world "a")
    (update-in world [:avatar :angle] #(+ % 0.5))

    (avatar-key-on? world "w")
    (update-in world [:avatar :head-angle] #(within (- % 0.5) -30 30))

    (avatar-key-on? world "s")
    (update-in world [:avatar :head-angle] #(within (+ % 0.5) -30 30))

    (avatar-key-pressed? world "k")
    (-> world
        (vision-press-part)
        (assoc-in [:avatar :part-pressed] true))

    (and
     (avatar-key-released? world "k")
     (get-in world [:avatar :part-pressed]))
    (-> world
          (avatar-release-part)
          (assoc-in [:avatar :part-pressed] false))

    (avatar-key-pressed? world "j")
    (do
      (println! "vision jump" (rand))
      world)

    :else world))

(defn update-vision-state [world]
  (-> world
      ;; (move-avatar)
      (vision-handle-keys)))
