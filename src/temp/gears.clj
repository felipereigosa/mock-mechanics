
(ns temp.core (:gen-class))

(defn set-partner-values [world part-name]
  (let [world (assoc-in world [:parts part-name :already-set] true)
        value (get-in world [:parts part-name :value])
        partners (remove-nil
                  (map (fn [entry]
                         (cond
                           (= (:part-1-name entry) part-name) [(:part-2-name entry) (/ 1.0 (:ratio entry))]
                           (= (:part-2-name entry) part-name) [(:part-1-name entry) (:ratio entry)]
                           :else nil))
                       (vals (:gears world))))]
    (reduce (fn [w [partner-name ratio]]
              (if (not (get-in w [:parts partner-name :already-set]))
                (-> w
                    (assoc-in [:parts partner-name :value] (* -1 ratio value))
                    (set-partner-values partner-name))
                w))
            world
            partners)))

(defn get-driven-parts [world]
  (let [chip-names (get-parts-with-type (:parts world) :chip)]
    (mapcat (fn [chip-name]
              (let [chip (get-in world [:parts chip-name])]
                (if (< (:time chip) (:final-time chip))
                  (keys (:functions chip))
                  nil)))
            chip-names)))    

(defn enforce-gears [world]
  (let [gear-part-names (distinct (flatten (map (fn [entry]
                                                  [(:part-1-name entry)
                                                   (:part-2-name entry)])
                                                (vals (:gears world)))))
        world (reduce (fn [w part-name]
                        (assoc-in w [:parts part-name :already-set] false))
                      world
                      gear-part-names)
        driven-parts (concat [(:part-name (:mouse-force world))]
                             (into [] (clojure.set/intersection
                                       (into #{} (get-driven-parts world))
                                       (into #{} gear-part-names))))]
    (reduce (fn [w driven-part]
              (set-partner-values w driven-part))
            world
            driven-parts)))

;; (do
;; 1

(defn create-gear-mesh [world radius position rotation
                        scale n angle-offset skin]
  (let [body (select-keys (get-in world [:info :gear :body])
                               [:vertices :normals])
        tooth (select-keys (get-in world [:info :gear :tooth])
                           [:vertices :normals])
        vertices (partition 3 (:vertices body))
        radius (- radius 0.08)
        [sx sy sz] [radius 0.2 radius]
        body-vertices (map (fn [[x y z]]
                        [(* x sx) (* y sy) (* z sz)])
                           vertices)
        body-normals (:normals body)
        tooth-vertices (partition 3 (:vertices tooth))
        tooth-vertices (map (fn [[x y z]]
                              [(+ x (* radius 0.5)) y z])
                            tooth-vertices)
        tooth-normals (partition 3 (:normals tooth))
        teeth-vertices
        (mapcat (fn [i]
               (map (fn [[x y z]]
                      (vector-rotate [x y z] [0 1 0] (+ angle-offset (* i (/ 360 n)))))
                    tooth-vertices))
             (range n))
        teeth-normals
        (map (fn [i]
               (map (fn [[x y z]]
                      (vector-rotate [x y z] [0 1 0] (+ angle-offset (* i (/ 360 n)))))
                    tooth-normals))
             (range n))
        vertices (concat body-vertices teeth-vertices)
        [sx sy sz] scale
        vertices (map (fn [[x y z]]
                        [(* x sx) (* y sy) (* z sz)])
                      vertices)        
        vertices (flatten vertices)
        normals (flatten (concat body-normals teeth-normals))]
    (create-mesh vertices position rotation [1 1 1] skin nil normals)))
  
;; (clear-output!)
;; (let [world @world
;;       ]

;;   (set-thing! [:background-meshes :gear]
;;               (create-gear-mesh world 1 [0 -100.1 0] [1 0 0 0]
;;                                 [1 1 1]
;;                                 12
;;                                 15
;;                                 :gray))
;;   ))

;; (do
;; 1
;; (update-thing! [:use-weld-groups] not)
;; (redraw!))

;; (set-thing! [:gears] nil)
;; (println! (:gears @world))
