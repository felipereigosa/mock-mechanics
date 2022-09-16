(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

(declare get-track-plane)

(defn set-partner-values [world part-name]
  (let [world (assoc-in world [:parts part-name :already-set] true)
        value (get-in world [:parts part-name :value])
        partners (remove-nil
                   (map (fn [entry]
                          (cond
                            (= (:part-1-name entry) part-name) [(:part-2-name entry) (:ratio entry)]
                            (= (:part-2-name entry) part-name) [(:part-1-name entry) (/ 1.0 (:ratio entry))]
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

(defn enforce-gears [world & extra]
  (let [gear-part-names (distinct (flatten (map (fn [entry]
                                                  [(:part-1-name entry)
                                                   (:part-2-name entry)])
                                                (vals (:gears world)))))
        world (reduce (fn [w part-name]
                        (assoc-in w [:parts part-name :already-set] false))
                      world
                      gear-part-names)
        driven-parts (remove-nil
                       (concat [(:part-name (:mouse-force world))
                                (:part-name (:track-force world))]
                               extra
                               (vec (clojure.set/intersection
                                      (into #{} (:driven-parts world))
                                      (into #{} gear-part-names)))))
        world (assoc-in world [:driven-parts] [])]
    (reduce (fn [w driven-part]
              (set-partner-values w driven-part))
            world
            driven-parts)))

(defn create-gear-mesh [world radius position rotation
                        scale n angle-offset skin]
  (let [body (select-keys (get-in world [:info :gear :body])
                          [:vertices :normals])
        tooth (select-keys (get-in world [:info :gear :tooth])
                           [:vertices :normals])
        vertices (partition 3 (:vertices body))
        [sx sy sz] [(* radius 2) 0.2 (* radius 2)]
        body-vertices (map (fn [[x y z]]
                             [(* x sx) (* y sy) (* z sz)])
                           vertices)
        body-normals (:normals body)
        tooth-vertices (partition 3 (:vertices tooth))
        tooth-vertices (map (fn [[x y z]]
                              [(+ x radius) y z])
                            tooth-vertices)
        tooth-normals (partition 3 (:normals tooth))
        teeth-vertices
        (mapcat (fn [i]
                  (map (fn [[x y z]]
                         (vector/rotate [x y z] [0 1 0]
                                        (+ angle-offset (* i (/ 360 n)))))
                       tooth-vertices))
                (range n))
        teeth-normals
        (map (fn [i]
               (map (fn [[x y z]]
                      (vector/rotate [x y z] [0 1 0]
                                     (+ angle-offset (* i (/ 360 n)))))
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

(defn add-gear-models [world ratio gear-1-name r1 gear-2-name r2 angle-offset]
  (let [scale-1 (get-in world [:parts gear-1-name :scale])
        scale-2 (get-in world [:parts gear-2-name :scale])
        inverse-scale-1 (map #(/ 1.0 %) scale-1)
        inverse-scale-2 (map #(/ 1.0 %) scale-2)
        n (int (/ (* 2 pi (* 2 r2)) 0.5))
        n (if (odd? n) (dec n) n)
        tooth-height 0.09
        other-angle-offset (/ 180 n)
        model-1 (create-gear-mesh world (- r1 (* tooth-height 0.5))
                                  [0 0 0] [1 0 0 0] inverse-scale-1
                                  (int (* n ratio)) angle-offset :red)
        model-2 (create-gear-mesh world (- r2 (* tooth-height 0.5))
                                  [0 0 0] [1 0 0 0] inverse-scale-2
                                  n (+ angle-offset other-angle-offset) :red)]
    (-> world
        (assoc-in [:parts gear-1-name :model] model-1)
        (assoc-in [:parts gear-2-name :model] model-2))))

(defn add-two-gears [world parent-1-name parent-2-name ratio & extra]
  (let [parent-1 (get-in world [:parts parent-1-name])
        parent-2 (get-in world [:parts parent-2-name])
        layer (apply min (:visible-layers world))
        color-1 (new Color 10 10 10)
        gear-1 (create-part :gear color-1 layer (:info world))
        [gear-1-name gear-2-name] (if (empty? extra)
                                    [(gen-keyword :gear)
                                     (gen-keyword :gear)]
                                    extra)
        axis (vector/normalize (get-track-direction parent-1))
        offset (vector/multiply
                 axis
                 (+ 0.1 (* -1 (second (:scale parent-1)))))
        gear-1-transform (combine-transforms
                           (:transform parent-1)
                           (make-transform offset [1 0 0 0]))
        color-2 (new Color 128 128 128)
        gear-2 (create-part :gear color-2 layer (:info world))
        parent-2-plane (get-track-plane parent-2)
        gear-1-point (get-transform-position gear-1-transform)
        offset (vector/multiply
                 axis
                 (point-plane-distance gear-1-point parent-2-plane))
        gear-2-transform (combine-transforms
                           (:transform parent-2)
                           (make-transform offset [1 0 0 0]))
        gear-2-point (get-transform-position gear-2-transform)
        d (vector/distance gear-1-point gear-2-point)
        r1 (* (/ ratio (+ ratio 1)) d)
        r2 (* (/ 1 (+ ratio 1)) d)
        scale-1 [(* r1 2) 0.2 (* r1 2)]
        scale-2 [(* r2 2) 0.2 (* r2 2)]
        w (set-value-0-transform world parent-1-name)
        p1 (get-in w [:parts parent-1-name])
        track-1-rotation (get-rotation-component (:transform p1))
        track-1-x (apply-transform track-1-rotation [1 0 0])
        track-1-position (get-part-position world parent-1-name)
        track-2-position (get-part-position world parent-2-name)
        point (point-line-projection track-2-position [track-1-position axis])
        to-track-2 (vector/subtract track-2-position point)
        angle-offset (vector/angle track-1-x to-track-2 axis)]
    (-> world
        (assoc-in [:parts gear-1-name] gear-1)
        (assoc-in [:parts gear-1-name :scale] scale-1)
        (assoc-in [:parts gear-1-name :transform] gear-1-transform)
        (create-relative-transform gear-1-name parent-1-name)
        (assoc-in [:parts gear-2-name] gear-2)
        (assoc-in [:parts gear-2-name :scale] scale-2)
        (assoc-in [:parts gear-2-name :transform] gear-2-transform)
        (create-relative-transform gear-2-name parent-2-name)
        (add-gear-models ratio gear-1-name r1 gear-2-name r2 angle-offset)
        (assoc-in [:gears [gear-1-name gear-2-name]]
                  {:part-1-name parent-1-name
                   :radius-1 r1
                   :part-2-name parent-2-name
                   :radius-2 r2
                   :angle-offset angle-offset
                   :ratio ratio})
        (enforce-gears parent-1-name))))

(defn create-rack-mesh [world position rotation
                        scale length gap skin]
  (let [body (create-cube-mesh [0 0 0] [1 0 0 0] [1 1 1] :white)
        tooth (select-keys (get-in world [:info :gear :tooth])
                           [:vertices :normals])
        body-vertices (partition 3 (:vertices body))
        [sx sy sz] [0.1 (+ length 0.5) 0.2]
        body-vertices (map (fn [[x y z]]
                             [(* x sx) (* y sy) (* z sz)])
                           body-vertices)
        tooth-vertices (partition 3 (:vertices tooth))
        tooth-vertices (map (fn [[x y z]]
                              (vector/rotate [x y z] [1 0 0] 90))
                            tooth-vertices)
        n (round (/ (+ length 0.5) gap))
        offset (+ (* gap 0.5) (* length 0.5))
        teeth-vertices (mapcat (fn [i]
                                 (map (fn [[x y z]]
                                        [(+ x 0.06)
                                         (+ y (* i -1 gap) offset)
                                         z])
                                      tooth-vertices))
                               (range n))
        vertices (concat body-vertices teeth-vertices)
        [sx sy sz] scale
        vertices (map (fn [[x y z]]
                        [(* x sx) (* y sy) (* z sz)])
                      vertices)
        vertices (map (fn [[x y z]]
                        (vector/rotate [x y z] [0 1 0] 180))
                      vertices)
        vertices (flatten vertices)]
    (create-mesh vertices position rotation [1 1 1] skin nil nil)))

(defn add-gear-and-rack-models [world gear-name radius rack-name length angle-offset]
  (let [scale-1 (get-in world [:parts gear-name :scale])
        scale-2 (get-in world [:parts rack-name :scale])
        inverse-scale-1 (map #(/ 1.0 %) scale-1)
        inverse-scale-2 (map #(/ 1.0 %) scale-2)
        n (Math/floor (/ (* 2 pi (* 2 radius)) 0.5))
        model-1 (create-gear-mesh world radius [0 0 0] [1 0 0 0]
                                  inverse-scale-1 n angle-offset :red)
        gap (/ (* 2 pi radius) n)
        model-2 (create-rack-mesh world [0 0 0] [1 0 0 0]
                                  inverse-scale-2 length gap :red)]
    (-> world
        (assoc-in [:parts gear-name :model] model-1)
        (assoc-in [:parts rack-name :model] model-2))))

(defn add-gear-and-rack [world part-1-name part-2-name]
  (let [[track-name wagon-name] (if (= (get-in world [:parts part-1-name :type]) :track)
                                  [part-1-name part-2-name]
                                  [part-2-name part-1-name])
        track (get-in world [:parts track-name])
        wagon (get-in world [:parts wagon-name])
        track-position (get-part-position world track-name)
        wagon-position (get-part-position world wagon-name)
        layer (apply min (:visible-layers world))
        color-1 (new Color 10 10 10)
        gear (create-part :gear color-1 layer (:info world))
        gear-name (gen-keyword :gear)
        track-plane (get-track-plane track)
        axis (vector/normalize (get-track-direction track))
        offset (vector/multiply
                 axis
                 (point-plane-distance wagon-position track-plane))
        gear-transform (combine-transforms
                         (:transform track)
                         (make-transform offset [1 0 0 0]))
        wagon-parent-name (get-parent-part world wagon-name)
        wagon-parent (get-in world [:parts wagon-parent-name])
        wagon-parent-direction (get-track-direction wagon-parent)
        gear-position (get-transform-position gear-transform)
        point (point-line-projection gear-position [wagon-position wagon-parent-direction])
        to-wagon (vector/normalize (vector/subtract point gear-position))
        d (point-plane-distance
            wagon-position
            (line->plane [track-position to-wagon]))
        color-2 (new Color 128 128 128)
        rack (create-part :rack color-2 layer (:info world))
        rack-name (gen-keyword :rack)
        gear-position (get-transform-position gear-transform)
        h-distance (point-line-distance
                     (get-part-position world wagon-parent-name)
                     [gear-position to-wagon])
        wagon-parent-length (second (:scale wagon-parent))
        parallel-offset (- (/ wagon-parent-length 2) h-distance)
        parallel-offset (vector/multiply
                          (vector/normalize wagon-parent-direction) parallel-offset)
        perpendicular-offset (vector/multiply to-wagon -0.125)
        wagon-rotation (get-rotation-component (:transform wagon))
        rack-angle (vector/angle (apply-transform wagon-rotation [-1 0 0])
                                 (vector/multiply to-wagon -1) wagon-parent-direction)
        rack-transform (combine-transforms
                         (make-transform [0 0 0] [0 1 0 rack-angle])
                         (combine-transforms
                           (:transform wagon)
                           (make-transform (vector/add parallel-offset perpendicular-offset)
                                           [1 0 0 0])))
        tooth-height 0.09
        radius (- d 0.175 tooth-height)
        scale-1 [(* radius 2) 0.2 (* radius 2)]
        rack-length (+ wagon-parent-length 0.0)
        scale-2 [0.1 rack-length 0.2]
        w (set-value-0-transform world track-name)
        p1 (get-in w [:parts track-name])
        track-1-rotation (get-rotation-component (:transform p1))
        track-1-x (apply-transform track-1-rotation [1 0 0])
        angle-offset (- (vector/angle track-1-x to-wagon axis))
        sign (- (vector/dot-product to-wagon
                                    (vector/cross-product
                                      wagon-parent-direction
                                      (get-track-direction track))))]
    (-> world
        (assoc-in [:parts gear-name] gear)
        (assoc-in [:parts gear-name :scale] scale-1)
        (assoc-in [:parts gear-name :transform] gear-transform)
        (create-relative-transform gear-name track-name)
        (assoc-in [:parts rack-name] rack)
        (assoc-in [:parts rack-name :scale] scale-2)
        (assoc-in [:parts rack-name :transform] rack-transform)
        (create-relative-transform rack-name wagon-name)
        (add-gear-and-rack-models gear-name radius rack-name rack-length angle-offset)
        (assoc-in [:gears [gear-name rack-name]]
                  {:part-1-name track-name
                   :radius radius
                   :part-2-name wagon-name
                   :length rack-length
                   :angle-offset angle-offset
                   :ratio (/ (* 2 pi radius) (* sign wagon-parent-length))})
        (enforce-gears track-name))))
