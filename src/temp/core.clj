(ns temp.core)

(load "world")
(load "util")
(load "vector")
(load "matrix")
(load "analytic-geometry")
(load "xml")
(load "svg")
(load "physics")
(load "keymap")
(load "transforms")
(load "window")
(load "meshes")
(load "weld-optimization")

;;----------------------------------------------------------------------;;
;; miscellaneous

(defn create-info []
  {:ground {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [1 1 1] :white)
           :points []
           :scale [0.5 0.5 0.5]
           :direction nil
           }

   :block {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [1 1 1] :white)
           :points [[0.5 0 0] [-0.5 0 0]
                    [0 0.5 0] [0 -0.5 0]
                    [0 0 0.5] [0 0 -0.5]]
           :scale [0.5 0.5 0.5]
           :direction nil
           }

   :wagon {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [1 1 1] :white)
           :points [[0.5 0 0] [-0.5 0 0]
                    [0 0.5 0] [0 -0.5 0]
                    [0 0 0.5] [0 0 -0.5]]
           :scale [0.5 0.5 0.5]
           :direction nil
           }

   :probe {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [1 1 1] :white)
           :points []
           :scale [0.1 0.1 0.1]
           :direction :input
           }

   :track {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                    [1 1 1] :white)
           :points [[0.2 0 0] [-0.2 0 0]
                    [0 0.2 0]
                    [0 0 0.2] [0 0 -0.2]
                    ]
           :scale [0.1 1 0.1]
           :direction :output
           }

   :chip {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                   [1 1 1] :white)
          :points []
          :scale [0.3 0.1 0.3]
          :direction :output
          }

   :cpu {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                  [1 1 1] :white)
         :points []
         :scale [0.3 0.1 0.3]
         :direction :output
         }

   :button {:model (create-cube-mesh [0 0 0] [1 0 0 0]
                                     [1 1 1] :white)
            :points []
            :scale [0.5 0.2 0.5]
            :direction :input
            }
   })

(defn create-part [type color info]
  (let [part {:type type
              :color color
              :value 0
              :transform (make-transform [0 0.5 0] [0 1 0 0])
              :scale (get-in info [type :scale])}]
    (if (= type :chip)
      (-> part
          (assoc-in [:functions] {})
          (assoc-in [:time] 1.0)
          (assoc-in [:final-time] 0.0)
          (assoc-in [:view] {:offset [0 0]
                             :zoom 1}))
      part)))

(defn create-relative-transform [world child-name parent-name]
  (let [child (get-in world [:parts child-name])]
    (if (= parent-name :ground)
      (assoc-in world [:ground-children child-name] (:transform child))
      (let [parent (get-in world [:parts parent-name])
            child-transform (:transform child)
            parent-transform (:transform parent)
            final-transform (remove-transform child-transform parent-transform)]
        (assoc-in world [:parts parent-name :children child-name]
                  final-transform)))))

(defn create-ground-part [world]
  (let [color (make-color 40 40 40)
        part (create-part :ground color (:info world))
        name :ground-part
        transform (make-transform [0 -0.25 0] [1 0 0 0])
        scale [12 0.5 12]]
      (-> world
          (assoc-in [:parts name] part)
          (assoc-in [:parts name :transform] transform)
          (assoc-in [:parts name :scale] scale)
          (create-relative-transform name :ground))))

(defn get-part-with-color [world color]
  (first (find-if (fn [[name part]]
                    (= (:color part) color))
                  (:parts world))))

(defn get-part-position [world name]
  (let [transform (get-in world [:parts name :transform])]
    (get-transform-position transform)))

(defn get-parent-part [world child-name]
  (if (in? child-name (keys (:ground-children world)))
    :ground
    (find-if (fn [name]
               (let [parent (get-in world [:parts name])]
                 (in? child-name (keys (:children parent)))))
             (keys (:parts world)))))

(defn set-value-0-transform [world part-name]
  (let [parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        parent-transform (:transform parent)
        relative-transform (get-in parent [:children part-name])
        transform (combine-transforms relative-transform parent-transform)]
    (assoc-in world [:parts part-name :transform] transform)))

(defn update-move-plane [world]
  (assoc-in world [:move-plane]
            (get-camera-plane world (get-in world [:camera :pivot]))))

(declare get-closest-snap-point)

(defn inserting? [world]
  (and
   (= (:mode world) :insert)
   (not (= (:insert-type world) :wagon))))

(defn move-cursor [world event]
  (if (inserting? world)
    (let [x (:x event)
          y (:y event)
          snap-spec (get-closest-snap-point world x y (:snap-specs world))
          [snap-point snap-rotation color snapped]
          (if (nil? snap-spec)
            [(get-ground-camera-point world x y 0) [0 1 0 0] :black false]
            [(:position snap-spec) (:rotation snap-spec) :yellow true])
          transform (make-transform snap-point snap-rotation)]
      (-> world
          (update-in [:cursor] (fn [cursor]
                                 (-> cursor
                                     (assoc-in [:transform] transform)
                                     (set-mesh-color color))))
          (assoc-in [:cursor-snapped] snapped)))
    world))

(declare get-part-at)

(defn get-function-value [function t interpolator]
  (let [final-time (first (last function))]
    (cond
      (<= t 0.0) (last (first function))
      (>= t final-time) (last (last function))
      :else
      (let [pairs (map vector function (rest function))
            pair (find-if (fn [[[t0 & _] [t1 & _]]]
                            (<= t0 t t1))
                          pairs)
            t0 (first (first pair))
            t1 (first (second pair))
            s (map-between-ranges t t0 t1 0 1)
            v0 (second (first pair))
            v1 (second (second pair))]
        (interpolator v0 v1 s)))))

(defn create-image [filename x y w h]
  (let [document (read-xml filename)
        image (if (= w -1)
                (parse-svg-from-map-with-height document h)
                (parse-svg-from-map-with-width document w))
        menu {:x x
              :y y
              :w (get-image-width image)
              :h (get-image-height image)
              :image image}
        regions (get-absolute-svg-regions document menu)]
    (assoc-in menu [:regions] regions)))

(defn get-region-at [image x y]
  (first (find-if (fn [[name box]]
                    (inside-box? box x y))
                  (:regions image))))

(defn draw-text-box! [world]
  (let [[text color] (if (:text-input world)
                       [(str (:text world)) :dark-gray]
                       [(:command world) :black])]
    (fill-rect! color 80 13 150 25)
    (draw-text! :green text 15 17 14)))

(defn read-input [world callback]
  (println! "listening for input:")
  (-> world
      (assoc-in [:input-callback] callback)
      (assoc-in [:text-input] true)))

(defn get-parts-with-type [parts type]
  (map first (filter (fn [[name part]]
                       (= (:type part) type))
                     parts)))

(defn print-parts! []
  (println! (keys (:parts @world))))

;;-------------------------------------------------------------------------------;;
;; draw parts

(defn get-tail-transform [track]
  (let [track-transform (:transform track)
        y-offset (* -0.5 (second (:scale track)))]
    (combine-transforms
     (make-transform [0 y-offset 0] [1 0 0 0])
     track-transform)))

;;---

(defn draw-part! [world part]
  (let [info (get-in world [:info (:type part)])
        transform (if (= (:type part) :track)
                    (get-tail-transform part)
                    (:transform part))
        mesh (-> (:model info)
                 (assoc-in [:transform] transform)
                 (assoc-in [:scale] (:scale part))
                 (set-mesh-color (:color part)))]
    (draw-mesh! world mesh)))

;;-------------------------------------------------------------------------------;;
;; snap specs

(defn make-spec [position rotation point]
  (let [rotation-transform (make-transform [0 0 0] rotation)
        transform (make-transform position rotation)
        p (apply-transform transform point)
        normal-table {[ 1  0  0] [0 0 1 -90]
                      [-1  0  0] [0 0 1 90]
                      [ 0  1  0] [1 0 0 0]
                      [ 0 -1  0] [1 0 0 180]
                      [ 0  0  1] [1 0 0 90]
                      [ 0  0 -1] [1 0 0 -90]}
        extra-rotation (get normal-table (map round (vector-normalize point)))
        final-rotation (get-transform-rotation
                        (combine-transforms
                         (make-transform [0 0 0] extra-rotation)
                         (make-transform [0 0 0] rotation)))]
    [p final-rotation]))

;;---

(defn get-snap-specs [world]
  (let [grid-specs (vec (map (fn [[a b]]
                               (let [x (+ (* a 0.5) 0.25)
                                     y (+ (* b 0.5) 0.25)]
                                 {:position [x 0 y]
                                  :rotation [1 0 0 0]
                                  :part :ground-part}))
                             (create-combinations (range 10) (range 10))))
        face-specs (vec
                    (remove-nil
                     (mapcat (fn [[name part]]
                               (let [part (get-in world [:parts name])
                                     transform (:transform part)
                                     position (get-transform-position transform)
                                     rotation (get-transform-rotation transform)
                                     points (get-in world [:info (:type part) :points])
                                     [sa sb sc] (if (= (:type part) :track)
                                                  [1 1 1]
                                                  (:scale part))
                                     points (map (fn [[a b c]]
                                                   [(* sa a) (* sb b) (* sc c)])
                                                 points)]
                                 (map (fn [p]
                                        (let [[pos rot] (make-spec position rotation p)]
                                          {:position pos
                                           :rotation rot
                                           :part name}))
                                      points)))
                             (:parts world))))]
    (vec (concat grid-specs face-specs))))

(defn get-closest-snap-point [world x y snap-specs]
  (let [line (unproject-point world [x y])
        close-points (filter (fn [spec]
                               (< (point-line-distance (:position spec) line) 0.2))
                             snap-specs)
        eye (get-in world [:camera :eye])]
    (first (sort-by (fn [spec]
                      (distance (:position spec) eye)) close-points))))

;;-------------------------------------------------------------------------------;;
;; collision

(defn unproject-point [world [x y]]
  (let [dx (dec (/ x (/ window-width 2)))
        dy (- (dec (/ y (/ window-height 2))))
        p-matrix (:projection-matrix world)
        v-matrix (:view-matrix world)
        matrix (multiply-matrices v-matrix p-matrix)
        inverse-matrix (get-inverse-matrix matrix)
        p-2d-a (into-array Float/TYPE [dx dy -1.0 1.0])
        p-3d-a (into [] (multiply-matrix-vector inverse-matrix p-2d-a))
        p-3d-a (map (slots / _ (nth p-3d-a 3)) p-3d-a)
        p-3d-a (into [] (butlast p-3d-a))

        p-2d-b (into-array Float/TYPE [dx dy 0.0 1.0])
        p-3d-b (into [] (multiply-matrix-vector inverse-matrix p-2d-b))
        p-3d-b (map (slots / _ (nth p-3d-b 3)) p-3d-b)
        p-3d-b (into [] (butlast p-3d-b))]
    [p-3d-a (vector-normalize (vector-subtract p-3d-b p-3d-a))]))

(defn distance-comparator [a b]
  (cond
    (nil? a) false
    (nil? b) true
    (and (nil? a) (nil? b)) a
    :else (< a b)))

(defn get-mesh-triangles [mesh transform scale]
  (let [vertices (partition 3 (into [] (:vertices mesh)))
        matrix (multiply-matrices
                (apply get-scale-matrix scale)
                (get-transform-matrix transform))
        vertices (map (fn [[x y z]]
                        (let [vertex (into-array Float/TYPE [x y z 1])]
                          (butlast (into [] (multiply-matrix-vector
                                             matrix vertex)))))
                      vertices)]
    (partition 3 vertices)))

(defn get-mesh-collision [mesh transform scale line]
  (let [triangles (get-mesh-triangles mesh transform scale)
        measured-triangles (map (fn [i]
                                  {:d (line-triangle-distance
                                       line (nth triangles i))
                                   :i i})
                                (range (count triangles)))
        collision (first (sort-by :d distance-comparator measured-triangles))]
    (if (nil? (:d collision))
      nil
      [(:i collision) (:d collision) (line-get-point line (:d collision))])))

;;---

(defn get-part-collision [world px py]
  (let [line (unproject-point world [px py])
        distances (map (fn [[name part]]
                         (let [type (:type part)
                               info (get-in world [:info type])
                               mesh (:model info)
                               transform (if (= (:type part) :track)
                                           (get-tail-transform part)
                                           (:transform part))
                               scale (:scale part)
                               [i d p] (get-mesh-collision mesh transform scale line)]
                           (if (nil? d)
                             nil
                             {:part-name name
                              :distance d
                              :point p
                              :index i})))
                       (:parts world))]
    (first (sort-by :distance (remove-nil distances)))))

(defn get-part-at [world px py]
  (:part-name (get-part-collision world px py)))

(defn get-collision-normal [world collision]
  (let [{:keys [part-name point index]} collision
        part (get-in world [:parts part-name])
        vertices (get-in world [:info :block :model :vertices])
        triangles (partition 3 (partition 3 vertices))
        [a b c] (nth triangles index)
        v1 (vector-subtract b a)
        v2 (vector-subtract c a)]
    (vector-cross-product v1 v2)))

;;-------------------------------------------------------------------------------;;
;; track loop

(defn tracks-connected? [world p0-name p1-name]
  (let [parts (:parts world)
        p0 (get-in parts [p0-name])
        p1 (get-in parts [p1-name])]
    (or
     (and (in? p1-name (keys (:children p0)))
          (nil? (:loop-fn p1)))

     (and (in? p0-name (keys (:children p1)))
          (nil? (:loop-fn p0))))))

(defn get-track-neighbours [world part-name]
  (filter (fn [other-part-name]
            (and
             (not (= other-part-name part-name))
             (tracks-connected? world other-part-name part-name)))
          (keys (:parts world))))

(defn grow-loop [world loop color]
  (let [start (first loop)
        end (last loop)
        get-next (fn [tip]
                   (first
                    (filter (fn [part-name]
                              (let [part (get-in world [:parts part-name])]
                                (and
                                 (= (:type part) :track)
                                 (= (:color part) color)
                                 (not (in? part-name loop)))))
                            (get-track-neighbours world tip))))
        before (get-next start)
        after (get-next end)]
    (if (and (nil? before)
             (nil? after))
      loop
      (let [new-loop (cond
                       (or (nil? before)
                           (= before after))
                       (conj loop after)

                       (nil? after)
                       (vec (concat [before] loop))

                       :else
                       (vec (concat [before] (conj loop after))))]
        (recur world new-loop color)))))

(defn get-tail-point [world track-name]
  (let [track (get-in world [:parts track-name])
        [_ sy _] (:scale track)]
    (apply-transform (:transform track) [0 (- sy) 0])))

(defn get-track-loop [world t0-name]
  (let [t0 (get-in world [:parts t0-name])
        loop-color (:color t0)
        loop-names (grow-loop world [t0-name] loop-color)
        loop-names (if (in? (get-parent-part world (first loop-names))
                            loop-names)
                     (vec (reverse loop-names))
                     loop-names)
        points (map (fn [name]
                      (get-part-position world name))
                    loop-names)
        tail-point (get-tail-point world (first loop-names))
        points (cons tail-point points)
        inverse-transform (get-inverse-transform (:transform t0))]
    (vec (map #(apply-transform inverse-transform %) points))))

(defn is-extra-point? [a b c]
  (float-equals?
   (vector-dot-product
    (vector-normalize (vector-subtract b a))
    (vector-normalize (vector-subtract c a)))
   1.0))

(defn remove-extra-points [points]
  (concat [(first points)]
          (filter not-nil?
                  (map (fn [n]
                         (let [a (nth points (dec n))
                               b (nth points n)
                               c (nth points (inc n))]
                           (if (is-extra-point? a b c)
                             nil
                             b)))
                       (range 1 (dec (count points)))))
          [(last points)]))

(defn set-wagon-loop [world wagon-name track-name]
  (let [loop (remove-extra-points (get-track-loop world track-name))
        lengths (map (fn [a b]
                       (vector-length (vector-subtract a b)))
                     loop (rest loop))
        total-length (reduce + lengths)
        times (map #(/ % total-length) (accumulate lengths))
        loop-fn (map vector times loop)]
    (update-in world [:parts wagon-name]
               (fn [wagon]
                 (-> wagon
                     (assoc-in [:loop-fn] loop-fn)
                     (assoc-in [:track-lengths] lengths))))))

;;-------------------------------------------------------------------------------;;
;; mechanical tree

(declare compute-subtree-transforms)

(defn compute-children-transforms [world part-name transform key]
  (reduce (fn [w [child-name relative-transform]]
            (let [new-transform (combine-transforms
                                 relative-transform transform)]
              (compute-subtree-transforms w child-name new-transform key)))
          world
          (get-in world [key part-name :children])))

(defn compute-translated-loop-fn [loop-fn]
  (let [offset (second (first loop-fn))]
    (map (fn [[t p]]
           [t (vector-subtract p offset)])
         loop-fn)))

(defn block-compute-subtree-transforms [world name transform key]
  (-> world
      (assoc-in [key name :transform] transform)
      (compute-children-transforms name transform key)))

(defn track-compute-subtree-transforms [world name transform key]
  (let [track (get-in world [:parts name])
        angle (map-between-ranges (:value track) 0.0 1.0 0.0 360.0)
        angle-transform (make-transform [0 0 0] [0 1 0 angle])
        transform (combine-transforms angle-transform transform)
        world (assoc-in world [key name :transform] transform)]
    (reduce (fn [w [child-name relative-transform]]
              (let [new-transform (combine-transforms
                                   relative-transform transform)]
                (compute-subtree-transforms w child-name new-transform key)))
            world
            (get-in world [key name :children]))))

(defn wagon-compute-subtree-transforms [world name transform key]
  (let [wagon (get-in world [:parts name])
        rotation (get-transform-rotation transform)
        loop-fn (if (= key :weld-groups)
                  (compute-translated-loop-fn (:loop-fn wagon))
                  (:loop-fn wagon))
        loop-fn (map (fn [[t v]]
                       [t (apply-transform transform v)])
                     loop-fn)
        value (within (:value wagon) 0.0 1.0)
        position (get-function-value loop-fn value vector-interpolate)
        transform (make-transform position rotation)]
    (-> world
        (assoc-in [key name :transform] transform)
        (compute-children-transforms name transform key))))

(defn compute-subtree-transforms [world name transform key]
  (case (get-in world [:parts name :type])
    :track (track-compute-subtree-transforms world name transform key)
    :wagon (wagon-compute-subtree-transforms world name transform key)
    (block-compute-subtree-transforms world name transform key)))

;;---

(defn compute-transforms [world key]
  (reduce (fn [w [name relative-transform]]
            (compute-subtree-transforms w name relative-transform key))
          world
          (:ground-children world)))

;;----------------------------------------------------------------------;;
;; modes

(defn get-function [mode function]
  (resolve (symbol (str "temp.core/"
                        (subs (str mode) 1)
                        "-mode-"
                        (subs (str function) 1)))))

(declare create-weld-groups)

(defn prepare-tree [world]
  (-> world
      (compute-transforms :parts)
      (#(assoc-in % [:snap-specs] (get-snap-specs %)))
      (create-weld-groups)))

(defn change-mode [world new-mode]
  (println! "entering" new-mode "mode")
  (let [exit-fun (or (get-function (:mode world) :exited) identity)
        enter-fun (or (get-function new-mode :entered) identity)
        world (-> world
                  (exit-fun)
                  (assoc-in [:mode] new-mode)
                  (enter-fun)
                  (prepare-tree))]
    (draw-2d! world)
    world))

(defn mode-mouse-pressed [world event]
  (if-let [fun (get-function (:mode world) :pressed)]
    (fun world event)
    world))

(defn mode-mouse-moved [world event]
  (if-let [fun (get-function (:mode world) :moved)]
    (fun world event)
    world))

(defn mode-mouse-released [world event]
  (if-let [fun (get-function (:mode world) :released)]
    (fun world event)
    world))

;;----------------------------------------------------------------------;;

(load "graph-mode")
(load "cpu-mode")
(load "set-value-mode")
(load "undo")
(load "machines")

;;----------------------------------------------------------------------;;
;; insert mode

(defn spec->transform [offset spec parent]
  (let [final-rotation (:rotation spec)
        rotation-transform (make-transform [0 0 0] final-rotation)
        offset [0 offset 0]
        offset (apply-transform rotation-transform offset)
        final-point (if (= (:type parent) :track)
                      (get-transform-position (:transform parent))
                      (:position spec))
        final-point (vector-add final-point offset)]
    (make-transform final-point final-rotation)))

;;---

(defn get-part-offset [part]
  (if (= (:type part) :track)
    (second (:scale part))
    (* 0.5 (second (:scale part)))))

(defn insert-part [world type color x y]
  (if-let [spec (get-closest-snap-point world x y (:snap-specs world))]
    (let [part (create-part type color (:info world))
          name (gen-keyword type)
          offset (get-part-offset part)
          parent-name (:part spec)
          parent (get-in world [:parts parent-name])
          transform (spec->transform offset spec parent)]
      (-> world
          (assoc-in [:parts name] part)
          (assoc-in [:parts name :transform] transform)
          (create-relative-transform name parent-name)))
    world))

(defn insert-wagon [world color x y]
  (let [part-name (get-part-at world x y)]
    (if (and (not-nil? part-name)
             (= (get-in world [:parts part-name :type]) :track))
      (let [part (create-part :wagon color (:info world))
            name (gen-keyword :wagon)
            transform (get-in world [:parts part-name :transform])]
        (-> world
            (assoc-in [:parts name] part)
            (assoc-in [:parts name :transform] transform)
            (assoc-in [:parts part-name :children name]
                      (make-transform [0 0 0] [1 0 0 0]))
            (set-wagon-loop name part-name)))
      world)))

(declare create-sphere)

(defn insert-sphere [world x y]
  (if-let [collision (get-part-collision world x y)]
    (let [normal (get-collision-normal world collision)
          offset (vector-multiply (vector-normalize normal)
                                  (:sphere-radius world))
          position (vector-add (:point collision) offset)]
      (create-sphere world position))
    (let [line (unproject-point world [x y])
          ground-plane [[0 0 0] [1 0 0] [0 0 1]]
          offset [0 (:sphere-radius world) 0]
          point (line-plane-intersection line ground-plane)
          position (vector-add point offset)]
      (create-sphere world position))))

(defn insert-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (case (:insert-type world)
      :block
      (insert-part world :block :white x y)

      :track
      (insert-part world :track :red x y)

      :wagon
      (insert-wagon world :yellow x y)

      :chip
      (insert-part world :chip :gray x y)

      :cpu
      (insert-part world :cpu :blue x y)

      :probe
      (insert-part world :probe :purple x y)

      :button
      (insert-part world :button :black x y)

      :sphere
      (insert-sphere world x y))))

;;----------------------------------------------------------------------;;
;; cable mode

(defn get-cable-at [world x y]
  ;;################################
  )

;;----------------------------------------------------------------------;;
;; edit mode

(defn set-object-color [world x y]
  (if-let [cable-name (get-cable-at world x y)]
    (assoc-in world [:cables cable-name :color] (:current-color world))
    (if-let [part-name (get-part-at world x y)]
      (assoc-in world [:parts part-name :color] (:current-color world))
      world)))

(defn rotate-part [world event]
  (if-let [part-name (get-part-at world (:x event) (:y event))]
    (let [part (get-in world [:parts part-name])
          transform (:transform part)
          rotation (make-transform [0 0 0] [0 1 0 90])
          transform (combine-transforms rotation transform)
          parent-name (get-parent-part world part-name)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (create-relative-transform part-name parent-name)))
    world))

;;-------------------------------------------------------------------------------;;
;; delete mode

;;---

(declare get-sphere-at)
(declare delete-sphere)

(defn forget-part [world parent-name part-name]
  (-> world
      (dissoc-in [:parts parent-name :children part-name])
      (dissoc-in [:parts parent-name :inputs part-name])
      (dissoc-in [:parts parent-name :outputs part-name])
      (dissoc-in [:parts parent-name :functions part-name])))

(defn unselect-part [world part-name]
  (cond
    (= part-name (:selected-chip world))
    (dissoc-in world [:selected-chip])

    (= part-name (:selected-cpu world))
    (dissoc-in world [:selected-cpu])

    :else world))

(defn delete-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
  ;; (if-let [cable-name (get-cable-at world x y)]
  ;;   (dissoc-in world [:cables cable-name])
    (if-let [sphere (get-sphere-at world x y)]
      (delete-sphere world sphere)
      (if-let [part-name (get-part-at world x y)]
        (let [world (reduce #(forget-part %1 %2 part-name)
                            world
                            (keys (:parts world)))]
          (-> world
              (unselect-part part-name)
              (dissoc-in [:parts part-name])))
        world))))

;;----------------------------------------------------------------------;;
;; scale mode

(defn set-block-size [world block-name original-scale
                      original-center increase]
  (let [block (get-in world [:parts block-name])
        new-scale (map (fn [a b]
                         (if (zero? b)
                           a
                           (abs b)))
                       original-scale increase)
        scale-change (vector-subtract new-scale original-scale)
        value (if (some neg? increase)
                -0.5
                0.5)
        part-rotation (get-transform-rotation (:transform block))
        rotation-transform (make-transform [0 0 0] part-rotation)
        offset (apply-transform rotation-transform
                                (vector-multiply scale-change value))
        new-center (vector-add original-center offset)
        new-transform (make-transform new-center part-rotation)]
    (-> world
        (assoc-in [:parts block-name :scale] (map abs new-scale))
        (assoc-in [:parts block-name :transform] new-transform))))

(defn scale-block-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            vertices (get-in world [:info :block :model :vertices])
            triangles (partition 3 (partition 3 vertices))
            [a b c] (nth triangles index)
            v1 (vector-subtract b a)
            v2 (vector-subtract c a)
            normal (vector-cross-product v1 v2)
            rotation-transform (get-rotation-component (:transform part))
            v (apply-transform rotation-transform normal)
            scale (:scale part)
            center (get-transform-position (:transform part))]
        (-> world
            (assoc-in [:edited-part] part-name)
            (create-weld-groups)
            (assoc-in [:adjust-line] [point (vector-normalize v)])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)
            (assoc-in [:normal] normal)))
        world)))

(defn scale-block-moved [world event]
  (if-let [block-name (:edited-part world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          grain-size 0.1
          d (* grain-size (round (/ d grain-size)))
          scale (:original-scale world)
          center (:original-center world)
          normal (:normal world)
          l (within (+ d (abs (reduce + (map * normal scale)))) 0.1 10)
          increase-vector (map * (:normal world) [l l l])]
      (println! (format "scale: %.2f" l))
      (-> world
          (set-block-size block-name scale center increase-vector)
          (assoc-in [:increase-vector] increase-vector)))
    world))

(defn scale-block-released [world event]
  (if-let [block-name (:edited-part world)]
    (let [parent-name (get-parent-part world block-name)
          scale (:original-scale world)
          increase-vector (:increase-vector world)
          world (-> world
                    (assoc-in [:parts block-name :scale] scale)
                    (set-value-0-transform block-name))
          center (get-part-position world block-name)]
      (-> world
          (set-block-size block-name scale center increase-vector)
          (create-relative-transform block-name parent-name)
          (dissoc-in [:edited-part])))
    world))

(defn set-track-size [world track-name original-scale original-center height]
  (let [track (get-in world [:parts track-name])
        new-scale (assoc original-scale 1 height)
        scale-change (vector-subtract new-scale original-scale)
        part-rotation (get-transform-rotation (:transform track))
        rotation-transform (make-transform [0 0 0] part-rotation)
        new-center (->> scale-change
                        (apply-transform rotation-transform)
                        (vector-add original-center))
        new-transform (make-transform new-center part-rotation)]
    (-> world
        (assoc-in [:parts track-name :scale] new-scale)
        (assoc-in [:parts track-name :transform] new-transform))))

(defn scale-track-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point _]} (get-part-collision world x y)]
      (let [part (get-in world [:parts part-name])
            scale (:scale part)
            transform (:transform part)
            center (get-transform-position transform)
            rotation-transform (get-rotation-component transform)
            v (apply-transform rotation-transform [0 1 0])]
        (-> world
            (assoc-in [:edited-part] part-name)
            (create-weld-groups)
            (assoc-in [:adjust-line] [point (vector-normalize v)])
            (assoc-in [:original-scale] scale)
            (assoc-in [:original-center] center)))
      world)))

(defn scale-track-moved [world event]
  (if-let [track-name (:edited-part world)]
    (let [adjust-line (:adjust-line world)
          mouse-line (unproject-point world [(:x event) (:y event)])
          d (line-line-closest-point adjust-line mouse-line)
          grain-size 0.1
          d (* grain-size (round (/ d grain-size)))
          scale (:original-scale world)
          center (:original-center world)
          normal (second adjust-line)
          l (within (+ (apply max scale) d) grain-size 10)]
      (println! (format "scale: %.2f" l))
      (-> world
          (set-track-size track-name scale center l)
          (assoc-in [:track-length] l)))
    world))

(defn scale-track-released [world event]
  (if-let [track-name (:edited-part world)]
    (let [parent-name (get-parent-part world track-name)
          scale (:original-scale world)
          world (-> world
                    (assoc-in [:parts track-name :scale] scale)
                    (set-value-0-transform track-name))
          center (get-part-position world track-name)
          track-length (:track-length world)]
      (-> world
          (set-track-size track-name scale center track-length)
          (create-relative-transform track-name parent-name)
          (dissoc-in [:edited-part])))
    world))

;;---

(defn scale-mode-pressed [world event]
  (if-let [part-name (get-part-at world (:x event) (:y event))]
    (let [type (get-in world [:parts part-name :type])
          world (assoc-in world [:scale-type] type)]
      (case type
        :block (scale-block-pressed world event)
        :wagon (scale-block-pressed world event)
        :track (scale-track-pressed world event)
        world))
    world))

(defn scale-mode-moved [world event]
  (case (:scale-type world)
    :block (scale-block-moved world event)
    :wagon (scale-block-moved world event)
    :track (scale-track-moved world event)
    world))

(defn scale-mode-released [world event]
  (let [world (case (:scale-type world)
                :block (scale-block-released world event)
                :wagon (scale-block-released world event)
                :track (scale-track-released world event)
                world)]
    (dissoc-in world [:scale-type])))

;;----------------------------------------------------------------------;;
;; move mode

(defn move-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [{:keys [part-name point index]} (get-part-collision world x y)]
      (let [w (set-value-0-transform world part-name)
            part (get-in w [:parts part-name])
            v1 (apply-rotation (:transform part) [1 0 0])
            v2 (apply-rotation (:transform part) [0 0 1])
            plane [point (vector-add point v1) (vector-add point v2)]
            part-position (get-part-position world part-name)
            offset (vector-subtract part-position point)]
        (-> world
            (assoc-in [:edited-part] part-name)
            (create-weld-groups)
            (assoc-in [:plane] plane)
            (assoc-in [:offset] offset)
            (assoc-in [:original-position] part-position)))
      world)))

(defn move-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [line (unproject-point world [(:x event) (:y event)])
          touch-point (line-plane-intersection line (:plane world))
          position (vector-add touch-point (:offset world))
          [a b c] (:plane world)
          v1 (vector-subtract b a)
          v2 (vector-subtract c a)
          origin (:original-position world)
          s (point-line-coordinate position [origin v1])
          t (point-line-coordinate position [origin v2])
          grain-size 0.1
          s (* grain-size (round (/ s grain-size)))
          t (* grain-size (round (/ t grain-size)))
          snapped-position (reduce vector-add [origin
                                               (vector-multiply v1 s)
                                               (vector-multiply v2 t)])
          part (get-in world [:parts part-name])
          rotation (get-transform-rotation (:transform part))
          transform (make-transform snapped-position rotation)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (assoc-in [:snapped-position] snapped-position)))
    world))

(defn move-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (get-parent-part world part-name)
          world (set-value-0-transform world part-name)
          part (get-in world [:parts part-name])
          rotation (get-transform-rotation (:transform part))
          snapped-position (:snapped-position world)
          transform (make-transform snapped-position rotation)]
      (-> world
          (assoc-in [:parts part-name :transform] transform)
          (create-relative-transform part-name parent-name)
          (dissoc-in [:edited-part])))
    world))

;;----------------------------------------------------------------------;;
;; translate mode

(defn translate-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [part-name (:part-name (get-part-collision world x y))]
      (assoc-in world [:edited-part] part-name)
      world)))

(defn translate-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [x (:x event)
          y (:y event)
          part (get-in world [:parts part-name])]
      (if (= (:type part) :wagon)
        world
        (if-let [spec (get-closest-snap-point world x y
                                              (:snap-specs world))]
          (let [offset (get-part-offset part)
                parent-name (:part spec)
                parent (get-in world [:parts parent-name])
                transform (spec->transform offset spec parent)]
            (-> world
                (assoc-in [:parts part-name :transform] transform)
                (assoc-in [:parent-name] parent-name)))
          world)))
    world))

(defn translate-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [x (:x event)
          y (:y event)
          part (get-in world [:parts part-name])
          new-parent-name (:parent-name world)
          old-parent-name (get-parent-part world part-name)
          world (-> world
                    (dissoc-in [:parts old-parent-name
                                :children part-name])
                    (dissoc-in [:edited-part]))]
      (if (= (:type part) :wagon)
        (let [track-name (get-part-at world x y)
              transform (get-in world [:parts track-name :transform])]
          (-> world
              (assoc-in [:parts part-name :transform] transform)
              (assoc-in [:parts track-name :children part-name]
                        (make-transform [0 0 0] [1 0 0 0]))
              (set-wagon-loop part-name track-name)))
        (create-relative-transform world part-name new-parent-name)))
    world))

;;----------------------------------------------------------------------;;
;; paste mode

(defn change-keys [map suffix]
  (map-map (fn [[name value]]
             {(join-keywords name suffix)
              value})
           map))

(defn copy-part [parts part-name suffix]
  (let [copy-name (join-keywords part-name suffix)
        part (-> (get-in parts [part-name])
                 (update-in [:children] #(change-keys % suffix))
                 (update-in [:functions] #(change-keys % suffix))
                 (update-in [:inputs] #(change-keys % suffix))
                 (update-in [:outputs] #(change-keys % suffix)))]
    (assoc-in parts [copy-name] part)))

(defn copy-tree [parts part-name suffix]
  (let [copy-name (join-keywords part-name suffix)
        parts (copy-part parts part-name suffix)
        part (get-in parts [part-name])
        parts (reduce (fn [ps child-name]
                        (first (copy-tree ps child-name suffix)))
                      parts
                      (keys (:children part)))]
    [parts copy-name]))

(defn clean-map [map valid-keys]
  (map-map (fn [[key value]]
             (if (in? key valid-keys)
               {key value}
               (let [index (.indexOf (str key) "-")
                     old-key (keyword (subs (str key) 1 index))]
                 {old-key value})))
           map))

(defn change-missing-references [parts]
  (let [part-names (keys parts)]
    (map-map (fn [[name part]]
               {name
                (-> part
                    (update-in [:functions] #(clean-map % part-names))
                    (update-in [:inputs] #(clean-map % part-names))
                    (update-in [:outputs] #(clean-map % part-names)))})
             parts)))

(defn paste-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [part-name (:part-name (get-part-collision world x y))]
      (assoc-in world [:edited-part] part-name)
      world)))

(defn paste-mode-moved [world event]
  (if-let [part-name (:edited-part world)]
    (let [x (:x event)
          y (:y event)
          part (get-in world [:parts part-name])]
      (if-let [spec (get-closest-snap-point world x y
                                            (:snap-specs world))]
        (let [offset (get-part-offset part)
              parent-name (:part spec)
              parent (get-in world [:parts parent-name])
              transform (spec->transform offset spec parent)]
          (-> world
              (assoc-in [:parts part-name :transform] transform)
              (assoc-in [:parent-name] parent-name)))
        world))
    world))

(defn paste-mode-released [world event]
  (if-let [part-name (:edited-part world)]
    (let [parent-name (:parent-name world)
          suffix (gen-keyword :copy)
          [parts copy-part-name] (copy-tree (:parts world) part-name suffix)
          parts (change-missing-references parts)]
      (-> world
          (assoc-in [:parts] parts)
          (create-relative-transform copy-part-name parent-name)
          (dissoc-in [:edited-part])))
    world))

;;----------------------------------------------------------------------;;
;; pivot mode

(defn pivot-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)
        part-name (get-part-at world x y)
        pos (if (= part-name :ground-part)
              (let [line (unproject-point world [x y])
                    ground-plane [[0 0 0] [1 0 0] [0 0 1]]]
                (line-plane-intersection line ground-plane))
              (get-part-position world part-name))]
    (compute-camera (assoc-in world [:camera :pivot] pos))))

;;----------------------------------------------------------------------;;
;; idle mode

(defn idle-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [part-name (get-part-at world x y)]
      (let [part (get-in world [:parts part-name])]
        (if (= (:type part) :button)
          (-> world
              (assoc-in [:parts part-name :value] 1)
              (assoc-in [:button] part-name))
          world))
      world)))

(defn idle-mode-released [world event]
  (if-let [button-name (:button world)]
    (-> world
        (assoc-in [:parts button-name :value] 0)
        (dissoc-in [:button]))
    world))

;;----------------------------------------------------------------------;;
;; extra parts

(defn point-inside-block? [block point]
  (let [transform (:transform block)
        inverse-transform (get-inverse-transform transform)
        local-point (apply-transform inverse-transform point)]
    (every? (fn [[v d]]
              (< (abs v) (/ d 2)))
            (map vector local-point (:scale block)))))

(defn inside-any-block? [parts position]
  (find-if (fn [block-name]
             (let [block (get-in parts [block-name])]
               (point-inside-block? block position)))
           (get-parts-with-type parts :block)))

(defn set-probe-value [world probe-name]
  (if-let [probe (if (:use-weld-groups world)
                   (get-in world [:weld-groups probe-name])
                   (get-in world [:parts probe-name]))]
    (let [position (get-transform-position (:transform probe))
          value (if (inside-any-block? (:parts world) position)
                  1
                  0)]
      (assoc-in world [:parts probe-name :value] value))
    world))

(defn set-probe-values [world]
  (reduce (fn [w probe-name]
            (set-probe-value w probe-name))
          world
          (get-parts-with-type (:parts world) :probe)))

(defn get-chips-with-input [world input-name]
  (let [chips (get-parts-with-type (:parts world) :chip)]
    (filter (fn [chip-name]
              (in? input-name (get-in world [:parts chip-name :inputs])))
            chips)))

(defn draw-buttons! [world]
  (let [button-names (get-parts-with-type (:parts world) :button)]
    (doseq [button-name button-names]
      (let [button (get-in world [:parts button-name])
            base-transform (:transform button)
            rotation (get-transform-rotation (:transform button))
            rotation-transform (make-transform [0 0 0] rotation)
            up (apply-transform rotation-transform [0 1 0])
            offset (if (= (:value button) 1)
                     (make-transform (vector-multiply up 0.02) [1 0 0 0])
                     (make-transform (vector-multiply up 0.1) [1 0 0 0]))
            transform (combine-transforms base-transform offset)
            mesh (assoc-in (:button-mesh world) [:transform] transform)]
        (draw-mesh! world mesh)))))

;;----------------------------------------------------------------------;;
;; regular physics

(defn create-sphere [world position]
  (let [body (create-sphere-body
              (:sphere-radius world) 1.0
              (make-transform position [1 0 0 0]))]
    (add-body-to-planet (:planet world) body)
    (update-in world [:spheres] (partial cons body))))

(defn delete-sphere [world sphere]
  (remove-body (:planet world) sphere)
  (update-in world [:spheres]
             (fn [spheres]
               (remove #(= % sphere) spheres))))

(defn get-sphere-at [world x y]
  (let [line (unproject-point world [x y])
        radius (:sphere-radius world)
        spheres (filter (fn [sphere]
                          (let [transform (get-body-transform sphere)
                                position (get-transform-position transform)]
                            (< (point-line-distance position line) radius)))
                        (:spheres world))
        eye (get-in world [:camera :eye])]
    (first (sort-by (fn [sphere]
                      (let [transform (get-body-transform sphere)
                            position (get-transform-position transform)]
                        (distance position eye)))
                    spheres))))

(defn is-physical-part? [[name part]]
  (and
   (in? (:type part) [:block :wagon])
   (= (:color part) :yellow)))

(defn compute-kinematic-body [part-name parts groups]
  (let [part (get-in parts [part-name])
        position (get-transform-position (:transform part))
        rotation (get-transform-rotation (:transform part))
        scale (:scale part)
        body (create-kinematic-body position rotation scale)
        root-name (first (find-if #(in? part-name %) groups))
        root (get-in parts [root-name])
        part-transform (:transform part)
        root-transform (:transform root)
        relative-transform (remove-transform part-transform
                                             root-transform)]
    {:body body
     :transform relative-transform
     :root root-name}))

(defn compute-kinematic-bodies [parts groups]
  (let [physical-part-names (map first (filter is-physical-part? parts))]
    (map #(compute-kinematic-body % parts groups)
         physical-part-names)))

(defn remove-all-bodies [world]
  (doseq [{:keys [body]} (:bodies world)]
    (remove-body (:planet world) body))
  (assoc-in world [:bodies] []))

(defn create-kinematic-bodies [world parts groups]
  (let [world (remove-all-bodies world)
        kinematic-bodies (compute-kinematic-bodies parts groups)]
    (doseq [{:keys [body]} kinematic-bodies]
      (add-body-to-planet (:planet world) body))
    (assoc-in world [:bodies] kinematic-bodies)))

(defn recompute-body-transforms! [world]
  (doseq [b (:bodies world)]
    (let [body (:body b)
          parent (get-in world [:weld-groups (:root b)])
          relative-transform (:transform b)
          parent-transform (:transform parent)
          transform (combine-transforms relative-transform
                                        parent-transform)]
      (set-body-transform body transform))))

;;---

(defn draw-spheres! [world]
  (let [mesh (:sphere-mesh world)]
    (doseq [body (:spheres world)]
      (let [transform (get-body-transform body)
            mesh (assoc-in mesh [:transform] transform)]
        (draw-mesh! world mesh)))))

;;-------------------------------------------------------------------------------;;
;; commands

(defn edit-mode-draw [world]
  (let [{:keys [image x y]} (:color-palette world)]
    (draw-image! image x y))

  (let [color-box (get-in world [:color-palette
                                 :regions (:current-color world)])
        {:keys [x y w h]} color-box]
    (dotimes [i 3]
      (draw-rect! :black x y (- w i 1) (- h i 1)))))

(defn edit-mode-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (if-let [color-name (get-region-at (:color-palette world) x y)]
      (assoc-in world [:current-color] color-name)
      (case (:edit-subcommand world)
        :color (set-object-color world x y)
        :move (move-mode-pressed world event)
        :translate (translate-mode-pressed world event)
        :paste (paste-mode-pressed world event)
        :scale (scale-mode-pressed world event)
        :delete (delete-mode-pressed world event)
        :rotate (rotate-part world event)
        world))))

(defn edit-mode-moved [world event]
  (case (:edit-subcommand world)
    :move (move-mode-moved world event)
    :translate (translate-mode-moved world event)
    :paste (paste-mode-moved world event)
    :scale (scale-mode-moved world event)
    world))

(defn edit-mode-released [world event]
  (case (:edit-subcommand world)
    :move (move-mode-released world event)
    :translate (translate-mode-released world event)
    :paste (paste-mode-released world event)
    :scale (scale-mode-released world event)
    world))

;;----------------------------------------------------------------------;;

(do
1

(defn get-bindings []
  {"C-x i" (fn [w]
             (-> w
                 (change-mode :insert)
                 (assoc-in [:insert-type] :block)))
   ":insert b" (fn [w]
                 (assoc-in w [:insert-type] :block))
   ":insert w" (fn [w]
                 (assoc-in w [:insert-type] :wagon))
   ":insert t" (fn [w]
                 (assoc-in w [:insert-type] :track))
   ":insert c" (fn [w]
                 (assoc-in w [:insert-type] :chip))
   ":insert m" (fn [w] ;;#########
                 (assoc-in w [:insert-type] :cpu))
   ":insert p" (fn [w]
                 (assoc-in w [:insert-type] :probe))
   ":insert a" (fn [w] ;;#########
                 (assoc-in w [:insert-type] :button))
   ":insert s" (fn [w]
                 (assoc-in w [:insert-type] :sphere))

   "C-x e" (fn [w]
             (-> w
                 (change-mode :edit)
                 (assoc-in [:edit-subcommand] :move)))
   ":edit d" (fn [w]
               (assoc-in w [:edit-subcommand] :delete))
   ":edit c" (fn [w]
               (assoc-in w [:edit-subcommand] :color))
   ":edit s" (fn [w]
               (assoc-in w [:edit-subcommand] :scale))
   ":edit m" (fn [w]
               (assoc-in w [:edit-subcommand] :move))
   ":edit t" (fn [w]
               (assoc-in w [:edit-subcommand] :translate))
   ":edit p" (fn [w]
               (assoc-in w [:edit-subcommand] :paste))
   ":edit r" (fn [w]
               (assoc-in w [:edit-subcommand] :rotate))

   "C-x g" #(change-mode % :graph)
   ":graph m" (fn [w]
                (assoc-in w [:graph-subcommand] :move))
   ":graph x" (fn [w]
                (assoc-in w [:graph-subcommand] :set-x))
   ":graph y" (fn [w]
                (assoc-in w [:graph-subcommand] :set-y))
   ":graph C-c x" (fn [w]
                (assoc-in w [:graph-subcommand] :set-both))
   ":graph a" (fn [w]
                (assoc-in w [:graph-subcommand] :add))
   ":graph d" (fn [w]
                (assoc-in w [:graph-subcommand] :delete))
   ":graph r" #(run-selected-chip %)
   ":graph s" (fn [w]
                (dissoc-in w [:selected-chip]))
   ":graph t" (fn [w]
                (assoc-in w [:graph-subcommand] :toggle-relative))
   ":graph 1" #(reset-graph-view %)
   ":graph C-c s" #(set-snap-value %)
   ":graph l" (fn [w]
                (assoc-in w [:graph-subcommand] :print-lengths))

   "C-x m" #(change-mode % :cpu) ;;#########
   ":cpu s" (fn [w]
              (dissoc-in w [:selected-cpu]))
   ":cpu r" #(run-selected-cpu %)
   ":cpu l" #(load-script %)

   "." (fn [w]
         (change-mode w :pivot))

   "C-x r" (fn [w]
             (println! "reset world")
             (create-world))

   "C-x s" #(read-input % save-machine-callback)
   "C-x C-s" save-version
   "C-x l" #(read-input % load-machine-callback)
   "C-x C-l" #(read-input % load-last-version-callback)

   "C-x v" #(change-mode % :set-value)

   "C-/" #(undo! %)
   "C-x /" #(redo! %)
   })

(set-thing! [:bindings] (get-bindings)))

(defn get-key [control-pressed code]
  (if-let [name (get-in keymap [code])]
    (if control-pressed
      (str "C-" name)
      name)
    nil))

(defn find-binding [bindings command mode]
  (if-let [fun (get bindings command)]
    fun
    (second (find-if (fn [[keys fun]]
                      (let [mode (str mode)]
                        (and (.startsWith keys mode)
                             (.equals (subs keys (inc (count mode))) command))))
                    bindings))))

(defn execute-command [world]
  (if-let [fun (find-binding (:bindings world)
                             (:command world)
                             (:mode world))]
    (-> world
        (assoc-in [:end-of-command] true)
        (fun))
    world))

(defn text-input-key-pressed [world event]
  (let [key (get-in keymap [(:code event)])]
    (case key
      :enter
      (let [callback (:input-callback world)
            world (try
                    (callback world (:text world))
                    (catch Exception e
                      (do
                        (println! "invalid input:" (:text world))
                        world)))
            world (-> world
                      (dissoc-in [:text])
                      (assoc-in [:text-input] false))]
        (save-checkpoint! world))

      :backspace
      (if (empty? (:text world))
        world
        (update-in world [:text] #(apply str (butlast %))))

      (update-in world [:text]
                 (fn [text]
                   (apply str (concat text key)))))))

(defn key-pressed [world event]
  (if (in? (:code event) [341 345])
    (assoc-in world [:control-pressed] true)
    (if-let [key (get-key (:control-pressed world) (:code event))]
      (if (= key "C-g")
        (-> world
            (assoc-in [:command] "")
            (assoc-in [:text] "")
            (assoc-in [:text-input] false)
            (assoc-in [:mode] :idle))
        (if (:text-input world)
          (text-input-key-pressed world event)
          (-> world
              (update-in [:command] (fn [c]
                                      (if (or (empty? c)
                                              (:end-of-command world))
                                        key
                                        (str c " " key))))
              (assoc-in [:end-of-command] false)
              (execute-command))))
      world)))

(defn key-released [world event]
  (draw-2d! world)

  (if (in? (:code event) [341 345])
    (assoc-in world [:control-pressed] false)
    world))

;;----------------------------------------------------------------------;;

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
        (assoc-in [:graph-box] {:x 343 :y 540
                                :w 685 :h 150
                                :buffer (new-image 685 150)
                                })
        (assoc-in [:cpu-box] {:x 343 :y 540 :w 685 :h 150})
        (prepare-tree)
        (update-move-plane)
        (assoc-in [:cursor] (create-cone-mesh [0 -5 0] [1 0 0 0]
                                              [0.05 0.1 0.05] :black))

        (assoc-in [:command] "")
        (assoc-in [:mode] :idle)
        (assoc-in [:bindings] (get-bindings))
        (assoc-in [:current-color] :red)
        (assoc-in [:color-palette]
                  (create-image "resources/colors.svg" 150 590 -1 40))
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

        (assoc-in [:graph-snap-value] 0.1)
        (reset-undo! [:ground-children :planet :spheres :parts])
    )))
(reset-world!)
)

(defn update-world [world elapsed]
  (if (in? (:mode world) [:insert :edit])
    world
    (let [world (-> world
                    (run-chips elapsed)
                    (compute-transforms (if (:use-weld-groups world)
                                          :weld-groups
                                          :parts))
                    (set-probe-values)
                    (cpus-input-changes)
                    ;; (enforce-cable-lengths)
                    )]
      (recompute-body-transforms! world)
      (step-simulation! (:planet world) elapsed)
      world)))

(defn draw-3d! [world]
  (doseq [mesh (vals (:background-meshes world))]
    (draw-mesh! world mesh))

  (doseq [mesh (vals (:meshes world))]
    (draw-mesh! world mesh))

  (if (:use-weld-groups world)
    (doseq [group (vals (:weld-groups world))]
      (draw-mesh! world group))
    (doseq [part (vals (:parts world))]
      (draw-part! world part)))

  (if-let [edited-part (:edited-part world)]
    (let [part (get-in world [:parts edited-part])]
      (draw-part! world part)))

  (draw-buttons! world)
  (draw-spheres! world)

  ;; (doseq [cable (:cables world)]
  ;;   (draw-cable! world cable))

  (if (and
       (= (:mode world) :graph)
       (:selected-chip world))
    (draw-mesh! world (:selected-mesh world)))

  (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
  (if (inserting? world)
    (draw-mesh! world (:cursor world)))
  (draw-debug-meshes!)
  )

(do
1

(defn draw-2d! [world]
  (clear!)
  (draw-text-box! world)
  (if-let [fun (get-function (:mode world) :draw)]
    (fun world))
  (draw-output!)
  )
(redraw!))

(defn mouse-scrolled [world event]
  (if (and (= (:mode world) :graph)
           (inside-box? (:graph-box world) (:x event) (:y event)))
    (graph-mode-scrolled world event)
    (let [amount (+ 1 (* (:amount event) -0.05))]
      (zoom-camera world amount))))

(defn mouse-pressed [world event]
  (let [x (:x event)
        y (:y event)]
    (cond
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
    (-> world
        (move-cursor event)
        (mode-mouse-moved event))))

(defn mouse-released [world event]
  (let [world (cond
                (not-nil? (:last-point world))
                (-> world
                    (dissoc-in [:last-point])
                    (update-move-plane))

                :else
                (mode-mouse-released world event))]
    (draw-2d! world)
    (-> world
        (prepare-tree)
        (save-checkpoint!))))
