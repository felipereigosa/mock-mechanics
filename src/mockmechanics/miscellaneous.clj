(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]
            [clojure.java.io :as io]))

(declare get-parts-with-type)
(declare get-tail-transform)
(declare get-part-position)
(declare save-version)
(declare undo!)
(declare redo!)
(declare show-hint)
(declare change-mode)

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

(defn read-input [world callback]
  (-> world
      (assoc-in [:input-callback] callback)
      (assoc-in [:text-input] true)))

(declare get-spec-line)

(defn pivot-animation [world animation]
  (let [{:keys [t start end]} animation]
    (if (float= t 1.0)
      (-> world
          (assoc-in [:camera :pivot] end)
          (compute-camera))
      (-> world
          (assoc-in [:camera :pivot]
                    (vector/interpolate start end (sigmoid t)))
          (compute-camera)))))

(defn create-pivot-animation [world event]
  (let [x (:x event)
        y (:y event)
        part-name (get-part-at world event)
        part (get-in world [:parts part-name])
        pos (cond
              (nil? part-name)
              (let [line (get-spec-line world event)
                    ground-plane [[0 0 0] [1 0 0] [0 0 1]]]
                (line-plane-intersection line ground-plane))

              (= (:type part) :track)
              (get-transform-position (get-tail-transform part))

              :else
              (get-part-position world part-name))
        pivot (get-in world [:camera :pivot])]
    {:start pivot
     :end pos
     :time (max 0.8 (* 0.1 (vector/distance pivot pos)))
     :t 0
     :fn pivot-animation}))

(declare delete-all-parts)
(declare delete-all-spheres)
(declare create-physics-world)
(declare create-layer-info)
(declare reset-avatar)

(defn create-directory! [name]
  (.mkdirs (new File name)))

(defn delete-temp-files! []
  (let [files (filter #(.isFile %)
                      (file-seq (io/file "temp")))]
    (doseq [file files]
      (io/delete-file file))))

(defn new-file [world]
  (set-title! "-")
  (reset! motherboard-activation-count 0)
  (delete-temp-files!)
  (-> world
      (dissoc-in [:last-saved-machine])
      (delete-all-parts)
      (delete-all-spheres)
      (reset-camera)
      (change-mode :simulation)
      (create-layer-info)
      (create-physics-world)
      (reset-avatar)
      (redraw)))

(defn place-box [world name & {:keys [rx ry wx wy ox oy]}]
  (let [{:keys [x y w h]} (get-in world [name])
        window-width (:window-width world)
        window-height (:window-height world)

        x (if (nil? rx)
            (* window-width wx)
            (let [n (neg? rx)
                  rx (abs rx)
                  x (* rx w)]
              (if n
                (- window-width x)
                x)))

        y (if (nil? ry)
            (* window-height wy)
            (let [n (neg? ry)
                  ry (abs ry)
                  y (* ry h)]
              (if n
                (- window-height y)
                y)))
        ox (or ox 0)
        oy (or oy 0)]
    (-> world
        (assoc-in [name :x] (+ x ox))
        (assoc-in [name :y] (+ y oy)))))

(defn place-elements [world]
  (let [oy (- (+ (* (:num-lines world) 16) 10))
        menu-offset (if (:show-submenu world) 30 0)]
    (-> world
        (place-box :action-menu :rx 0.6 :ry 0.5 :oy 10)
        (place-box :mode-menu :rx -0.6 :ry 0.5 :oy 10)
        (place-box :add-menu :wx 0.5 :ry -0.6 :oy oy)
        (place-box :color-palette :wx 0.5 :ry -0.5 :oy oy)
        (place-box :edit-menu :wx 0.5 :ry -0.6 :oy oy)
        (place-box :layer-box :wx 0.5 :ry -0.5 :oy oy)
        (place-box :graph-box :wx 0.5 :ry -0.5 :oy (- oy menu-offset))
        (place-box :graph-menu :wx 0.5 :ry -0.5 :oy oy)
        (place-box :motherboard-box :wx 0.5 :ry -0.5 :oy (- oy menu-offset))
        (place-box :motherboard-menu :wx 0.5 :ry -0.5 :oy oy)
        (place-box :property-box :wx 0.5 :ry -0.5 :oy oy))))

(defn point-between-points? [p p1 p2 d]
  (if (vector/equal? p1 p2)
    (vector/equal? p p1)
    (let [v (vector/subtract p2 p1)
          line [p1 (vector/normalize v)]
          l (vector/length v)]
      (and
        (< (point-line-distance p line) d)
        (< (vector/distance p p1) l)
        (< (vector/distance p p2) l)))))

(defn run-animation [world elapsed]
  (if-let [animation (:animation world)]
    (let [time (or (:time animation) 1.0)
          dt (/ elapsed (* time 1000))
          new-t (min (+ (:t animation) dt) 1.0)
          world (-> world
                    ((:fn animation) animation)
                    (assoc-in [:animation :t] new-t))]
      (reset! time-since-update 0)
      (if (float= new-t 1.0)
        (-> world
            ((:fn animation) (assoc-in animation [:t] 1.0))
            (dissoc-in [:animation]))
        world))
    world))

(defn read-and-execute-command [world]
  (read-input world (fn [w text]
                      (let [atoms (split text #" ")
                            function (->> (first atoms)
                                          (str "mockmechanics.core/")
                                          (symbol)
                                          (resolve))]
                        (try
                          (apply function w (rest atoms))
                          (catch Exception e
                            (println "Error with command:" text)
                            w))))))
