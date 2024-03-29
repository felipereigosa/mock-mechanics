(ns mockmechanics.core
  (:require [mockmechanics.library.util :as util]
            [mockmechanics.library.vector :as vector]
            [mockmechanics.library.matrix :as matrix]))

(defn compute-camera [world]
  (let [camera (:camera world)
        pivot (:pivot camera)
        eye (-> (:vector camera)
                (vector/multiply (:distance camera))
                (vector/rotate [1 0 0] (- (:x-angle camera)))
                (vector/rotate [0 1 0] (- (:y-angle camera)))
                (vector/add pivot))]
    (-> world
        (assoc-in [:view-matrix] (matrix/get-look-at eye pivot [0 1 0]))
        (assoc-in [:camera :eye] eye))))

(defn create-camera [world vector distance x-angle y-angle]
  (-> world
      (assoc-in [:camera] {:vector vector
                           :distance distance
                           :x-angle x-angle
                           :y-angle y-angle
                           :pivot [0 0 0]})
      (compute-camera)))

(defn rotate-camera [world dx dy]
  (let [x-speed 0.4
        y-speed 0.4
        camera (-> (:camera world)
                   (update-in [:x-angle]
                              (fn [angle]
                                (util/within (+ angle (* dy y-speed)) -89 89)))
                   (update-in [:y-angle] (fn [angle] (+ angle (* dx y-speed)))))]
    (-> world
        (assoc-in [:camera] camera)
        (compute-camera))))

(declare unproject-point)

(defn pan-camera [world x1 y1 x2 y2]
  (let [l1 (unproject-point world [x1 y1])
        l2 (unproject-point world [x2 y2])
        plane [[0 0 0] [0 0 1] [1 0 0]]
        p1 (line-plane-intersection l1 plane)
        p2 (line-plane-intersection l2 plane)
        d (vector/subtract p1 p2)]
    (-> world
        (update-in [:camera :pivot] (fn [pivot]
                                      (vector/add pivot d)))
        (compute-camera))))

(defn get-camera-plane [world point]
  (let [camera (:camera world)
        to-camera (vector/subtract (:eye camera) (:pivot camera))
        x-axis (vector/cross-product [0 1 0] to-camera)
        y-axis (vector/cross-product x-axis to-camera)
        p1 point
        p2 (vector/add point x-axis)
        p3 (vector/add point y-axis)]
    [p1 p2 p3]))

(defn zoom-camera [world amount]
  (-> world
      (update-in [:camera :distance] #(* % amount))
      (compute-camera)))

(defn mouse-rotate [world event]
  (let [[x y] (:last-point world)
        dx (- (:x event) x)
        dy (- (:y event) y)]
    (-> world
        (rotate-camera dx dy)
        (assoc-in [:last-point] [(:x event) (:y event)]))))

(defn mouse-pan [world event]
  (let [[x1 y1] (:last-point world)
        x2 (:x event)
        y2 (:y event)]
    (-> world
        (pan-camera x1 y1 x2 y2)
        (assoc-in [:last-point] [x2 y2]))))

(defn reset-camera [world]
  (create-camera world [0 0 1] 40 25 -35))

(declare get-transform-position)

(defn view-all-parts [world]
  (let [real-parts (dissoc-in (:parts world) [:ground-part])]
    (if (empty? real-parts)
      (reset-camera world)
      (let [center (vector/multiply
                     (reduce vector/add
                             (map #(get-transform-position (:transform %))
                                  (vals real-parts)))
                     (/ 1.0 (count real-parts)))
            distance 50]
        (-> world
            (assoc-in [:camera :pivot] center)
            (assoc-in [:camera :distance] distance)
            (compute-camera))))))
