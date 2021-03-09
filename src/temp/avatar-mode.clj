
(ns temp.core (:gen-class))

(load "running")
(load "jumping")

;; (defn get-shadow-transform [world point]
;;   (if-let [[block-name point-underneath]
;;         (first
;;          (sort-by (comp second second) >
;;                   (filter not-nil?
;;                           (map (fn [[name block]]
;;                                  (if-let [point (project-down world block point)]
;;                                    [name point]
;;                                    nil))
;;                                (:blocks world)))))]
;;     (let [block (get-in world [:blocks block-name])
;;           block-transform (:transform block)
;;           block-rotation (get-transform-rotation block-transform)
;;           [_ h _] point-underneath
;;           position (assoc-in point [1] h)]
;;       (make-transform position block-rotation))
;;     (make-transform [0 0 0] [1 0 0 0])))

;; (defn draw-shadow! [world]
;;   (let [avatar (:avatar world)
;;         avatar-position (:position avatar)
;;         avatar-height (nth avatar-position 1)
;;         transform (get-shadow-transform world avatar-position)
;;         height (second (get-transform-position transform))
;;         size (map-between-ranges (- avatar-height height) 0 10 1 0.3)        
;;         scale (vector-multiply [1 0 1] size)
;;         scale (assoc-in scale [1] 0.1)
;;         mesh (-> (:shadow-mesh avatar)
;;                  (assoc-in [:transform] transform)
;;                  (assoc-in [:scale] scale))]
;;     (draw-mesh! world mesh)))

(defn avatar-mode-draw-3d! [world]
  (let [avatar (:avatar world)
        position (:position avatar)
        angle (:angle avatar)
        rotation [0 1 0 angle]
        transform (make-transform position rotation)
        mesh (assoc-in (:mesh avatar) [:transform] transform)]
    (draw-mesh! world mesh)
    ;; (draw-shadow! world)
    ))

(defn normalize-cameraman [world]
  (let [avatar (:avatar world)
        avatar-position (:position avatar)
        cameraman (:cameraman world)
        cameraman-position (:position cameraman)
        to-cameraman (vector-normalize
                      (vector-subtract
                       cameraman-position avatar-position))
        to-cameraman (vector-multiply to-cameraman (:distance cameraman))
        new-cameraman-position (vector-add avatar-position to-cameraman)]
    (assoc-in world [:cameraman :position] new-cameraman-position)))

(defn set-view [world]
  (let [avatar (:avatar world)
        cameraman (:cameraman world)
        avatar-position (:position avatar)
        cameraman-position (:position cameraman)
        cameraman-position (assoc cameraman-position
                                  1 (:height cameraman))
        matrix (get-look-at-matrix cameraman-position
                                   avatar-position [0 1 0])]
    (assoc-in world [:view-matrix] matrix)))

(defn get-state-function [state fn-name]
  (let [function-name (str fn-name "-" (subs (str state) 1) "-state")]
    (ns-resolve 'temp.core (symbol function-name))))

(defn change-state [world new-state]
  (let [avatar (:avatar world)
        current-state (:state avatar)
        exit-fn (or (get-state-function current-state "exit") identity)
        enter-fn (or (get-state-function new-state "enter") identity)]
    (-> world
        (exit-fn)
        (assoc-in [:avatar :state] new-state)
        (enter-fn))))

(defn update-state [world]
  (let [state (get-in world [:avatar :state])
        update-fn (get-state-function state "update")]
    (update-fn world)))

(defn is-avatar-active? [world]
  (let [joystick @joystick
        buttons (:buttons joystick)
        axes (:axes joystick)]
    (not
     (and
      (every? #(= % :released) (vals buttons))
      (every? #(float= % 0.0) (vals axes))))))

(defn avatar-mode-update [world elapsed]
  (-> world
      (update-state)
      (normalize-cameraman)
      (set-view)))

(defn avatar-mode-entered [world]
  (let [parts (:parts world)
        blocks (vec (map first (filter is-solid-part? parts)))]
    (-> world
        (assoc-in [:block-names] (conj blocks :ground))
        (change-state :running))))

(defn get-solid-block [world block-name]
  (if (= block-name :ground)
    (:other-ground world)
    (let [part (get-in world [:parts block-name])]
      (-> (get-in world [:info :block :model])
          (assoc-in [:transform] (:transform part))
          (assoc-in [:scale] (:scale part))))))

(defn project-down [world block point]
  (let [line [point [0 -1 0]]
        [_ d ground-point]
        (get-mesh-collision block (:transform block)
                            (:scale block) line)]
    (if (and (not-nil? d)
             (< d -0.1))
      nil
      ground-point)))
