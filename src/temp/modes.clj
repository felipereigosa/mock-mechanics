
(ns temp.core)

(load "set-value-mode")
(load "simulation-mode")
(load "toggle-mode")
(load "add-mode")
(load "edit-mode")
(load "graph-mode")
(load "cpu-mode")
(load "color-mode")
(load "layer-mode")
(load "physics-mode")

(defn get-function [mode function]
  (resolve (symbol (str "temp.core/"
                        (subs (str mode) 1)
                        "-mode-"
                        (subs (str function) 1)))))

(defn change-mode [world new-mode]
  (let [exit-fun (or (get-function (:mode world) :exited) identity)
        enter-fun (or (get-function new-mode :entered) identity)]
    (-> world
        (exit-fun)
        (assoc-in [:mode] new-mode)
        (enter-fun)
        (redraw))))

(defn prepare-simulation [world]
  (if (= (:mode world) :simulation)
    (compute-transforms world :parts)
    world))

(defn mode-mouse-pressed [world event]
  (if-let [fun (get-function (:mode world) :pressed)]
    (-> world
        (fun event)
        (prepare-simulation)
        (redraw))
    world))

(defn mode-mouse-moved [world event]
  (if-let [fun (get-function (:mode world) :moved)]
    (fun world event)
    world))

(declare reset-wagons)

(defn mode-mouse-released [world event]
  (let [fun (or (get-function (:mode world) :released)
                (fn [w e] w))]
    (-> world
        (fun event)
        (reset-wagons)
        (prepare-tree)
        (redraw))))

(defn draw-mode-text! [world]
  (let [text (str (kw->str (:mode world)) " mode")
        size 15
        width (get-text-width! text size)
        ww (:window-width world)
        wh (:window-height world)
        x (- ww width 10)
        y (- wh 6)]
    (draw-text! :gray text x y size)))
