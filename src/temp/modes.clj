
(ns temp.core)

(load "set-value-mode")
(load "idle-mode")
(load "toggle-mode")
(load "insert-mode")
(load "edit-mode")
(load "graph-mode")
(load "cpu-mode")
(load "color-mode")
(load "layer-mode")

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
        (prepare-tree)
        (redraw))))

(defn mode-mouse-pressed [world event]
  (if-let [fun (get-function (:mode world) :pressed)]
    (-> world
        (fun event)
        (redraw))
    world))

(defn mode-mouse-moved [world event]
  (if-let [fun (get-function (:mode world) :moved)]
    (fun world event)
    world))

(defn mode-mouse-released [world event]
  (if-let [fun (get-function (:mode world) :released)]
    (-> world
        (fun event)
        (redraw))
    world))
