
(ns temp.core)

(load "set-value-mode")
(load "idle-mode")
(load "toggle-mode")
(load "edit-mode")
(load "graph-mode")
(load "cpu-mode")
(load "insert-mode")

(defn get-function [mode function]
  (resolve (symbol (str "temp.core/"
                        (subs (str mode) 1)
                        "-mode-"
                        (subs (str function) 1)))))

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
