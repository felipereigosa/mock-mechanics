
(load "property-mode")
(load "simulation-mode")
(load "add-mode")
(load "edit-mode")
(load "graph-mode")
(load "motherboard-mode")
(load "color-mode")
(load "layer-mode")
(load "physics-mode")
(load "avatar-mode")

(defn get-function [mode function]
  (resolve (symbol (str "mockmechanics.core/"
                        (subs (str mode) 1)
                        "-mode-"
                        (subs (str function) 1)))))

(defn change-mode [world new-mode]
  (let [exit-fun (or (get-function (:mode world) :exited) identity)
        enter-fun (or (get-function new-mode :entered) identity)]
    (-> world
        (assoc-in [:mode] new-mode)
        (exit-fun)
        (enter-fun)
        (redraw))))

(defn mode-mouse-pressed [world event]
  (if-let [fun (get-function (:mode world) :pressed)]
    (-> world
        (compute-transforms :parts)
        (fun event)
        (redraw))
    world))

(defn mode-mouse-moved [world event]
  (if-let [fun (get-function (:mode world) :moved)]
    (fun world event)
    world))

(defn mode-mouse-released [world event]
  (let [fun (or (get-function (:mode world) :released)
                (fn [w e] w))]
    (-> world
        (fun event)
        (compute-transforms :parts)
        (save-checkpoint!)
        (redraw))))
