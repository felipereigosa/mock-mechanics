
[button pointer]

;; (defn mode-click! [mode pointer keys]
;;   (println! (get-function mode :pressed))
;;   (println! (get-function mode :moved))
;;   (println! (get-function mode :released))
;;   )

(defn probe->line [probe-name]
  (let [world @world
        transform (use-root-relative-transform world probe-name)
        point (get-transform-position transform)
        direction (vector-normalize (apply-transform (get-rotation-component transform) [0 1 0]))
        offset (vector-multiply direction 0.051)]
    [(vector-add point offset) direction]))

(defn make-spec [probe-name]
  {:x 100000
   :y 100000
   :line (probe->line probe-name)})

(defn mode-click! [mode pointer keys]
  (let [spec (make-spec pointer)
        press-function (get-function mode :pressed)
        move-function (get-function mode :moved)
        release-function (get-function mode :released)]
    (swap! world (fn [w] (-> w
                             (press-function spec)
                             (release-function spec))))))

(fn [part-name]
  (when (and (= part-name button)
             (= (get-value button) 1))

    (set-thing! [:add-type] :block)
    (mode-click! :add pointer [])

    ;; (set-thing! [:edit-subcommand] :move)
    ;; (mode-press! :edit source-pointer [])
    ;; (mode-release! :release source-pointer [:shift])
    
    ;; (mode-click! :copy source-pointer [:control])
    ;; (mode-click! :copy destination-pointer [])
    ;; (if (= (get-value in-place) 1.0)
    ;;   (set-thing! [:parts copied-part-name :transform]
    ;;               (make-transform [0 0 0] [1 0 0 0])))
    ))
