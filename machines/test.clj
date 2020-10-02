
[button source-pointer destination-pointer in-place]

;; (defn copy-part [part-name]
;;   (let [suffix (gen-keyword :copy)
;;         [parts copy-part-name] (copy-tree (:parts @world) part-name suffix)
;;         copied-parts (get-tree-with-root parts part-name)
;;         parts (fix-references parts copied-parts suffix)
;;         ]

;;     (println! (get-in parts [:copy-part-name 
;;     ;; (-> world
;;     ;;     (assoc-in [:parts] parts)
;;     ;;     (create-relative-transform part-name parent-name)
;;     ;;     )
;;     ))

;; (defn place-part [part-name pointer]
;;   (println! "place part")
;;   )

;; (defn change-parent [part-name parent-name]
;;   (println! "change parent")
;;   )

(defn mode-click! [mode pointer keys]
  (println! (get-function mode :pressed))
  (println! (get-function mode :moved))
  (println! (get-function mode :released))
  )

(fn [part-name]
  (when (and (= part-name button)
             (= (get-value button) 1))

    (set-thing! [:add-type] :block)
    (mode-click! :add destination-pointer [])

    ;; (set-thing! [:edit-subcommand] :move)
    ;; (mode-press! :edit source-pointer [])
    ;; (mode-release! :release source-pointer [:shift])
    
    ;; (mode-click! :copy source-pointer [:control])
    ;; (mode-click! :copy destination-pointer [])
    ;; (if (= (get-value in-place) 1.0)
    ;;   (set-thing! [:parts copied-part-name :transform]
    ;;               (make-transform [0 0 0] [1 0 0 0])))
    ))
