
[button source destination in-place]

(fn [part-name]
  (when (on? button)
    (let [original (get-part source)]
      (set-thing! [:edit-subcommand] :copy)
      (set-thing! [:selected-part] nil)
      (mode-click! :edit source [:control])
      (mode-click! :edit destination [])

      (when (on? in-place)
        (set-transform
         @copy-name
         (combine-transforms (get-transform original)
                             (make-transform [-0.001 0 0] [1 0 0 0]))))

      (while (not (:use-weld-groups @world)))
      (set-value button 0))))
