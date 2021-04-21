
[button source destination in-place]

(fn [part-name]
  (when (not (on? button))
    (if-let [original (get-part source)]
      (do
        (set-thing! [:edit-subcommand] :copy)
        (set-thing! [:selected-part] nil)
        (mode-click! :edit source [:control])
        (mode-click! :edit destination [])
        (set-transform @copy-name (get-transform destination))
        
        (while (not (:use-weld-groups @world)))
        ))))
