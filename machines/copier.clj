
[button source destination in-place]

(fn [part-name]
  (when (and (= part-name button)
             (on? button))
    (let [original (get-part source)]
      (set-thing! [:edit-subcommand] :copy)
      (mode-click! :edit source [:control])
      (mode-click! :edit destination [])

      (when (= (get-value in-place) 1.0)
        (set-transform @copy-name (get-transform original)))
      
      (println! "copy" original "to" (get-part destination) (rand))
      )))
