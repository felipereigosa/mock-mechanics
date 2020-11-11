
(ns temp.core (:gen-class))

;; (defn set-indicator-text [world text]
;;   (.start
;;    (new Thread
;;         (proxy [Runnable] []
;;           (run []
;;             (try
;;               (sleep 1000)
;;               (set-thing! [:input-indicator :text] "")
;;               (redraw!)
;;               (catch Exception e))))))
;;   (assoc-in world [:input-indicator :text] text))

(defn create-input-indicator [world]
  ;; (let [x 105 
  ;;       y 670 
  ;;       w 200]
  ;;   (assoc-in world [:input-indicator]
  ;;             {:picture (create-picture "indicator" x y w -1)
  ;;              :left (:image (create-picture "left-button" x y w -1))
  ;;              :middle (:image (create-picture "middle-button" x y w -1))
  ;;              :right (:image (create-picture "right-button" x y w -1))
  ;;              :up (:image (create-picture "up-button" x y w -1))
  ;;              :down (:image (create-picture "down-button" x y w -1))
  ;;              :text ""
  ;;              :button nil}))
  world
  )

;; (defn pretty-key [text]
;;   (if (= (count text) 3)
;;     (let [modifier (case (first text)
;;                      \C "Ctrl"
;;                      \S "Shift"
;;                      \A "Alt")]
;;       (str modifier " + " (.toUpperCase (subs text 2))))
;;     (.toUpperCase text)))

(defn input-indicator-key-pressed [world event]
  ;; (redraw (let [key-name (get-in keymap [(:code event)])]
  ;;           (cond
  ;;             (= key-name :delete) (do ;;##############################
  ;;                                    (clear-output!)
  ;;                                    world)

  ;;             (= key-name :control)
  ;;             (-> world
  ;;                 (assoc-in [:control-pressed] true)
  ;;                 (assoc-in [:input-indicator :text] "Ctrl"))
              
  ;;             (= key-name :shift)
  ;;             (-> world
  ;;                 (assoc-in [:shift-pressed] true)
  ;;                 (assoc-in [:input-indicator :text] "Shift"))
              
  ;;             (= key-name :alt)
  ;;             (-> world
  ;;                 (assoc-in [:alt-pressed] true)
  ;;                 (assoc-in [:input-indicator :text] "Alt"))

  ;;             (= key-name :esc)
  ;;             (-> world
  ;;                 (set-indicator-text "Esc")
  ;;                 (cancel-action))

  ;;             :else
  ;;             (if-let [key (get-key (:code event)
  ;;                                   (:control-pressed world)
  ;;                                   (:alt-pressed world)
  ;;                                   (:shift-pressed world))]
  ;;               (cond
  ;;                 (:text-input world)
  ;;                 (text-input-key-pressed world event)

  ;;                 (string? key)
  ;;                 (-> world
  ;;                     (update-in [:command] (fn [c]
  ;;                                             (if (empty? c)
  ;;                                               key
  ;;                                               (str c " " key))))
  ;;                     (set-indicator-text (pretty-key key))
  ;;                     (execute-command)))
  ;;               world))))
  world
  )

(defn input-indicator-key-released [world event]
  ;; (let [key-name (get-in keymap [(:code event)])

  ;;       text (get-in world [:input-indicator :text])
  ;;       world (if (in? text ["Ctrl" "Shift" "Alt"])
  ;;               (assoc-in world [:input-indicator :text] "")
  ;;               world)
  ;;       world (redraw world)
  ;;       ]
  ;;   (cond
  ;;     (= key-name :control) (assoc-in world [:control-pressed] false)
  ;;     (= key-name :shift) (assoc-in world [:shift-pressed] false)
  ;;     (= key-name :alt) (assoc-in world [:alt-pressed] false)
  ;;     :else world))
  world
  )

(defn draw-input-indicator! [world ]
  ;; (let [input-indicator (:input-indicator world)
  ;;       picture (:picture input-indicator)
  ;;       {:keys [image x y]} picture
  ;;       text-region (get-absolute-region
  ;;                    (get-in picture [:regions :text]) picture)
  ;;       ]
  ;;   (draw-image! image x y)
  ;;   (if-let [button (:button input-indicator)]
  ;;     (draw-image! (get input-indicator button) x y))
  ;;   (draw-text-in-box! (:text input-indicator) :white 20 text-region)
  ;;   )
  )

(defn input-indicator-mouse-scrolled [world event]
  ;; (let [world (redraw (assoc-in world [:input-indicator :button]
  ;;                       (if (pos? (:amount event))
  ;;                         :up
  ;;                         :down)))]
  ;;   (.start
  ;;    (new Thread
  ;;         (proxy [Runnable] []
  ;;           (run []
  ;;             (try
  ;;               (sleep 300)
  ;;               (set-thing! [:input-indicator :button] nil)
  ;;               (redraw!)
  ;;               (catch Exception e))))))    
  ;;   (if (and (= (:mode world) :graph)
  ;;            (inside-box? (:graph-box world) (:x event) (:y event)))
  ;;     (graph-mode-scrolled world event)
  ;;     (let [amount (+ 1 (* (:amount event) -0.05))]
  ;;       (zoom-camera world amount))))
  world
  )

(defn input-indicator-mouse-pressed [world event]
  ;; (let [x (:x event)
  ;;       y (:y event)
  ;;       world (-> world
  ;;                 (assoc-in [:press-time] (get-current-time))
  ;;                 (assoc-in [:press-point] [x y])
  ;;                 (assoc-in [:input-indicator :button] (:button event))
  ;;                 (redraw))]
  ;;   (cond
  ;;     (and
  ;;      (show-buttons? world)
  ;;      (inside-box? (:action-menu world) x y))
  ;;     (action-menu-pressed world x y)

  ;;     (and
  ;;      (show-buttons? world)
  ;;      (inside-box? (:mode-menu world) x y))
  ;;     (mode-menu-pressed world x y)

  ;;     (and
  ;;      (in? (:button event) [:middle :right])
  ;;      (not (and (= (:mode world) :graph)
  ;;                (inside-box? (:graph-box world) x y))))
  ;;     (assoc-in world [:last-point] [x y])

  ;;     :else
  ;;     (mode-mouse-pressed world event)))
  (println! "indicator mouse pressed")
  world
  )

(defn input-indicator-mouse-released [world event]
  ;; (let [elapsed (- (get-current-time) (:press-time world))
  ;;       world (if (and (< elapsed 200)
  ;;                      (= (:button event) :right)
  ;;                      (< (distance (:press-point world)
  ;;                                   [(:x event) (:y event)]) 10))
  ;;               (set-pivot world event)
  ;;               world)
  ;;       world (-> world
  ;;                 (dissoc-in [:press-point])
  ;;                 (dissoc-in [:input-indicator :button])
  ;;                 (redraw))
  ;;       ]
  ;;   (if (not-nil? (:last-point world))
  ;;     (dissoc-in world [:last-point])
  ;;     (mode-mouse-released world event)))
  world
  )
