
(ns temp.core)

(def undo-ring (atom (vec (range 10))))
(def undo-index (atom 0))

(defn reset-undo! []
  (reset! undo-ring (vec (range 10)))
  (reset! undo-index 0))

(defn save-checkpoint! []
  ;; (println! "saved checkpoint")
  ;; (swap! undo-ring (fn [ring]
  ;;                    (let [index (mod @undo-index (count @undo-ring))]
  ;;                      (assoc-in ring [index] @world))))
  ;; (swap! undo-index inc)
  (println! "save checkpoint")
  nil)

(defn undo! []
  ;; (swap! undo-index dec)
  ;; (let [index (mod (dec @undo-index) (count @undo-ring))]
  ;;     (reset! world (nth @undo-ring index))
  ;;   nil)
  ;; (println! @undo-ring @undo-index)
  (println! "undo")
  nil
  )

(defn redo! []
  ;; (swap! undo-index inc)
  ;; (let [index (mod (dec @undo-index) (count @undo-ring))]
  ;;   (reset! world (nth @undo-ring index))
  ;;   nil)
  (println! "redo")
  )

;;----------------------------------------------------------------------;;

;; (save-checkpoint!)

;; (do
;;   (clear-output!)
;;   (println! @undo-index)
;;   (println! (nth @undo-ring 2)))

;; (do
;;   (reset! world (nth @undo-ring 0))
;;   nil)
