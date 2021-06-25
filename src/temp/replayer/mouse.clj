;; (defn insert-instruction! [filename index instruction]
;;   (let [lines (with-open [rdr (clojure.java.io/reader filename)]
;;                 (vec (line-seq rdr)))
;;         new-lines (vector-insert lines instruction index)]
;;     (spit filename (apply str (interpose "\n" new-lines)))))

;; (defn change-event [event start-time]
;;   [(int (:x event)) (int (:y event)) (- (get-current-time) start-time)])

;; (defn replay-pressed [world event]
;;   (if (:replay-filename world)
;;     ;; (if (:active @robot)
;;     ;;   world
;;     ;;   (-> world
;;     ;;       (assoc-in [:replay-button] (dekeyword (:button event)))
;;     ;;       (assoc-in [:replay-events]
;;     ;;                 [(change-event event (:press-time world))])))
;;     (do
;;       ;; (println! "pressed" event)
;;       world)
;;     world))

;; (defn replay-moved [world event]
;;   (if (:replay-filename world)
;;     ;; (if (not-nil? (:replay-events world))
;;     ;;   (update-in world [:replay-events]
;;     ;;              #(conj % (change-event event (:press-time world))))
;;     ;;   world)
;;     (do
;;       ;; (println! "moved" event)
;;       world)
;;     world))

;; (defn replay-released [world event]
;;   (if (:replay-filename world)
;;     ;; (if (:active @robot)
;;     ;;   world
;;     ;;   (let [points (conj (:replay-events world)
;;     ;;                      (change-event event (:press-time world)))
;;     ;;         button (:replay-button world)
;;     ;;         instruction (str "mouse " button " " (join " " points))]
;;     ;;     (insert-instruction! (str "res/" (:replay-filename world) ".txt")
;;     ;;                          (:instruction-index world)
;;     ;;                          instruction)
;;     ;;     (-> world
;;     ;;         (dissoc-in [:replay-events])
;;     ;;         (update-in [:instruction-index] inc))))
;;     (do
;;       ;; (println! "released" event)
;;       world)
;;     world))
