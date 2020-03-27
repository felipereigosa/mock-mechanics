
(ns temp.core)

(defn show-hint [world menu action]
  (let [texts {:action {}
               :mode {:insert "Ctrl + a,Add"
                      :edit "Ctrl + e,Edit"
                      }
               :insert {}
               :edit {}
               :cpu {}
               :chip {}
               }
        world (assoc-in world [:hint]
                        {:text (get-in texts [menu action])
                         :time (get-current-time)})]
    (.start
     (new Thread
          (proxy [Runnable] []
            (run []
              (try
                (sleep 2500)
                (redraw!)
                (catch Exception e))))))
    (redraw!)
    world))

(defn draw-hint [hint]
  (let [elapsed (- (get-current-time) (:time hint))
        [command description] (split (:text hint) #",")]
    (if (< elapsed 2000)
      (let [x (/ (:window-width world) 2)
            y (- (/ (:window-height world) 2) 50)]
        (fill-rect! :black x y 300 150)
        (draw-text! :white command (+ (- x 150) 40) y 20)
        (draw-text! :red description (+ x 30) y 20)))))
