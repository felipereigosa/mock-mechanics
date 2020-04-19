
(ns temp.core)

(defn show-hint [world menu action]
  (let [texts {:action {}
               :mode {:insert "Ctrl + a,Add Mode"
                      :edit "Ctrl + e,Edit Mode"
                      :graph "Ctrl + g,Graph Mode"
                      :cpu "Ctrl + q,Cpu Mode"
                      :color "Ctrl + c,Color Mode"
                      :set-value "Ctrl + v,Set Value Mode"
                      :toggle "Ctrl + t,Toggle Mode"
                      :layer "Ctrl + L,Layer Mode"
                      :idle "Ctrl + S,Simulation Mode"
                      }
               :insert {:block "b,Block"
                        :wagon "w,Wagon"
                        :track "t,Track"
                        :chip "g,Chip"
                        :cpu "c,Cpu"
                        :probe "p,Probe"
                        :button "a,Button"
                        :sphere "s,Sphere"
                        :cylinder "h,Cylinder"
                        :cone "v,Cone"
                        :lamp "l,Lamp"
                        :speaker "o,Loudspeaker"
                        }
               :edit {:move "m,Move"
                      :sink "v,Vertical Move"
                      :rotate "r,Rotate"
                      :delete "d,Delete"
                      :scale "s,Scale"
                      :copy "c,Copy/Paste"
                      :translate "t,Translate"
                      }
               :graph {:move "m,Move"
                       :set-x "x,Set x"
                       :set-y "y,Set y"
                       :add "a,Add Node"
                       :delete "d,Delete Node"
                       :run "r,Run"
                       :toggle-relative "t,Toggle Relative"
                       :view "v,View"
                       :print-lengths "l,Print Lenghts"
                       }
               :cpu {:move "m,Move"
                     :and "a,And Gate"
                     :or "o,Or Gate"
                     :not "n,Not Gate"
                     :delete "d,Delete"
                     :connect "c,Connect"
                     :toggle "t, Toggle Trigger"
                     :run "r,Run"
                     :script "s,Toggle Script"
                     }
               }
        world (assoc-in world [:hint]
                        {:text (get-in texts [menu action])
                         :time (get-current-time)})]
    (.start
     (new Thread
          (proxy [Runnable] []
            (run []
              (try
                (sleep 1000)
                (redraw!)
                (catch Exception e))))))
    (redraw world)))

(defn draw-hint [world hint]
  (let [elapsed (- (get-current-time) (:time hint))
        [command description] (split (:text hint) #",")]
    (if (< elapsed 800)
      (let [x (/ (:window-width world) 2)
            y (- (/ (:window-height world) 2) 50)
            left-box {:x (- x 125) :y y :w 250 :h 150}
            right-box {:x (+ x 125) :y y :w 250 :h 150}]
        (fill-rect! :black x y 500 150)
        (draw-line! :white x (- y 50) x (+ y 50))
        (draw-text-in-box! command :white  20 left-box)
        (draw-text-in-box! description :red  20 right-box)))))
