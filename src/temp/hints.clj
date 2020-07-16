
(ns temp.core (:gen-class))

(defn show-hint [world menu action]
  (let [texts {:action {:new "Alt + N,New File"
                        :view "Alt + C,Reset Camera"
                        :save "Alt + S,Save"
                        :open "Alt + O,Open"
                        :undo "Alt + ←,Undo"
                        :redo "Alt + →,Redo"
                        :cancel "Esc,Cancel Action"
                        }
               :mode {:add "Ctrl + A,Add Mode"
                      :edit "Ctrl + E,Edit Mode"
                      :graph "Ctrl + G,Graph Mode"
                      :motherboard "Ctrl + M,Motherboard Mode"
                      :color "Ctrl + C,Color Mode"
                      :property "Ctrl + P,Property Mode"
                      :toggle "Ctrl + T,Toggle Mode"
                      :layer "Ctrl + L,Layer Mode"
                      :simulation "Ctrl + S,Simulation Mode"
                      }
               :add {:block "B,Block"
                        :wagon "W,Wagon"
                        :track "T,Track"
                        :chip "G,Graph Chip"
                        :motherboard "M,Motherboard"
                        :probe "P,Probe"
                        :button "Shift + B,Button"
                        :sphere "S,Sphere"
                        :cylinder "C,Cylinder"
                        :cone "V,Cone"
                        :lamp "L,Lamp"
                        :speaker "Shift + L,Loudspeaker"
                        }
               :edit {:move "M,Move"
                      :sink "V,Vertical Move"
                      :rotate "R,Rotate"
                      :delete "D,Delete"
                      :scale "S,Scale"
                      :copy "C,Copy/Paste"
                      :translate "T,Translate"
                      }
               :graph {:move "M,Move"
                       :set-x "X,Set x"
                       :set-y "Y,Set y"
                       :add "A,Add Node"
                       :delete "D,Delete Node"
                       :run "R,Run"
                       :toggle-relative "T,Toggle Relative"
                       :view "V,View"
                       :print-lengths "L,Print Lenghts"
                       }
               :motherboard {:move "M,Move"
                     :and "A,And Gate"
                     :or "O,Or Gate"
                     :not "N,Not Gate"
                     :delete "D,Delete"
                     :connect "C,Connect"
                     :toggle "T, Toggle Trigger"
                     :run "R,Run"
                     :script "S,Toggle Script"
                     :on "1,Set Pin to 1"
                     :off "0,Set Pin to 0"
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
