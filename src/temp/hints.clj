
(defn show-hint [world menu action]
  (if (:show-hints world)
    (let [texts {:action {:new "Ctrl + N,New File"
                          :view "Ctrl + C,Center Camera"
                          :save "Ctrl + S,Save"
                          :open "Ctrl + O,Open"
                          :undo "Ctrl + Z,Undo"
                          :redo "Ctrl + R,Redo"
                          :cancel "Esc,Cancel Action"
                          }
                 :mode {:add "Alt + A,Add Mode"
                        :edit "Alt + E,Edit Mode"
                        :graph "Alt + G,Graph Mode"
                        :motherboard "Alt + M,Motherboard Mode"
                        :color "Alt + C,Color Mode"
                        :property "Alt + P,Property Mode"
                        :layer "Alt + L,Layer Mode"
                        :avatar "Alt + F,Avatar Mode"
                        :simulation "Alt + S,Simulation Mode"
                        }
                 :add {:block "B,Block"
                       :cylinder "C,Cylinder"
                       :cone "Shift + C,Cone"
                       :sphere "S,Sphere"
                       :wagon "W,Wagon"
                       :track "T,Track"
                       :chip "G,Graph Chip"
                       :motherboard "M,Motherboard"
                       :probe "P,Probe"
                       :button "Shift + B,Button"
                       :lamp "L,Lamp"
                       :speaker "Shift + S,Speaker"
                       :gear "Shift + G,Gear"
                       :display "I,Image"
                       }
                 :edit {:move "M,Move"
                        :sink "H,Change Height"
                        :rotate "R,Rotate"
                        :delete "D,Delete"
                        :scale "S,Scale"
                        :copy "C,Copy/Paste"
                        :translate "T,Transfer"
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
                               :editor "E,Open editor"
                               }
                 }
          world (assoc-in world [:hint]
                          {:text (get-in texts [menu action])
                           :time (get-current-time)
                           })]
      (do-later redraw! 1000)
      (redraw world))
    world))

(defn show-message [world message]
  (let [world (assoc-in world [:hint]
                {:text message
                 :time (get-current-time)})]
    (do-later redraw! 1000)
    (redraw world)))

(defn draw-hint! [world]
  (when (:show-hints world)
    (if-let [hint (:hint world)]
      (let [elapsed (- (get-current-time) (:time hint))]
        (if (< elapsed 800)
          (let [x (/ (:window-width world) 2)
                y (- (/ (:window-height world) 2) 50)]
            (fill-rect! :black x y 500 150)
            (if (= (.indexOf (:text hint) ",") -1)
              (let [box {:x x :y y :w 500 :h 150}]
                (draw-text-in-box! (:text hint) :white 20 box))
              (let [[command description] (split (:text hint) #",")
                    left-box {:x (- x 125) :y y :w 250 :h 150}
                    right-box {:x (+ x 125) :y y :w 250 :h 150}]
                (draw-line! :white x (- y 50) x (+ y 50))
                (draw-text-in-box! command :white  20 left-box)
                (draw-text-in-box! description :red  20 right-box)))))))))
