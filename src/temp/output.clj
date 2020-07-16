
(ns temp.core (:gen-class))

(def output (atom ""))

(defn draw-mode-text! [world]
  (let [text (str (kw->str (:mode world)) " mode")
        size 15
        width (get-text-width! text size)
        ww (:window-width world)
        wh (:window-height world)
        x (- ww width 10)
        y (- wh 6)]
    (fill-rect! :black x y 250 32)
    (draw-text! :gray text x y size)))

(defn draw-buffer! [world]
  (let [ww (:window-width world)
        wh (:window-height world)
        hww (* ww 0.5)
        hwh (* wh 0.5)
        num-lines (:num-lines world)
        height (+ (* num-lines 16) 10)
        hh (* height 0.5)
        helper (fn [lines color background-color marker]
                 (fill-rect! background-color hww (- wh hh) ww height)
                 (dotimes [i (count lines)]
                   (let [text (nth lines i)
                         text (if marker
                                (str "= " text)
                                text)
                         y (+ (* i 16) (- wh height) 18)]
                     (draw-text! color text 30 y 14))))]
    (cond
      (:text-input world)
      (helper [(str (:text world))] :red :white true)

      (not (empty? (:command world)))
      (helper [(:command world)] :blue :black false)

      :else
      (let [lines (take-last num-lines (split @output #"\n"))]
        (helper lines  :green :black false)))

    (draw-mode-text! world)))

(defn println! [& args]
  (apply gl-println args)
  (let [line (apply print-str (conj (into [] args) "\n"))
        truncated-output (apply str (take-last 1024 @output))]
    (swap! output (fn [output]
                    (str truncated-output line))))
  (draw-buffer! @world)
  nil)

(def user-message! println!)

(defn clear-output! []
  (reset! output "")
  (draw-buffer! @world)
  nil)

(defn toggle-output [world]
  (let [n (if (= (:num-lines world) 1) 6 1)]
    (-> world
        (assoc-in [:num-lines] n)
        (place-elements)
        (redraw))))

(defn toggle-output! []
  (update-thing! [] toggle-output))

