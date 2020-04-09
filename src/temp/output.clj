
(ns temp.core)

(def output (atom ""))

(defn draw-buffer! [world]
  (let [y (- (:window-height world) 12)
        helper (fn [text marker color]
                 (if marker
                   (do
                     (fill-rect! :white 80 y 5000 25)
                     (draw-text! :black "=" 15 (+ y 4) 14))
                   (fill-rect! :black 80 y 5000 25))                 
                 (draw-text! color text 30 (+ y 4) 14))]
    (cond
      (:text-input world)
      (helper (str (:text world)) true :black)

      (not (empty? (:command world)))
      (helper (:command world) false :blue)

      :else
      (helper (last (split @output #"\n")) false :green))))

(defn println! [& args]
  (apply gl-println args)
  (let [line (apply print-str (conj (into [] args) "\n"))
        truncated-output (apply str (take-last 1024 @output))]
    (swap! output (fn [output]
                    (str truncated-output line))))
  (draw-buffer! @world))

(defn clear-output! []
  (reset! output "")
  (draw-buffer! @world)
  nil)


