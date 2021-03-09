
[wagon image]

(defn get-segments-pattern [& segments]
  (let [blank (vec (repeat 7 [0 0 0 0]))
        coords {0 [[1 0] [2 0]] 1 [[0 1] [0 2]]
                2 [[3 1] [3 2]] 3 [[1 3] [2 3]]
                4 [[0 4] [0 5]] 5 [[3 4] [3 5]]
                6 [[1 6] [2 6]]}]
    (reduce (fn [b [x y]]
              (assoc-in b [y x] 1))
            blank
            (mapcat #(get coords %) segments))))

(defn get-number-pattern [number]
  (let [segments {0 [0 1 2 4 5 6] 1 [2 5] 2 [0 2 3 4 6] 3 [0 2 3 5 6]
                  4 [1 2 3 5] 5 [0 1 3 5 6] 6 [0 1 3 4 5 6] 7 [0 2 5]
                  8 [0 1 2 3 4 5 6] 9 [0 1 2 3 5]}]
    (apply get-segments-pattern (get segments number))))

(defn draw-number [number]
  (draw-pattern image (get-number-pattern number) 0 0 :blue))

(fn [part-name]
  (clear-display image :black)
  (draw-number (round (* 10 (get-value part-name)))))
