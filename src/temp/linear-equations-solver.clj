
(defn get-row [matrix n]
  (nth matrix n))

(defn get-element [row n]
  (nth row n))

(defn multiply-row [matrix row value]
  (update-in matrix [row] (fn [r]
                            (into [] (map (partial * value) r)))))

(defn subtract-rows [matrix a b]
  (update-in matrix [a] (fn [r]
                          (let [other (get-row matrix b)]
                            (into [] (map - r other))))))

(defn pivot [row]
  (first (keep-indexed (fn [i v]
                         (if (not (zero? v))
                           i))
                       row)))

(defn reorder-rows [matrix]
  (into [] (sort-by pivot matrix)))

(defn opportunity-index [matrix]
  (letfn [(rec [pivots index]
            (if (= (count pivots) 1)
              nil
              (if (= (first pivots) (second pivots))
                index
                (rec (rest pivots) (inc index)))))]
    (rec (map pivot matrix) 0)))

(defn normalize-row [matrix row-index]
  (let [row (get-row matrix row-index)
        leading (get-element row (pivot row))]
    (multiply-row matrix row-index (/ 1 leading))))

(defn solve-row [matrix a b]
  (if (= (pivot (get-row matrix a))
         (pivot (get-row matrix b)))
    (let [matrix (normalize-row matrix a)
          matrix (normalize-row matrix b)]
      (subtract-rows matrix b a))
    matrix))

(defn solve-step [matrix]
  (let [matrix (reorder-rows matrix)
        index (opportunity-index matrix)]
    (if (nil? index)
      matrix
      (solve-row matrix index (inc index)))))

(defn flip-matrix [matrix]
  (into [] (reverse (map (fn [row]
                  (into [] (reverse (cons (last row) (butlast row)))))
                matrix))))

(defn solve-forward [matrix]
  (let [m (solve-step matrix)]
    (if (= m matrix)
      matrix
      (recur m))))

(defn get-coordinates [n]
  (let [max 4]
    (mapcat (fn [x]
              (map (fn [y]
                     [x y]) (range (inc x) n))) (range n))))

(defn solve [matrix]
  (let [max (dec (count matrix))
        forward (flip-matrix (normalize-row (solve-forward matrix) max))
        rows (get-coordinates (count matrix))

        backward (normalize-row (reduce (fn [m [a b]]
                                           (solve-row m a b))
                                         forward
                                         rows) max)
        solution (reorder-rows backward)
        ]
    (into [] (reverse (map last solution)))))
