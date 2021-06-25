
(defn export-path [points filename]
  (let [points (read-string (str "(" points ")"))
        final-time (last (last points))
        w @window-width
        h @window-height
        points (concat [[w 0 0] [w h 0] [0 h 0]] points)
        ratio (float (/ w h))]
    (with-open [writer (clojure.java.io/writer filename)]
      (.write writer (format "# final-time %s\n" final-time))
      (.write writer (format "# width %s\n" w))
      (.write writer "o Path\n")
      (doseq [[x y t] points]
        (.write writer (format "v %s %s %s\n"
                               (/ x (float w))
                               (/ t (float final-time))
                               (/ y (float w))
                               )))
      (doseq [i (range 1 (count points))]
        (if (not= i 3)
          (.write writer (format "f %s %s\n" i (inc i))))))))

(defn import-path [filename]
  (let [lines (read-lines filename)
        [_ [_ _ ratio] _ & vertices]
        (map parse-line
             (filter (fn [line]
                       (.startsWith line "v "))
                     lines))

        vertices (sort-by second vertices)
        final-time (>> lines
                       (find-if #(.startsWith % "# final-time") .)
                       (subs .  13)
                       (read-string .))
        width (>> lines
                  (find-if #(.startsWith % "# width") .)
                  (subs .  8)
                  (read-string .))]
    (println!
     (map (fn [[x y z]]
            [(round (* x width))
             (round (* z width))
             (round (* y final-time))])
          vertices))))
