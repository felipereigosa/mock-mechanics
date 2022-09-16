(ns mockmechanics.core
  (:require [mockmechanics.library.vector :as vector]))

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
    (println
      (map (fn [[x y z]]
             [(round (* x width))
              (round (* z width))
              (round (* y final-time))])
           vertices))))

(defn create-extended! [source destination]
  (let [source (str "replayer/" source ".txt")
        destination (str "replayer/" destination ".txt")]
    (with-open [writer (clojure.java.io/writer destination)]
      (let [instructions (-> source
                             (read-lines)
                             (extend-instructions))]
        (doseq [instruction instructions]
          (.write writer instruction)
          (.write writer "\n"))))))

(defn print-part-names [filename]
  (let [types ["sphere" "wagon" "block" "button" "gear" "chip"
               "motherboard" "cylinder" "probe" "cone" "lamp"
               "display" "rack" "speaker" "track" "gate" "connection"]
        get-type (fn [word]
                   (second (re-find #"([a-z]*).*?" (str word))))
        names (>> (str "replayer/" filename ".txt")
                  (slurp)
                  (str "[" . "]")
                  (read-string)
                  (flatten)
                  (filter #(in? (get-type %) types) .)
                  (distinct))]
    (println "{")
    (doseq [name names]
      (if (.startsWith (str name) "gate")
        (println (keyword name) "foobar")
        (println (keyword name) (keyword (get-type name)))))
    (println "}")))

(defn get-camera-vector [world]
  (let [camera (:camera world)]
    [(:pivot camera)
     [(:x-angle camera) (:y-angle camera)]
     (:distance camera)]))

(defn print-camera-instruction! []
  (let [[pivot angles distance] (get-camera-vector @world)]
    (println "set camera"
             "pivot" pivot
             "angles" angles
             "distance" distance)))

(defn check-replay! [world]
  (let [filename (get-last-version-filename (:replay-filename world))
        parts (:parts (read-string (slurp filename)))]
    (doseq [part-name (keys parts)]
      (if (not= part-name :ground-part)
        (if-let [new-part (get-in world [:parts part-name])]
          (let [parent-name (get-parent-part world part-name)
                transform (get-in world [:parts parent-name
                                         :children part-name])
                target-transform (get-in parts [parent-name
                                                :children part-name])]
            (if (not
                  (and
                    (vector/equal? (:position target-transform)
                                    (get-transform-position transform))
                    (vector/equal? (:rotation target-transform)
                                    (get-transform-rotation transform))))
              (println part-name "wrong relative transform")))
          (println part-name "missing"))))
    (println "check done")))
