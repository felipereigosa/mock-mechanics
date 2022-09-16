(ns mockmechanics.core
  (:require [clojure.set :refer [difference union map-invert]]
            [mockmechanics.library.vector :as vector]))

(defn create-tab-instructions [writer tab part-name part seen-pins]
  (let [connections (filter #(= (:tab (second %)) tab)
                            (:connections part))]
    (when (not (empty? connections))
      (let [elements (->> connections
                          (map (comp :points second))
                          (apply concat)
                          (filter keyword?)
                          (distinct))
            [gate-names pin-names] (predicate-split
                                     #(.startsWith (str %) ":gate")
                                     elements)
            pin-names (vec (difference (into #{} pin-names) seen-pins))
            dekeyword (fn [k] (symbol (subs (str k) 1)))
            pins (sort-by second <
                          (map (fn [pin-name]
                                 (let [pin (get-in part [:pins pin-name])
                                       pin-name (dekeyword pin-name)]
                                   (if (:trigger pin)
                                     [pin-name (:x pin) true]
                                     [pin-name (:x pin)])))
                               pin-names))
            gates (map (fn [gate-name]
                         (let [gate (get-in part [:gates gate-name])]
                           [(symbol (subs (str gate-name) 6))
                            (:x gate) (:y gate)]))
                       gate-names)
            connections (map (fn [[connection-name connection]]
                               (let [connection-name (dekeyword connection-name)
                                     points (map #(if (keyword? %)
                                                    (dekeyword %)
                                                    %) (:points connection))]
                                 (vec (concat [connection-name] points))))
                             connections)]
        (.write writer (format "set %s tab %s %s %s %s\n"
                               (dekeyword part-name)
                               tab
                               (join " " (map str pins))
                               (join " " (map str gates))
                               (join " " (map str connections))))
        pin-names))))

(defn create-motherboard-instructions [writer part-name part]
  (loop [n 0
         seen-pins []]
    (when (< n 5)
      (let [pins (create-tab-instructions
                   writer n part-name part seen-pins)]
        (recur (inc n) (concat seen-pins pins))))))

(defn get-descaled-relative-transform [world part-name change]
  (let [parent-name (get-parent-part world part-name)
        part (get-in world [:parts part-name])
        offset (case (:type part)
                 :sphere [0 0 0]
                 :track change
                 (:cylinder :cone) [0 (* (second change) 0.5) 0]
                 (vector/multiply change 0.5))
        offset (vector/multiply offset -1)
        offset-transform (make-transform offset [1 0 0 0])
        relative-transform (get-in world [:parts parent-name
                                          :children part-name])]
    (combine-transforms offset-transform relative-transform)))

(defn create-part-instructions [writer world part-name]
  (let [parent-name (get-parent-part world part-name)
        parent (get-in world [:parts parent-name])
        part (get-in world [:parts part-name])
        type (:type part)
        properties (keys (get-in world [:info type :properties]))
        color (get-in world [:info type :color])
        new-part (create-part type color 0 (:info world))
        scale-change (vector/subtract (:scale part) (:scale new-part))
        property= (fn [a b]
                    (if (number? a)
                      (float= a b)
                      (= a b)))
        get-scales (fn [type scale]
                     (filter #(not (vector/equal? % [0 0 0]))
                             (let [[x y z] scale]
                               (case type
                                 (:sphere :track) [scale]
                                 (:cylinder :cone) [[x 0 z] [0 y 0]]
                                 [[x 0 0] [0 y 0] [0 0 z]]))))]
    (cond
      (= type :wagon)
      (.write writer (format "add part %s to %s at [0 0 0] [1 0 0 0] %s\n"
                             (dekeyword part-name)
                             (dekeyword parent-name)
                             (* (:value part)
                                (reduce + (:track-lengths part)))))
      (and
        (in? type [:block :cylinder :cone])
        (= (:type parent) :track))
      (.write writer (format "add part %s to %s at [0 0.25 0] [1 0 0 0]\n"
                             (dekeyword part-name)
                             (dekeyword parent-name)))
      :else
      (let [relative-transform (get-descaled-relative-transform
                                 world part-name scale-change)
            position (get-transform-position relative-transform)
            rotation (get-transform-rotation relative-transform)]
        (.write writer (format "add part %s to %s at %s %s\n"
                               (dekeyword part-name)
                               (dekeyword parent-name)
                               position
                               rotation))))

    (doseq [s (get-scales type scale-change)]
      (.write writer (format "scale %s by + %s\n"
                             (dekeyword part-name) s)))

    (when (and (in? type [:block :cylinder :cone])
               (= (:type parent) :track))
      (let [relative-transform (get-in parent [:children part-name])]
        (.write writer (format "move %s to %s\n"
                               (dekeyword part-name)
                               (get-transform-position relative-transform)))))

    (doseq [property properties]
      (when (and
              (not (property= (get part property) (get new-part property)))
              (not (and
                     (in? type [:probe :wagon])
                     (= property :value))))
        (.write writer (format "set property %s of %s to %s\n"
                               (dekeyword property)
                               (dekeyword part-name)
                               (get part property)))))

    (let [old-color (get-reverse-color (:color part))]
      (when (not= color old-color)
        (.write writer (format "set color of %s to %s\n"
                               (dekeyword part-name)
                               (dekeyword old-color)))))

    (when (= type :chip)
      (doseq [[function-name function] (:functions part)]
        (let [{:keys [points relative]} function]
          (.write writer (format "set %s function %s %s %s\n"
                                 (dekeyword part-name)
                                 (dekeyword function-name)
                                 points relative)))))

    (when (= type :motherboard)
      (create-motherboard-instructions writer part-name part))
    ))

(defn get-part-number [part-name]
  (parse-int (last (re-seq #"\d+" (str part-name)))))

(defn get-sorted-part-names [world]
  (letfn [(helper [part-name]
            (let [children
                  (->> (get-in world [:parts part-name :children])
                       (keys)
                       (sort-by get-part-number))]
              (if (empty? children)
                part-name
                [part-name (map helper children)])))]
    (flatten (helper :ground-part))))

(defn create-instructions [world filename]
  (let [sorted-names (rest (get-sorted-part-names world))
        world (reduce (fn [w part-name]
                        (set-value-0-transform w part-name))
                      world
                      sorted-names)
        filename (str "replayer/" filename ".txt")]
    (create-directory! "replayer")
    (with-open [writer (clojure.java.io/writer filename)]
      (doseq [part-name sorted-names]
        (create-part-instructions writer world part-name)))

    (println "created instructions:" filename)
    world))
