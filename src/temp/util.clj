
(import java.awt.Color)
(import java.io.File)

(require '[clojure.set :refer [difference union map-invert]])
(require '[clojure.string :refer [split join]])

(def pi Math/PI)
(def e Math/E)

(defn to-radians [angle]
  (Math/toRadians angle))

(defn to-degrees [angle]
  (Math/toDegrees angle))

(defn sin [angle]
  (Math/sin (to-radians angle)))

(defn cos [angle]
  (Math/cos (to-radians angle)))

(defn acos [value]
  (to-degrees (Math/acos value)))

 (defn atan2 [y x]
   (Math/toDegrees (Math/atan2 y x)))

(defn sqrt [x]
  (Math/sqrt x))

(defn abs [x]
  (Math/abs x))

(defn pow [n e]
  (Math/pow n e))

(defn round [n]
  (Math/round (float n)))

(defn cartesian-to-polar [ox oy x y]
  (let [x (- x ox)
        y (- y oy)]
    [(sqrt (+ (* x x) (* y y))) (atan2 y x)]))

(defn within [value min max]
  (cond
    (< value min) min
    (> value max) max
    :else value))

(defn get-arg-names [list]
  (map (fn [n]
         (symbol (str "a" n)))
       (range (count (filter (fn [e] (= e '_)) list)))))

(defn substitute-arg-names [list names]
  (if (empty? list)
    ()
    (if (= (first list) '_)
      (cons (first names) (substitute-arg-names (rest list) (rest names)))
      (cons (first list) (substitute-arg-names (rest list) names)))))

(defmacro slots [fname & args]
  (let [names (into [] (get-arg-names args))]
    `(fn ~names
       (~fname ~@(substitute-arg-names args names)))))

(defn in? [elm seq]
  (some #(= elm %) seq))

(defn rand-range [min max]
  (+ (* (rand) (- max min)) min))

(def colors {:medium-gray (new Color 128 128 128)
             :gray (new Color 128 128 128)
             :orange (new Color 255 102 0)
             :white (new Color 255 255 255)
             :light-gray (new Color 179 179 179)
             :yellow (new Color 255 255 0)
             :green (new Color 0 255 0)
             :dark-red (new Color 128 0 0)
             :dark-yellow (new Color 255 204 0)
             :dark-gray (new Color 51 51 51)
             :red (new Color 255 0 0)
             :blue (new Color 0 0 255)
             :dark-green (new Color 0 145 0)
             :dark-blue (new Color 0 0 128)
             :almost-black (new Color 10 10 10)
             :pink (new Color 255 0 255)
             :teal (new Color 170 212 0)
             :purple (new Color 128 0 175)
             :beige (new Color 170 136 0)
             :black (new Color 0 0 0)})

(defn get-color [name]
  (if (keyword? name)
    (get colors name)
    name))

(defn make-color [r g b]
  (new Color r g b))

(defn get-red [color]
  (.getRed color))

(defn get-green [color]
  (.getGreen color))

(defn get-blue [color]
  (.getBlue color))

(defn get-color-vector [color-name]
  (let [color (get-color color-name)
        r (/ (get-red color) 255.0)
        g (/ (get-green color) 255.0)
        b (/ (get-blue color) 255.0)]
    [r g b 1.0]))

(defn get-dark-color [color]
  (let [color (get-color color)
        amount 0.3
        r (int (* (.getRed color) amount))
        g (int (* (.getGreen color) amount))
        b (int (* (.getBlue color) amount))]
    (new Color r g b)))

(defn color= [a b]
  (let [a (get-color a)
        b (get-color b)]
    (and
     (= (get-red a) (get-red b))
     (= (get-green a) (get-green b))
     (= (get-blue a) (get-blue b)))))

(defn near-zero? [value]
  (< (abs value) 0.001))

(defn =-approx [& values]
  (every? near-zero? (map (fn [v1 v2]
                            (- v1 v2))
                          values
                          (rest values))))
(defn sleep [n]
  (Thread/sleep n))

(defn rotate-list [list]
  (reverse (into () (conj (into [] (rest list)) (first list)))))

(defn rotate-until [list predicate] ;;################## infinite loop
  (if (predicate (first list))
    list
    (rotate-until (rotate-list list) predicate)))

(defn map-between-ranges [value min-a max-a min-b max-b]
  (let [a-size (- max-a min-a)
        b-size (- max-b min-b)
        proportion (/ (- value min-a) a-size)]
    (float (+ (* proportion b-size) min-b))))

(defn gen-keyword [base]
  (keyword (gensym (symbol (subs (str base) 1)))))

(defn find-if [pred coll]
  (first (filter pred coll)))

(defn parse-int [string]
  (Integer/parseInt string))

(defn parse-float [string]
  (try
    (Float/parseFloat string)
    (catch Exception e nil)))

(defn collection-difference [a b]
  (into [] (difference (into #{} a) b)))

(defn create-combinations [& lists]
  (if (= (count lists) 1)
    (map vector (first lists))
    (mapcat (fn [a]
              (map (fn [b]
                     (cons a b))
                   (apply create-combinations (rest lists))))
            (first lists))))

(defn accumulate [list]
  (let [rec (fn [acc lst]
              (if (empty? lst)
                acc
                (recur (conj acc (+ (last acc) (first lst)))
                       (rest lst))))]
    (rec [0] list)))

(defn remove-nil [coll]
  (filter (comp not nil?) coll))

(defn starts-with? [str end]
  (.startsWith str end))

(defn ends-with? [str end]
  (.endsWith str end))

(defn float= [a b]
  (and
   (number? a)
   (number? b)
   (< (abs (- a b)) 0.0001)))

(def not-nil? (comp not nil?))

(defn inside-box? [box x y]
  (let [bx (:x box)
        by (:y box)
        hw (/ (:w box) 2)
        hh (/ (:h box) 2)
        x1 (- bx hw)
        x2 (+ bx hw)
        y1 (- by hh)
        y2 (+ by hh)]
    (and (< x1 x x2)
         (< y1 y y2))))

(defn get-current-time []
  (System/currentTimeMillis))

(defn vector-insert [seq element index]
  (let [before (take index seq)
        after (nthrest seq index)]
    (vec (concat before [element] after))))

(defn vector-remove [seq index]
  (let [before (take index seq)
        after (nthrest seq (inc index))]
    (vec (concat before after))))

(defn kw->str [k]
  (subs (str k) 1))

(defn map-map [func m]
  (apply merge (map func m)))

(defn file-exists? [filename]
  (.exists (clojure.java.io/file filename)))

(defn get-files-at [filename]
  (let [directory (clojure.java.io/file filename)]
    (map #(.getName %)
         (filter #(.isFile %) (file-seq directory)))))

(defn join-keywords [& keywords]
  (keyword (apply str (interpose "-" (map (fn [k]
                                            (subs (str k) 1)) keywords)))))

(defn snap-value [value step]
  (if (number? step)
    (* (round (/ value step)) step)
    (first
     (first
      (sort-by second (map #(list % (abs (- % value))) step))))))

(defn snap-point [[x y]]
  [(snap-value x 10)
   (snap-value y 10)])

(defn keyword->str [k]
  (subs (str k) 1))

(defmacro >> [argument & functions]
  (letfn [(helper [a fs]
            (if (empty? fs)
              a
              (let [[f & fs] fs
                    index (.indexOf f '.)
                    [prefix suffix] (if (= index -1)
                                      [(list (first f)) (rest f)]
                                      [(take index f)
                                       (nthrest f (inc index))])]
                (recur (concat prefix [a] suffix) fs))))]
    (helper argument functions)))

(defn create-groups [acc header? lines]
  (if (empty? lines)
    acc
    (let [line (first lines)]
      (if (header? line)
        (recur (conj acc [line])
               header?
               (rest lines))
        (recur (update-in acc [(dec (count acc))] #(conj % line))
               header?
               (rest lines))))))

(defn dissoc-in [map keys]
  (if (= (count keys) 1)
    (dissoc map (nth keys 0))
    (update-in map (butlast keys) dissoc (last keys))))

(defn make-coordinates [i j]
  (mapcat (fn [x]
            (map (fn [y]
                   [x y])
                 (range i)))
          (range j)))

(defn do-later [func time]
  (.start
   (new Thread
        (proxy [Runnable] []
          (run []
            (try
              (sleep time)
              (func)
              (catch Exception e)))))))

(defn snap-axis [v]
  (let [m (apply max (map abs v))]
    (map (fn [e]
           (if (float= (abs e) m)
             (/ e (abs e))
             0))
         v)))

(defn get-index [elm coll]
  (second (find-if #(= elm (first %))
                   (map vector coll (range (count coll))))))

(defn get-reverse-color [color]
  (first (find-if #(color= color (second %)) colors)))

(declare get-part-collision)
(declare get-pixel-coordinates)
(declare get-pixel)

(defn get-color-at [world spec]
  (let [collision (get-part-collision world spec)
        part-name (:part-name collision)
        part (get-in world [:parts part-name])
        color (if (= (:type part) :display)
                (let [image (get-in part [:texture :image])
                      [px py] (get-pixel-coordinates world spec)
                      c (get-color-vector (get-pixel image (+ 10 (* 20 px)) (+ 10 (* 20 py))))
                      [r g b _] (map #(int (* 255 %)) c)]
                  (new Color r g b))
                (:color part))]
    (get-reverse-color color)))

(defn dekeyword [k]
  (subs (str k) 1))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn interpolate-values [a b t]
  (+ (* a (- 1.0 t)) (* b t)))

(def third #(nth % 2))
(def fourth #(nth % 3))
(def fifth #(nth % 4))

(defn predicate-split [predicate coll]
  (let [m (group-by predicate coll)]
    [(get m true) (get m false)]))

(defn sigmoid [t]
  (/ 1 (+ 1 (pow e (- (* (- t 0.5) 12))))))

(declare vector-interpolate)

(defn interpolate [a b t]
  (if (vector? a)
    (vector-interpolate a b t)
    (interpolate-values a b t)))

(defn interpolate-maps [m1 m2 t]
  (merge-with #(interpolate %1 %2 t) m1 m2))

(defn enumerate [collection]
  (map vector (range) collection))

(defn find-index [pred coll]
  (first (find-if (comp pred second) (enumerate coll))))
