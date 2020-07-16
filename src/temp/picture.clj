
(ns temp.core (:gen-class))

(defn get-absolute-region [region box]
  (let [bx (:x box)
        by (:y box)
        bw (:w box)
        bh (:h box)
        {:keys [x y w h]} region]
    (-> region
        (assoc-in [:x] (+ (* x bw) bx (- (/ bw 2))))
        (assoc-in [:y] (+ (* y bh) by (- (/ bh 2))))
        (assoc-in [:w] (* w bw))
        (assoc-in [:h] (* h bh)))))

(defn get-region-at [picture x y]
  (first (find-if (fn [[name region]]
                    (inside-box? (get-absolute-region
                                  region picture) x y))
                  (:regions picture))))

(defn create-picture [base x y w h]
  (let [svg-filename (str "res/" base ".svg")
        document (read-xml svg-filename)
        image (if (= w -1)
                (parse-svg-from-map-with-height document h)
                (parse-svg-from-map-with-width document w))
        picture {:x x
                 :y y
                 :w (get-image-width image)
                 :h (get-image-height image)
                 :image image}
        regions (get-svg-regions document)
        png-filename (str "res/" base ".png")
        reg-filename (str "res/" base ".reg")]
    (spit reg-filename regions)
    (ImageIO/write image "png" (new File png-filename))
    (assoc-in picture [:regions] regions)))

;; (defn create-picture [base x y _ _]
;;   (println! "created png from" base)
;;   (let [png-filename (str "res/" base ".png")
;;         reg-filename (str "res/" base ".reg")
;;         regions (read-string (slurp reg-filename))
;;         image (ImageIO/read (new File png-filename))]
;;     {:x x
;;      :y y
;;      :w (get-image-width image)
;;      :h (get-image-height image)
;;      :image image
;;      :regions regions}))
