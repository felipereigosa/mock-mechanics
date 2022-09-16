(ns mockmechanics.core
  (:require [mockmechanics.library.util :refer :all]
            [mockmechanics.library.xml :refer :all]
            [clojure.string :refer [split]])
  (:import org.apache.batik.transcoder.image.ImageTranscoder
           java.io.FileInputStream
           org.apache.batik.transcoder.image.ImageTranscoder
           org.apache.batik.transcoder.TranscoderInput
           org.apache.batik.transcoder.TranscoderOutput
           org.apache.batik.transcoder.TranscodingHints
           org.apache.batik.anim.dom.SVGDOMImplementation
           org.apache.batik.util.SVGConstants
           java.io.ByteArrayInputStream
           java.awt.image.BufferedImage))

(defn parse-svg-from-map [document width height]
  (let [transcoder-hints (new TranscodingHints)
        result (atom nil)]
    (.put transcoder-hints ImageTranscoder/KEY_XML_PARSER_VALIDATING Boolean/FALSE)
    (.put transcoder-hints ImageTranscoder/KEY_DOM_IMPLEMENTATION
          (SVGDOMImplementation/getDOMImplementation))
    (.put transcoder-hints ImageTranscoder/KEY_DOCUMENT_ELEMENT_NAMESPACE_URI
          SVGConstants/SVG_NAMESPACE_URI)
    (.put transcoder-hints ImageTranscoder/KEY_DOCUMENT_ELEMENT "svg")
    (.put transcoder-hints ImageTranscoder/KEY_WIDTH (float width))
    (.put transcoder-hints ImageTranscoder/KEY_HEIGHT (float height))

    (let [string (xml->str document)
          bais (new ByteArrayInputStream (.getBytes string))
          input (new TranscoderInput bais)
          t (proxy [ImageTranscoder] []
              (createImage [w h]
                (new BufferedImage w h BufferedImage/TYPE_INT_ARGB))

              (writeImage [image out]
                (reset! result image)))]

      (.setTranscodingHints t transcoder-hints)
      (.transcode t input nil)
      @result)))

(defn get-svg-dimensions [document]
  (let [[_ _ w h] (split (get-in document [:attrs :viewBox]) #" ")]
    [(parse-float w) (parse-float h)]))

(defn get-svg-aspect-ratio [document]
  (let [w (get-in document [:attrs :width])
        w (parse-float (subs w 0 (- (count w) 2)))
        h (get-in document [:attrs :height])
        h (parse-float (subs h 0 (- (count h) 2)))]
    (/ h w)))

(defn parse-svg-from-map-with-width [document width]
  (let [aspect-ratio (get-svg-aspect-ratio document)
        height (* aspect-ratio width)]
    (parse-svg-from-map document width height)))

(defn parse-svg-from-map-with-height [document height]
  (let [aspect-ratio (get-svg-aspect-ratio document)
        width (* (/ 1 aspect-ratio) height)]
    (parse-svg-from-map document width height)))

(defn parse-svg-with-width [filename width]
  (let [document (read-xml filename)]
    (parse-svg-from-map-with-width document width)))

(defn parse-hex-color [str]
  [(Integer/parseInt (subs str 0 2) 16)
   (Integer/parseInt (subs str 2 4) 16)
   (Integer/parseInt (subs str 4 6) 16)])

(defn get-svg-regions [document]
  (let [[dx dy] (get-svg-dimensions document)
        g (get-child-if document :g {:inkscape:label "regions"})
        [tx ty] (if-let [translation (get-in g [:attrs :transform])]
                  (let [[tx ty] (split (apply str (butlast (subs translation 10))) #",")]
                    [(parse-float tx) (parse-float ty)])
                  [0 0])
        rects (vec (:content g))]
    (or (apply merge (map (fn [rect]
                            (let [name (keyword (get-in rect [:attrs :id]))
                                  x (get-in rect [:attrs :x])
                                  x (parse-float x)
                                  y (get-in rect [:attrs :y])
                                  y (parse-float y)
                                  w (get-in rect [:attrs :width])
                                  w (parse-float w)
                                  h (get-in rect [:attrs :height])
                                  h (parse-float h)
                                  style (get-in rect [:attrs :style])
                                  fill-index (.indexOf style "fill:")
                                  color (parse-hex-color
                                          (subs style (+ fill-index 6)
                                                (+ fill-index 12)))]
                              {name {:x (/ (+ x tx (/ w 2)) dx)
                                     :y (/ (+ y ty (/ h 2)) dy)
                                     :w (/ w dx)
                                     :h (/ h dy)
                                     :color color}}))
                          rects))
        {})))

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

(defn create-png [base x y w h]
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

(defn create-picture [base x y w h]
  (let [png-filename (str "res/" base ".png")
        reg-filename (str "res/" base ".reg")]
    (if (file-exists? png-filename)
      (let [regions (read-string (slurp reg-filename))
            image (ImageIO/read (new File png-filename))]
        {:x x
         :y y
         :w (get-image-width image)
         :h (get-image-height image)
         :image image
         :regions regions})
      (create-png base x y w h))))
