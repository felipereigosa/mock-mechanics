
(ns temp.core (:gen-class))

(defn create-input-indicator [world y]
  (let [x 105 
        w 200]
    (assoc-in world [:input-indicator]
              {:picture (create-picture "indicator" x y w -1)
               :left (:image (create-picture "left-button" x y w -1))
               :middle (:image (create-picture "middle-button" x y w -1))
               :right (:image (create-picture "right-button" x y w -1))
               :up (:image (create-picture "up-button" x y w -1))
               :down (:image (create-picture "down-button" x y w -1))
               :text ""
               :button nil})))

(defn draw-input-indicator! [world]
  (if-let [input-indicator (:input-indicator world)]
    (let [picture (:picture input-indicator)
          {:keys [image x y]} picture
          text-region (get-absolute-region
                       (get-in picture [:regions :text]) picture)]
      (draw-image! image x y)
      (if-let [button (:button input-indicator)]
        (draw-image! (get input-indicator button) x y))
      (draw-text-in-box! (:text input-indicator) :white 20 text-region))))

(defn set-indicator-text [world text]
  (do-later (fn []
              (set-thing! [:input-indicator :text] "")
              (redraw!)) 1000)
  (assoc-in world [:input-indicator :text] text))

(defn pretty-key [text]
  (if (= (count text) 3)
    (let [modifier (case (first text)
                     \C "Ctrl"
                     \S "Shift"
                     \A "Alt")]
      (str modifier " + " (.toUpperCase (subs text 2))))
    (.toUpperCase text)))

(defn input-indicator-key-pressed [world event]
  (if-let [input-indicator (:input-indicator world)]
    (let [key-name (get-in keymap [(:code event)])]
      (cond
        (= key-name :control) (assoc-in world [:input-indicator :text] "Ctrl")
        (= key-name :shift) (assoc-in world [:input-indicator :text] "Shift")
        (= key-name :alt) (assoc-in world [:input-indicator :text] "Alt")
        (= key-name :esc) (set-indicator-text world "Esc")

        :else
        (if-let [key (get-key (:code event)
                              (:control-pressed world)
                              (:alt-pressed world)
                              (:shift-pressed world))]
            (if (not (:text-input world))
              (set-indicator-text world (pretty-key key))
              world)
            world)))
    world))

(defn input-indicator-key-released [world event]
  (if-let [input-indicator (:input-indicator world)]
    (let [text (get-in world [:input-indicator :text])]
      (if (in? text ["Ctrl" "Shift" "Alt"])
        (assoc-in world [:input-indicator :text] "")
        world))
    world))

(defn input-indicator-mouse-scrolled [world event]
  (if-let [input-indicator (:input-indicator world)]
    (let [world (assoc-in world [:input-indicator :button]
                          (if (pos? (:amount event))
                            :up
                            :down))]
      (do-later (fn []
                  (set-thing! [:input-indicator :button] nil)
                  (redraw!)) 300)
      world)
    world))

(defn input-indicator-mouse-pressed [world event]
  (if-let [input-indicator (:input-indicator world)]
    (assoc-in world [:input-indicator :button] (:button event))
    world))

(defn input-indicator-mouse-released [world event]
  (if-let [input-indicator (:input-indicator world)]
    (dissoc-in world [:input-indicator :button])
    world))
