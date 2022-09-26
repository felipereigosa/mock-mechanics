
(ns mockmechanics.core
  (:require [mockmechanics.library.keymap :refer [keymap shift-keymap]]))

(declare print-camera-instruction!)

(defn get-bindings []
  {"A-d" #(change-mode % :debug)
   "A-q" #(update-in % [:draw-update-cube] not)
   "A-x" read-and-execute-command
   "A-r" (fn [w]
           (println "reset world")
           (create-world))

   "C-x c" (fn [w]
             (print-camera-instruction!)
             w)

   "C-x m" #(toggle-mouse-recording %)

   "right" #(replay-forward %)
   "C-right" #(toggle-run-instructions %)
   "left" #(replay-back %)
   "C-left" #(replay-back %)
   "up" #(replay-up %)
   "down" #(replay-down %)

   "A-a" #(change-mode % :add)
   ":add b" #(assoc-in % [:add-type] :block)
   ":add c" #(assoc-in % [:add-type] :cylinder)
   ":add S-c" #(assoc-in % [:add-type] :cone)
   ":add s" #(assoc-in % [:add-type] :sphere)
   ":add w" #(assoc-in % [:add-type] :wagon)
   ":add t" #(assoc-in % [:add-type] :track)
   ":add g" #(assoc-in % [:add-type] :chip)
   ":add m" #(assoc-in % [:add-type] :motherboard)
   ":add p" #(assoc-in % [:add-type] :probe)
   ":add S-b" #(assoc-in % [:add-type] :button)
   ":add l" #(assoc-in % [:add-type] :lamp)
   ":add S-s" #(assoc-in % [:add-type] :speaker)
   ":add S-g" #(assoc-in % [:add-type] :gear)
   ":add i" #(assoc-in % [:add-type] :display)
   ":add r" #(assoc-in % [:add-type] :cable)

   "A-e" #(change-mode % :edit)
   ":edit d" #(assoc-in % [:edit-subcommand] :delete)
   ":edit s" #(assoc-in % [:edit-subcommand] :scale)
   ":edit m" #(assoc-in % [:edit-subcommand] :move)
   ":edit t" #(assoc-in % [:edit-subcommand] :translate)
   ":edit c" #(assoc-in % [:edit-subcommand] :copy)
   ":edit r" #(assoc-in % [:edit-subcommand] :rotate)
   ":edit h" #(assoc-in % [:edit-subcommand] :sink)

   "A-g" #(change-mode % :graph)
   ":graph m" #(assoc-in % [:graph-subcommand] :move)
   ":graph x" #(assoc-in % [:graph-subcommand] :set-x)
   ":graph y" #(assoc-in % [:graph-subcommand] :set-y)
   ":graph a" #(assoc-in % [:graph-subcommand] :add)
   ":graph d" #(assoc-in % [:graph-subcommand] :delete)
   ":graph r" #(run-selected-chip %)
   ":graph t" #(assoc-in % [:graph-subcommand] :toggle-relative)
   ":graph v" #(reset-graph-view %)
   ":graph l" #(assoc-in % [:graph-subcommand] :print-lengths)

   "A-m" #(change-mode % :motherboard)
   ":motherboard s" #(toggle-script %)
   ":motherboard m" #(assoc-in % [:motherboard-subcommand] :move)
   ":motherboard a" #(assoc-in % [:motherboard-subcommand] :and)
   ":motherboard o" #(assoc-in % [:motherboard-subcommand] :or)
   ":motherboard n" #(assoc-in % [:motherboard-subcommand] :not)
   ":motherboard d" #(assoc-in % [:motherboard-subcommand] :delete)
   ":motherboard c" #(assoc-in % [:motherboard-subcommand] :connect)
   ":motherboard t" #(assoc-in % [:motherboard-subcommand] :toggle)
   ":motherboard r" #(assoc-in % [:motherboard-subcommand] :run)
   ":motherboard e" #(open-editor %)

   "A-c" #(change-mode % :color)
   ":color 0" #(assoc-in % [:current-color] :black)
   ":color 1" #(assoc-in % [:current-color] :almost-black)
   ":color 2" #(assoc-in % [:current-color] :dark-gray)
   ":color 3" #(assoc-in % [:current-color] :medium-gray)
   ":color 4" #(assoc-in % [:current-color] :light-gray)
   ":color 5" #(assoc-in % [:current-color] :white)
   ":color w" #(assoc-in % [:current-color] :white)
   ":color S-r" #(assoc-in % [:current-color] :dark-red)
   ":color r" #(assoc-in % [:current-color] :red)
   ":color o" #(assoc-in % [:current-color] :orange)
   ":color S-y" #(assoc-in % [:current-color] :dark-yellow)
   ":color y" #(assoc-in % [:current-color] :yellow)
   ":color S-g" #(assoc-in % [:current-color] :dark-green)
   ":color g" #(assoc-in % [:current-color] :green)
   ":color S-b" #(assoc-in % [:current-color] :dark-blue)
   ":color b" #(assoc-in % [:current-color] :blue)
   ":color S-p" #(assoc-in % [:current-color] :purple)
   ":color p" #(assoc-in % [:current-color] :pink)

   "A-l" #(change-mode % :layer)

   "C-x p" #(change-mode % :physics)
   "A-p" #(change-mode % :property)

   "A-f" #(change-mode % :avatar)

   "A-s" #(change-mode % :simulation)
   ":simulation o" #(assoc-in % [:camera-rotating] -1.5)
   ":simulation u" #(assoc-in % [:camera-rotating] 1.5)
   ":simulation i" #(assoc-in % [:camera-rotating] nil)

   "C-n" #(-> %
              (new-file)
              (tree-changed))

   "C-c" #(reset-camera %)

   "C-s" #(save-machine-version %)
   "C-x s" #(-> %
                (dissoc-in [:last-saved-machine])
                (save-machine-version))
   "C-o" #(open-machine-version %)
   "C-x o" #(import-machine-version %)

   "C-z" #(undo! %)
   "C-r" #(redo! %)

   "A-o" #(-> %
              (update-in [:num-lines] (fn [n] (if (= n 6) 1 6)))
              place-elements)
   })

(defn get-key [code control-pressed alt-pressed shift-pressed]
  (if-let [name (get-in keymap [code])]
    (let [name (if (keyword? name)
                 (subs (str name) 1)
                 name)]
      (cond
        control-pressed (str "C-" name)
        alt-pressed (str "A-" name)
        shift-pressed (str "S-" name)
        :else name))
    nil))

(defn find-binding [bindings command mode]
  (if-let [fun (get bindings command)]
    fun
    (second (find-if (fn [[keys fun]]
                       (let [mode (str mode)]
                         (and (.startsWith keys mode)
                              (.equals (subs keys (inc (count mode)))
                                       command))))
                     bindings))))

(defn execute-command [world]
  (if-let [fun (find-binding (:bindings world)
                             (:command world)
                             (:mode world))]
    (-> world
        (fun)
        (assoc-in [:command] ""))
    world))

(defn text-input-key-pressed [world event]
  (let [km (if (:shift-pressed world) shift-keymap keymap)
        key (get-in km [(:code event)])]
    (case key
      :enter
      (let [callback (:input-callback world)
            world (try
                    (callback world (:text world))
                    (catch Exception e
                      (do
                        (user-message! "invalid input:" (:text world))
                        world)))]
        (-> world
            (dissoc-in [:text])
            (assoc-in [:text-input] false)))

      :backspace
      (if (empty? (:text world))
        world
        (update-in world [:text] #(apply str (butlast %))))

      (update-in world [:text]
                 (fn [text]
                   (apply str (concat text key)))))))

(defn cancel-action [world]
  (reset! output "")
  (-> world
      (assoc-in [:command] "")
      (assoc-in [:graph-subcommand] :move)
      (assoc-in [:motherboard-subcommand] :move)
      (assoc-in [:text] "")
      (assoc-in [:text-input] false)
      (dissoc-in [:first-gear-part])))

(declare input-indicator-key-pressed)
(declare input-indicator-key-released)

(defn key-pressed [world event]
  (let [key-name (get-in keymap [(:code event)])
        world (-> world
                  (input-indicator-key-pressed event)
                  (redraw))]
    (cond
      (= key-name :delete) (do
                             (clear-output!)
                             world)
      (= key-name :control) (assoc-in world [:control-pressed] true)
      (= key-name :shift) (assoc-in world [:shift-pressed] true)
      (= key-name :alt) (assoc-in world [:alt-pressed] true)
      (= key-name :esc) (cancel-action world)

      :else
      (if-let [key (get-key (:code event)
                            (:control-pressed world)
                            (:alt-pressed world)
                            (:shift-pressed world))]
        (cond
          (:text-input world)
          (text-input-key-pressed world event)

          (and
            (= (:mode world) :avatar)
            (avatar-key? key)
            (empty? (:command world)))
          (avatar-key-pressed world key)

          (string? key)
          (-> world
              (update-in [:command] (fn [c]
                                      (if (empty? c)
                                        key
                                        (str c " " key))))
              (execute-command)))
        world))))

(defn key-released [world event]
  (let [key-name (get-in keymap [(:code event)])
        world (-> world
                  (input-indicator-key-released event)
                  (redraw))]
    (cond
      (= key-name :control) (assoc-in world [:control-pressed] false)
      (= key-name :shift) (assoc-in world [:shift-pressed] false)
      (= key-name :alt) (assoc-in world [:alt-pressed] false)

      (and
        (= (:mode world) :avatar)
        (avatar-key? key-name))
      (avatar-key-released world key-name)

      :else world)))
