
(ns temp.core)

(do
1

(defn get-bindings []
  {"C-d" #(change-mode % :debug)
   
   "C-a" #(change-mode % :insert)
   ":insert b" #(assoc-in % [:insert-type] :block)
   ":insert w" #(assoc-in % [:insert-type] :wagon)
   ":insert t" #(assoc-in % [:insert-type] :track)
   ":insert g" #(assoc-in % [:insert-type] :chip)
   ":insert c" #(assoc-in % [:insert-type] :cpu)
   ":insert p" #(assoc-in % [:insert-type] :probe)
   ":insert a" #(assoc-in % [:insert-type] :button)
   ":insert s" #(assoc-in % [:insert-type] :sphere)
   ":insert h" #(assoc-in % [:insert-type] :cylinder)
   ":insert v" #(assoc-in % [:insert-type] :cone)
   ":insert l" #(assoc-in % [:insert-type] :lamp)

   "C-e" #(change-mode % :edit)
   ":edit d" #(assoc-in % [:edit-subcommand] :delete)
   ":edit s" #(assoc-in % [:edit-subcommand] :scale)
   ":edit m" #(assoc-in % [:edit-subcommand] :move)
   ":edit t" #(assoc-in % [:edit-subcommand] :translate)
   ":edit c" #(assoc-in % [:edit-subcommand] :copy)
   ":edit r" #(assoc-in % [:edit-subcommand] :rotate)
   ":edit y" #(assoc-in % [:edit-subcommand] :sink)          

   "C-g" #(change-mode % :graph)
   ":graph m" #(assoc-in % [:graph-subcommand] :move)
   ":graph x" #(assoc-in % [:graph-subcommand] :set-x)
   ":graph y" #(assoc-in % [:graph-subcommand] :set-y)
   ":graph C-c x" #(assoc-in % [:graph-subcommand] :set-both)
   ":graph a" #(assoc-in % [:graph-subcommand] :add)
   ":graph d" #(assoc-in % [:graph-subcommand] :delete)
   ":graph r" #(run-selected-chip %)
   ":graph s" #(dissoc-in % [:selected-chip])
   ":graph t" #(assoc-in % [:graph-subcommand] :toggle-relative)
   ":graph 1" #(reset-graph-view %)
   ":graph C-c s" #(set-snap-value %)
   ":graph l" #(assoc-in % [:graph-subcommand] :print-lengths)

   "C-q" #(change-mode % :cpu)
   ":cpu s" #(dissoc-in % [:selected-cpu])
   ":cpu a" #(assoc-in % [:cpu-subcommand] :and)
   ":cpu o" #(assoc-in % [:cpu-subcommand] :or)
   ":cpu n" #(assoc-in % [:cpu-subcommand] :not)
   ":cpu d" #(assoc-in % [:cpu-subcommand] :delete)
   ":cpu c" #(assoc-in % [:cpu-subcommand] :connect)
   ":cpu t" #(assoc-in % [:cpu-subcommand] :toggle)
   ":cpu r" #(assoc-in % [:cpu-subcommand] :run)

   "C-c" #(change-mode % :color)
   ":color r" #(assoc-in % [:current-color] :red)
   ":color g" #(assoc-in % [:current-color] :green)
   ":color b" #(assoc-in % [:current-color] :blue)
   ":color y" #(assoc-in % [:current-color] :yellow)
   ":color w" #(assoc-in % [:current-color] :white)
   ":color d" #(assoc-in % [:current-color] :black)

   "C-l" #(change-mode % :layer)
   ":layer 1" #(set-layer % 1)
   ":layer 2" #(set-layer % 2)
   ":layer 3" #(set-layer % 3)
   ":layer 4" #(set-layer % 4)
   ":layer 5" #(set-layer % 5)
   ":layer 6" #(set-layer % 6)
   ":layer 7" #(set-layer % 7)
   ":layer 8" #(set-layer % 8)

   "C-v" #(change-mode % :set-value)
   "C-t" #(change-mode % :toggle)
   "C-s" #(change-mode % :idle)

   "A-n" #(new-file %)
   "A-c" #(reset-camera %)
   "A-s" #(save-version %)
   "C-x s" #(read-input % save-machine-callback)
   "A-l" #(read-input % load-last-version-callback)
   "A-left" #(undo! %)
   "A-right" #(redo! %)

   })

(set-thing! [:bindings] (get-bindings)))

(defn get-key [code control-pressed alt-pressed]
  (if-let [name (get-in keymap [code])]
    (let [name (if (keyword? name)
                 (subs (str name) 1)
                 name)]
      (cond
        control-pressed (str "C-" name)
        alt-pressed (str "A-" name)
        :else name))
    nil))

(defn find-binding [bindings command mode]
  (if-let [fun (get bindings command)]
    fun
    (second (find-if (fn [[keys fun]]
                      (let [mode (str mode)]
                        (and (.startsWith keys mode)
                             (.equals (subs keys (inc (count mode))) command))))
                    bindings))))

(defn execute-command [world]
  (if-let [fun (find-binding (:bindings world)
                             (:command world)
                             (:mode world))]
    (-> world
        (fun)
        (assoc-in [:command] "")
        (prepare-tree)) 
    world))

(defn text-input-key-pressed [world event]
  (let [key (get-in keymap [(:code event)])]
    (case key
      :enter
      (let [callback (:input-callback world)
            world (try
                    (callback world (:text world))
                    (catch Exception e
                      (do
                        (println! "invalid input:" (:text world))
                        world)))
            world (-> world
                      (dissoc-in [:text])
                      (assoc-in [:text-input] false))]
        (save-checkpoint! world))

      :backspace
      (if (empty? (:text world))
        world
        (update-in world [:text] #(apply str (butlast %))))

      (update-in world [:text]
                 (fn [text]
                   (apply str (concat text key)))))))

(defn key-pressed [world event]
  (let [key-name (get-in keymap [(:code event)])]
    (cond
        (= key-name :control) (assoc-in world [:control-pressed] true)
        (= key-name :shift) (assoc-in world [:shift-pressed] true)
        (= key-name :alt) (assoc-in world [:alt-pressed] true)

        (= key-name :esc)
        (do
          (reset! output "")
          (-> world
              (assoc-in [:command] "")
              (assoc-in [:graph-subcommand] :move)
              (assoc-in [:cpu-subcommand] :move)
              (assoc-in [:text] "")
              (assoc-in [:text-input] false)))

        :else
        (if-let [key (get-key (:code event)
                              (:control-pressed world)
                              (:alt-pressed world))]
          (cond
            (:text-input world)
            (text-input-key-pressed world event)

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
        world (redraw world)]
    (cond
      (= key-name :control) (assoc-in world [:control-pressed] false)
      (= key-name :shift) (assoc-in world [:shift-pressed] false)
      (= key-name :alt) (assoc-in world [:alt-pressed] false)
      :else world)))
