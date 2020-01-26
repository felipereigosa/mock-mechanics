
(ns temp.core)

(do
1

(defn get-bindings []
  {"C-d" #(change-mode % :debug)
   
   "C-i" #(change-mode % :insert)
   ":insert b" #(assoc-in % [:insert-type] :block)
   ":insert w" #(assoc-in % [:insert-type] :wagon)
   ":insert t" #(assoc-in % [:insert-type] :track)
   ":insert c" #(assoc-in % [:insert-type] :chip)
   ":insert m" #(assoc-in % [:insert-type] :cpu)
   ":insert p" #(assoc-in % [:insert-type] :probe)
   ":insert a" #(assoc-in % [:insert-type] :button)
   ":insert s" #(assoc-in % [:insert-type] :sphere)
   ":insert h" #(assoc-in % [:insert-type] :cylinder)
   ":insert v" #(assoc-in % [:insert-type] :cone)

   "C-e" #(change-mode % :edit)
   ":edit d" #(assoc-in % [:edit-subcommand] :delete)
   ":edit s" #(assoc-in % [:edit-subcommand] :scale)
   ":edit m" #(assoc-in % [:edit-subcommand] :move)
   ":edit c" #(assoc-in % [:edit-subcommand] :copy)
   ":edit r" #(assoc-in % [:edit-subcommand] :rotate)
   ":edit y" #(assoc-in % [:edit-subcommand] :sink)

   "C-w" #(change-mode % :graph)
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

   "C-m" #(change-mode % :cpu)
   ":cpu s" #(dissoc-in % [:selected-cpu])
   ":cpu r" #(run-selected-cpu %)
   ":cpu l" #(load-script %)
   ":cpu p" #(print-script-name %)

   "C-c" #(change-mode % :color)
   ":color r" #(assoc-in % [:current-color] :red)
   ":color g" #(assoc-in % [:current-color] :green)
   ":color b" #(assoc-in % [:current-color] :blue)
   ":color y" #(assoc-in % [:current-color] :yellow)
   ":color w" #(assoc-in % [:current-color] :white)
   ":color d" #(assoc-in % [:current-color] :black)

   "C-v" #(change-mode % :set-value)
   "C-t" #(change-mode % :toggle)

   "." #(change-mode % :pivot)
   "C-s" #(change-mode % :idle)

   "C-x r" (fn [w]
             (println! "reset world")
             (create-world))

   "C-x s" #(read-input % save-machine-callback)
   "C-x C-s" #(save-version %)
   "C-x l" #(read-input % load-machine-callback)
   "C-x C-l" #(read-input % load-last-version-callback)

   "C-/" #(undo! %)
   "C-x /" #(redo! %)

   "C-z" #(reset-camera %)
   })

(set-thing! [:bindings] (get-bindings)))

(defn get-key [control-pressed code]
  (if-let [name (get-in keymap [code])]
    (if control-pressed
      (str "C-" name)
      name)
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
        (assoc-in [:end-of-command] true)
        (fun))
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
  (cond
    (in? (:code event) [341 345])
    (assoc-in world [:control-pressed] true)

    (in? (:code event) [340 344])
    (assoc-in world [:shift-pressed] true)

    :else
    (if-let [key (get-key (:control-pressed world) (:code event))]
      (cond
        (= key "C-g")
        (-> world
            (assoc-in [:command] "")
            (assoc-in [:text] "")
            (assoc-in [:text-input] false))

        (:text-input world)
        (text-input-key-pressed world event)

        (string? key)
        (-> world
            (update-in [:command] (fn [c]
                                    (if (or (empty? c)
                                            (:end-of-command world))
                                      key
                                      (str c " " key))))
            (assoc-in [:end-of-command] false)
            (execute-command))

        :else world)
      world)))

(defn key-released [world event]
  (draw-2d! world)

  (cond
    (in? (:code event) [341 345])
    (assoc-in world [:control-pressed] false)

    (in? (:code event) [340 344])
    (assoc-in world [:shift-pressed] false)

    :else
    world))
