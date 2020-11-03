
[load-button reset-button corner sheet]

(fn [part-name]
  (when (on? load-button)
    (with-open [reader (clojure.java.io/reader "machines/kokiri.txt")]
      (let [[point direction] (probe->line corner)
            lines (line-seq reader)
            song (map (fn [time line]
                        [time (read-string (str "[" line "]"))])
                      (range) lines)]
        (set-thing! [:add-type] :probe)
        (doseq [[time notes] song]
          (doseq [note notes]
            (let [offset (vector-add [0.2 -3.1 1] [(* note 0.25) (* time 0.25) 0])
                  corner-line [(vector-add point offset) direction]]
              (mode-click! :add corner-line [])))))))
  (when (on? reset-button)
    (update-thing! [] #(reduce (fn [w note]
                                 (if (= note corner)
                                   w
                                   (delete-part w note))) % (get-children sheet)))
    (update-thing! [] tree-changed)))

