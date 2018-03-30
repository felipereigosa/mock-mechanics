
(in-ns 'temp.core)

(def world (atom {}))

(defn set-thing! [path value]
  (if (= path [])
    (reset! world value)
    (swap! world assoc-in path value))
  nil)

(defn get-thing! [path]
  (if (= path [])
    @world
    (get-in @world path)))

(defmacro update-thing! [path fn]
  (let [val-name (gensym 'val)]
    `(set-thing! ~path (let [~val-name (get-thing! ~path)]
                         ~(cons fn (list val-name))))))

(defn dissoc-in [map keys]
  (if (= (count keys) 1)
    (dissoc map (nth keys 0))
    (update-in map (butlast keys) dissoc (last keys))))

(defn remove-thing! [path]
  (swap! world dissoc-in path)
  nil)


