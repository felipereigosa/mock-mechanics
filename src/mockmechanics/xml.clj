
(require '[clojure.xml :refer [parse]])

(defn get-children [map child-name]
  (filter (fn [child]
            (= (:tag child) child-name))
          (:content map)))

(defn get-child [map child-name]
  (first (get-children map child-name)))

(defn has-map-signature? [map tag-name attrs]
  (and
   (= (:tag map) tag-name)
   (every? (fn [[name value]]
             (= (get-in map [:attrs name]) value))
           attrs)))

(defn get-child-if [map tag attrs]
  (find-if (slots has-map-signature? _ tag attrs) (:content map)))

(defn read-xml [filename]
  (parse filename))

(defn ppxml [xml]
  (let [in (javax.xml.transform.stream.StreamSource.
            (java.io.StringReader. xml))
        writer (java.io.StringWriter.)
        out (javax.xml.transform.stream.StreamResult. writer)
        transformer (.newTransformer 
                     (javax.xml.transform.TransformerFactory/newInstance))]
    (.setOutputProperty transformer 
                        javax.xml.transform.OutputKeys/INDENT "yes")
    (.setOutputProperty transformer 
                        "{http://xml.apache.org/xslt}indent-amount" "2")
    (.setOutputProperty transformer 
                        javax.xml.transform.OutputKeys/METHOD "xml")
    (.transform transformer in out)
    (-> out .getWriter .toString)))

(defn xml->str [document]
  (with-out-str (clojure.xml/emit-element document)))

(defn tree->xml [t]
  (let [[name content] t]
    {:tag name
     :attrs nil
     :content (if (vector? content)
                (vec (map tree->xml (partition 2 content)))
                [content])}))
