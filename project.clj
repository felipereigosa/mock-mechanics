
(defn lwjgl [names version]
  (mapcat (fn [name]
            (let [artifact (symbol "org.lwjgl" name)]
              [[artifact version]
               [artifact version
                :classifier "natives-linux"
                :native-prefix ""]
               [artifact version
                :classifier "natives-macos-arm64"
                :native-prefix ""]
               [artifact version
                :classifier "natives-macos"
                :native-prefix ""]]))
          names))

(defproject mockmechanics "2.0"
  :dependencies ~(concat '[[org.clojure/clojure "1.10.0"]
                           [org.clojars.nakkaya/jbullet "20101010"]
                           [org.apache.xmlgraphics/batik-all "1.14" :extension "pom"]]
                         (lwjgl ["lwjgl"
                                 "lwjgl-opengl"
                                 "lwjgl-glfw"] "3.3.1"))
  :disable-implicit-clean true
  :main ^:skip-aot mockmechanics.core
  :target-path "target/%s"
  :omit-source true
  :jvm-opts ["--illegal-access=deny"]
  :aliases {"osx" ["update-in" ":jvm-opts" "concat" "[\"-XstartOnFirstThread\" \"-Djava.awt.headless=true\"]"]}
  :java-source-paths ["src/mockmechanics/java"]
  :profiles {:uberjar {:aot :all}
             :repl {:plugins [[cider/cider-nrepl "0.25.5"]]}})