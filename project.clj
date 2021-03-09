(defproject temp "0.1.0-SNAPSHOT"

  :dependencies [[org.clojure/clojure "1.10.2"]
                 [org.clojars.nakkaya/vecmath "1"]
                 [org.clojars.nakkaya/jbullet "20101010"]
                 [matrix/matrix "1.0.0"]
                 [batik/batik "1.7.1"]

                 [lwjgl/lwjgl "1.0.0"]
                 [lwjgl/lwjgl-natives-linux "1.0.0"]
                 [lwjgl/lwjgl-natives-macos "1.0.0"]
                 [lwjgl/lwjgl-natives-windows "1.0.0"]
                 [lwjgl/lwjgl-natives-windows-x86 "1.0.0"]
                 
                 [lwjgl/lwjgl-glfw "1.0.0"]
                 [lwjgl/lwjgl-glfw-natives-linux "1.0.0"]
                 [lwjgl/lwjgl-glfw-natives-macos "1.0.0"]
                 [lwjgl/lwjgl-glfw-natives-windows "1.0.0"]
                 [lwjgl/lwjgl-glfw-natives-windows-x86 "1.0.0"]
                 
                 [lwjgl/lwjgl-opengl "1.0.0"]
                 [lwjgl/lwjgl-opengl-natives-linux "1.0.0"]
                 [lwjgl/lwjgl-opengl-natives-macos "1.0.0"]
                 [lwjgl/lwjgl-opengl-natives-windows "1.0.0"]
                 [lwjgl/lwjgl-opengl-natives-windows-x86 "1.0.0"]
                 ]
  :main ^:skip-aot temp.core
  :target-path "target/%s"
  :omit-source true
  :jvm-opts ["--illegal-access=deny"]
  :profiles {:uberjar {:aot :all}
             :repl {:plugins [[cider/cider-nrepl "0.25.5"]]}
             })

