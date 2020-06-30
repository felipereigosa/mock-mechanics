(defproject temp "0.1.0-SNAPSHOT"

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojars.nakkaya/vecmath "1"]
                 [org.clojars.nakkaya/jbullet "20101010"]
                 [matrix/matrix "1.0.0"]
                 [batik/batik "1.7.1"]
                 ]
  :main ^:skip-aot temp.core
  :target-path "target/%s"

  :jvm-opts ["--illegal-access=deny"]

  :resource-paths ["libs/lwjgl.jar"
                   "libs/lwjgl-natives-linux.jar"
                   
                   "libs/lwjgl-glfw.jar"
                   "libs/lwjgl-glfw-natives-linux.jar"

                   "libs/lwjgl-jemalloc.jar"
                   "libs/lwjgl-jemalloc-natives-linux.jar"
                   
                   "libs/lwjgl-opengl.jar"
                   "libs/lwjgl-opengl-natives-linux.jar"]
  
  :profiles {:uberjar {:aot :all}
             :repl {:plugins [[cider/cider-nrepl "0.25.0-alpha1"]]}
             })
