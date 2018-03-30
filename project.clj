(defproject temp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojars.nakkaya/vecmath "1"]
                 [org.clojars.nakkaya/jbullet "20101010"]
                 [lwjgl/lwjgl "3.0.0"]
                 [matrix/matrix "1.0.0"]
                 [batik/batik "1.7.1"]
                 ]

  :plugins [[cider/cider-nrepl "0.8.2"]]

  :main ^:skip-aot temp.core
  
  :profiles {:uberjar {:aot :all}})
