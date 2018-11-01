(defproject diffusion "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [metasoarous/oz "1.3.1"]
                 [aysylu/loom "1.0.2-SNAPSHOT"]
                 [org.clojure/data.csv "0.1.3"]
                 [anglican "1.0.0"]
                 [ministrants "0.1.0-SNAPSHOT"]
                 [org.clojure/tools.cli "0.4.1"]]
  :main ^:skip-aot diffusion.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
