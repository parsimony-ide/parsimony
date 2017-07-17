(defproject parsimony "0.1.0"
  :description "Parsimony backend"
  :license {:name "BSD 3-clause"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[alanlcode/util "0.1.0"]
                 [aysylu/loom "0.5.4"]
                 [com.stuartsierra/component "0.3.1"]
                 [com.taoensso/encore "2.33.0"]
                 [com.taoensso/nippy "2.10.0"]
                 [com.taoensso/sente "1.8.1"]
                 [com.taoensso/timbre "4.2.1"]
                 [compojure "1.4.0"]
                 [dk.brics.automaton/automaton "1.11.2"]
                 [environ "1.0.2"]
                 [fipp "0.6.4"]
                 [instaparse "1.4.1"]
                 [liberator "0.14.0"]
                 [net.mikera/core.matrix "0.49.0"]
                 [net.mikera/vectorz-clj "0.43.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.danielsz/system "0.4.0"]
                 [org.jordanlewis/data.union-find "0.1.0"]
                 [org.immutant/web "2.1.2"]
                 [potemkin "0.4.3"]
                 [prismatic/schema "1.0.4"]
                 [progrock "0.1.2"]
                 [ring/ring-defaults "0.1.5"]
                 [ring/ring-core "1.4.0"]
                 [ubergraph "0.2.0"]]
  :main ^:skip-aot parsimony.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["dev"]
                   :dependencies [[alembic "0.3.2"]
                                  [org.clojure/tools.namespace "0.2.11"]
                                  [ring/ring-mock "0.2.0"]]
                   :plugins [[lein-environ "1.0.2"]]}})
