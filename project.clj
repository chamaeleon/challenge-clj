(defproject challenge "0.1.0-SNAPSHOT"
  :description "VM implementation for the Synacor Challenge"
  :url "https://github.com/chamaeleon/challenge-clj"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [gloss "0.2.6"]]
  ;:global-vars {*warn-on-reflection* true}
  :main ^:skip-aot challenge.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
