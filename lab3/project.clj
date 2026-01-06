(defproject lab2 "0.1.0-SNAPSHOT"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]]
  :main ^:skip-aot lab3.core
  :target-path "target/%s"
  :plugins [[lein-cljfmt "0.8.2"]
            [lein-kibit "0.1.8"]
            [lein-bikeshed "0.5.2"]
            [lambdaisland/kaocha "1.91.1392"]]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[org.clojure/test.check "1.1.1"]
                                  [pjstadig/humane-test-output "0.11.0"]
                                  [clj-kondo "2023.10.20"]
                                  [lambdaisland/kaocha "1.91.1392"]]}}
  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :all (constantly true)}
  :aliases {"lint" ["do"
                    ["cljfmt" "check"]
                    ["kibit"]
                    ["bikeshed" "--max-line-length" "120"]
                    ["run" "-m" "clj-kondo.main" "--lint" "src" "test" "--config" "../.clj-kondo/config.edn"]]
            "kaocha" ["run" "-m" "kaocha.runner"]
            "test" ["kaocha"]
            "test-unit" ["kaocha" "--focus" ":unit"]
            "test-property" ["kaocha" "--focus" ":property-based"]})