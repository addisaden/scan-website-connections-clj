(defproject website-connections "0.1.0"
  :description "Scan Websites for Connections between hosts"
  :url "https://github.com/addisaden/scan-website-connections-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [seesaw "1.4.4"]]
  :main ^:skip-aot website-connections.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
