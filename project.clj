;; Copyright © 2016, JUXT LTD.

(defproject tick "0.3.6-SNAPSHOT"
  :description "A Clojure library that deals with time"
  :url "https://github.com/juxt/tick"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/spec.alpha "0.1.94"]]
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.9.0-beta3"]]
              :jvm-opts ["-Dclojure.spec.compile-asserts=true"]}
             :repl {:dependencies
                    [[org.clojure/data.xml "0.2.0-alpha5"]
                     [net.mikera/core.matrix "0.61.0"]
                     [org.apache.xmlgraphics/batik-swing "1.9"]]
                    :source-paths ["dev/src"]}})
