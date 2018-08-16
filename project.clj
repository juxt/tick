;; Copyright © 2016, JUXT LTD.

(defproject tick "0.4.0-alpha"
  :description "A Clojure library that deals with time"
  :url "https://github.com/juxt/tick"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[cljs.java-time "0.1.2"]
                 [org.clojure/clojurescript "1.10.238"]
                 [net.cgrand/macrovich "0.2.0" :exclusions [org.clojure/clojurescript]]]
  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:output-to "target/testable.js"
                        :main tick.all-tests
                        :output-dir "target"
                        :optimizations :none
                        :target :nodejs}}]}
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.9.0"]
                             [cljsjs/js-joda-timezone "1.3.0-0"]
                             [henryw374/js-joda-locale-en-us "1.0.0-1"]]
              :plugins [[lein-doo "0.1.10"]]
              :jvm-opts ["-Dclojure.spec.compile-asserts=true"]}
             :repl
             {:dependencies
              [[cider/cider-nrepl "0.16.0"]
               [org.clojure/data.xml "0.2.0-alpha5"]
               [org.apache.xmlgraphics/batik-swing "1.9"]]
              :source-paths ["dev/src"]}
             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]
                     :plugins [[lein-codox "0.10.3"]]
                     :codox {:project {:name "tick"}
                             :metadata {:doc/format :markdown} ;; docstring format
                             :themes [:rdash]
                             :output-path "gh-pages"
                             :doc-paths ["CHANGELOG.md"
                                         "doc/intro.md"]}}})
