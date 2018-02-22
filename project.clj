;; Copyright © 2016, JUXT LTD.

(defproject henryw374.tick "0.4.0-alpha-SNAPSHOT"
  :description "A Clojure library that deals with time"
  :url "https://github.com/juxt/tick"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[cljsjs/js-joda "1.6.2-0"]
                 [net.cgrand/macrovich "0.2.0"]]
  :profiles {:dev
             {:source-paths ["dev/src/cljs"]
              :dependencies [[org.clojure/clojure "1.9.0"]
                             [org.clojure/clojurescript "1.9.946"]
                             [lein-doo "0.1.8"]]
              :jvm-opts ["-Dclojure.spec.compile-asserts=true"]}
             :repl
             {:dependencies
              [[cider/cider-nrepl "0.16.0"]
               [org.clojure/clojurescript "1.10.238"]
               [org.clojure/data.xml "0.2.0-alpha5"]
               [org.apache.xmlgraphics/batik-swing "1.9"]
               [henryw374/defoclock "0.1.1"]]
              :source-paths ["dev/src"]}
             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]
                     :plugins [[lein-codox "0.10.3"]]
                     :codox {:project {:name "tick"}
                             :metadata {:doc/format :markdown} ;; docstring format
                             :themes [:rdash]
                             :output-path "gh-pages"
                             :doc-paths ["CHANGELOG.md"
                                         "doc/intro.md"]}}})
