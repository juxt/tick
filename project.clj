;; Copyright © 2016, JUXT LTD.

(defproject tick "0.3.5"
  :description "A Clojure library that deals with time"
  :url "https://github.com/juxt/tick"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/spec.alpha "0.1.94"]]
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.9.0-alpha14"]]
              :jvm-opts ["-Dclojure.spec.compile-asserts=true"]}
             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]
                     :plugins [[lein-codox "0.10.3"]]
                     :codox {:project {:name "tick"}
                             :metadata {:doc/format :markdown} ;; docstring format
                             :themes [:rdash]
                             :output-path "gh-pages"
                             :doc-paths ["CHANGELOG.md"
                                         "doc/intro.md"]}}})
