;; Copyright © 2016, JUXT LTD.

(defproject tick "0.1.0"
  :description "Lightweight scheduling"
  :url "https://github.com/juxt/tick"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[clj-time/clj-time "0.12.0"]]
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.9.0-alpha13"]]}})
