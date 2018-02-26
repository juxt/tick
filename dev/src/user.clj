(ns user
  (:require
   [tick.alpha.api :as t]
   [tick.viz :refer [show-canvas view label]]
   [clojure.spec.alpha :as s]
   [cljs.cljs-repl :as cljs-repl]
   [infra]
   clojure.test)
  (:import [java.time DayOfWeek]))

(when (System/getProperty "nrepl.load")
  (require 'nrepl))

(defn test-all []
  (require
    'tick.alpha.api-test
    'tick.alpha.api.dates-test
    'tick.core-test
    'tick.interval-test
    'tick.ical-test)
  (clojure.test/run-all-tests #"(tick).*test$"))

(defn node []
  (cljs-repl/node-repl))
