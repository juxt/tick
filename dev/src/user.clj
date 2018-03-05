(ns user
  (:require
   [tick.alpha.api :as t]
   [tick.viz :refer [show-canvas view label]]
   [clojure.spec.alpha :as s]
   [clojure.test]
   [infra])
  (:import [java.time DayOfWeek]))

(defn test-all []
  (require
    'tick.alpha.api-test
    'tick.alpha.api.dates-test
    'tick.core-test
    'tick.interval-test
    'tick.ical-test)
  (clojure.test/run-all-tests #"(tick).*test$"))

(println "port is" (read-string (slurp ".nrepl-port")))
