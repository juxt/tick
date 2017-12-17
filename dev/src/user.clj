(ns user
  (:refer-clojure :exclude [dec < range <= min long int > - time / >= inc + max complement])
  (:require
   [tick.alpha.api :refer :all]
   [tick.deprecated.cal :refer [holidays-in-england-and-wales weekend?]]
   [tick.viz :refer [show-canvas view label]]
   [tick.deprecated.schedule :as s :refer [schedule]]
   [tick.deprecated.clock :refer [clock-ticking-in-seconds]]
   clojure.test
   tick.alpha.api-test
   tick.core-test
   tick.interval-test)
  (:import [java.time DayOfWeek]))

(defn test-all []
  (clojure.test/run-all-tests #"(tick).*test$"))
