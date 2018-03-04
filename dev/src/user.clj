(ns user
  (:refer-clojure :exclude [dec < range <= min long int > - time / >= inc + max complement atom])
  (:require
   [tick.alpha.api :refer :all]
   ;;[tick.deprecated.cal :refer [holidays-in-england-and-wales weekend?]]
   [tick.viz :refer [show-canvas view label]]
   ;;[tick.deprecated.schedule :as sch :refer [schedule]]
   [clojure.spec.alpha :as s]
   clojure.test)
  (:import [java.time DayOfWeek]))

(defn test-all []
  (require 'tick.alpha.api-test 'tick.core-test 'tick.interval-test)
  (clojure.test/run-all-tests #"(tick).*test$"))
