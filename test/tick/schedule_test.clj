;; Copyright © 2016-2017, JUXT LTD.

(ns tick.schedule-test
  (:require
   [clojure.test :refer :all]
   [tick.core :refer :all]
   [tick.schedule :as sched]))

(deftest schedule-test
  (let [a (atom 0)
        f (fn [dt] (swap! a inc))
        clk (clock-ticking-in-seconds)
        now (just-now clk)
        timeline (take 10 (periodic-seq now (millis 10)))]
    @(sched/start (sched/schedule f timeline) clk)
    (is (= @a 10))))

(deftest simulate-test
  (let [a (atom 0)
        f (fn [dt] (swap! a inc))
        clk (clock-ticking-in-seconds)
        now (just-now clk)
        timeline (take 1000 (periodic-seq now (seconds 1)))]
    @(sched/start (sched/simulate f timeline) clk)
    (is (= @a 1000))))
