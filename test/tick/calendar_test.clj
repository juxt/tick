;; Copyright © 2016-2018, JUXT LTD.

(ns tick.calendar-test
  (:require
   [clojure.test :refer :all]
   [tick.calendar :as cal]))

(deftest bank-holidays-in-england-and-wales-test
  (is (= 65 (count (cal/bank-holidays-in-england-and-wales)))))
