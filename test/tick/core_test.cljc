;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core-test
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :as t]
   [clojure.test
    :refer [deftest is testing run-tests]
    :refer-macros [deftest is testing run-tests]]))

(s/check-asserts true)

(deftest basics-test
  (is (t/instant? (t/now)))
  (is (t/date? (t/today)))
  (is (t/date? (t/tomorrow)))
  (is (t/date? (t/yesterday))))

(deftest divide-test
  (is
    ;; Duration -> Long -> Duration
    (= (t/new-duration 6 :hours) (t/divide (t/new-duration 6 :days) 24))
    ;; Duration -> Duration -> Long
    (= 63 (t/divide (t/new-duration 21 :days) (t/new-duration 8 :hours)))))

(deftest construction-test
  (is (= (t/date "2018-01-11")
         (t/date (t/instant 1515691416624)))))
