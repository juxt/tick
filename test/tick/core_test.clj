;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core-test
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :as t]
   [clojure.test :refer :all])
  (:import [java.time Instant LocalDate]))

(s/check-asserts true)

(deftest basics-test
  (is (instance? Instant (t/now)))
  (is (instance? LocalDate (t/today)))
  (is (instance? LocalDate (t/tomorrow)))
  (is (instance? LocalDate (t/yesterday))))
