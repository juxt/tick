;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core-test
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :as t]
   #?(:clj
      [clojure.test :refer :all]
      :cljs
      [cljs.test :refer-macros [deftest is testing run-tests]])
    #?(:cljs
       [tick.js-joda :refer [LocalDate Instant]]))
  #?(:clj (:import [java.time Instant LocalDate])))

(s/check-asserts true)

(deftest basics-test
  (is (instance? Instant (t/now)))
  (is (instance? LocalDate (t/today)))
  (is (instance? LocalDate (t/tomorrow)))
  (is (instance? LocalDate (t/yesterday))))

(deftest divide-test
  (is
    ;; Duration -> Long -> Duration
    (= (t/make-duration 6 :hours) (t/divide (t/make-duration 6 :days) 24))
    ;; Duration -> Duration -> Long
    (= 63 (t/divide (t/make-duration 21 :days) (t/make-duration 8 :hours)))))

(deftest construction-test
  (is (= (t/date "2018-01-11")
         (t/date (t/instant 1515691416624)))))
