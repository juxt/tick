;; Copyright © 2016-2017, JUXT LTD.

(ns tick.cal-test
  (:require
   [clojure.test :refer :all]
   [tick.cal :refer :all])
  (:import
   [java.time LocalDate]))

(deftest easter-test
  (is (easter-sunday? (-> "2017-04-16" LocalDate/parse)))
  (is (good-friday? (LocalDate/parse "2017-04-14")))
  (is (easter-monday? (-> "2017-04-17" LocalDate/parse)))
  (is (not (easter-sunday? (-> "2018-04-19" LocalDate/parse))))
  (is (every? easter-sunday? (easter-sundays (-> "1900-01-01" LocalDate/parse))))
  (is (every? good-friday? (good-fridays (-> "1900-01-01" LocalDate/parse))))
  (is (every? easter-monday? (easter-mondays (-> "1900-01-01" LocalDate/parse)))))
