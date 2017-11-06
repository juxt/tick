;; Copyright © 2016-2017, JUXT LTD.

(ns tick.deprecated.cal-test
  (:require
   [clojure.test :refer :all]
   [tick.deprecated.cal :refer :all])
  (:import
   [java.time LocalDate YearMonth DayOfWeek]))

;; https://www.gov.uk/bank-holidays
;; https://en.wikipedia.org/wiki/Bank_holiday

(deftest holidays
  (is (= {:name "New Year's Day"
          :date (LocalDate/parse "2012-01-02")
          :substitute-day true}
         (new-years-day-holiday 2012)))
  (is (= {:name "Good Friday"
          :date (LocalDate/parse "2012-04-06")}
         (good-friday-holiday 2012)))
  (is (= {:name "Easter Monday"
          :date (LocalDate/parse "2012-04-09")}
         (easter-monday-holiday 2012)))
  (is (= {:name "Early May bank holiday"
          :date (LocalDate/parse "2012-05-07")}
         (early-may-bank-holiday 2012)))

  ;; TODO: Add exceptions from https://en.wikipedia.org/wiki/Bank_holiday
  #_(is (= {:name "Spring bank holiday"
            :date (LocalDate/parse "2012-06-04")
            :substitute-day true}
           (spring-bank-holiday 2012)))

  (is (= {:name "Summer bank holiday"
          :date (LocalDate/parse "2012-08-27")}
         (summer-bank-holiday 2012)))
  (is (= {:name "Christmas Day"
          :date (LocalDate/parse "2012-12-25")
          :substitute-day false}
         (christmas-day-holiday 2012)))
  (is (= {:name "Boxing Day"
          :date (LocalDate/parse "2012-12-26")
          :substitute-day false}
         (boxing-day-holiday 2012)))

  (is (= {:name "New Year's Day"
          :date (LocalDate/parse "2013-01-01")
          :substitute-day false}
         (new-years-day-holiday 2013)))
  (is (= {:name "Good Friday"
          :date (LocalDate/parse "2013-03-29")}
         (good-friday-holiday 2013)))
  (is (= {:name "Easter Monday"
          :date (LocalDate/parse "2013-04-01")}
         (easter-monday-holiday 2013)))
  (is (= {:name "Early May bank holiday"
          :date (LocalDate/parse "2013-05-06")}
         (early-may-bank-holiday 2013)))
  (is (= {:name "Spring bank holiday"
          :date (LocalDate/parse "2013-05-27")}
         (spring-bank-holiday 2013)))
  (is (= {:name "Summer bank holiday"
          :date (LocalDate/parse "2013-08-26")}
         (summer-bank-holiday 2013)))
  (is (= {:name "Christmas Day"
          :date (LocalDate/parse "2013-12-25")
          :substitute-day false}
         (christmas-day-holiday 2013)))
  (is (= {:name "Boxing Day"
          :date (LocalDate/parse "2013-12-26")
          :substitute-day false}
         (boxing-day-holiday 2013)))

  (is (= {:name "New Year's Day"
          :date (LocalDate/parse "2014-01-01")
          :substitute-day false}
         (new-years-day-holiday 2014)))
  (is (= {:name "Good Friday"
          :date (LocalDate/parse "2014-04-18")}
         (good-friday-holiday 2014)))
  (is (= {:name "Easter Monday"
          :date (LocalDate/parse "2014-04-21")}
         (easter-monday-holiday 2014)))
  (is (= {:name "Early May bank holiday"
          :date (LocalDate/parse "2014-05-05")}
         (early-may-bank-holiday 2014)))
  (is (= {:name "Spring bank holiday"
          :date (LocalDate/parse "2014-05-26")}
         (spring-bank-holiday 2014)))
  (is (= {:name "Summer bank holiday"
          :date (LocalDate/parse "2014-08-25")}
         (summer-bank-holiday 2014)))
  (is (= {:name "Christmas Day"
          :date (LocalDate/parse "2014-12-25")
          :substitute-day false}
         (christmas-day-holiday 2014)))
  (is (= {:name "Boxing Day"
          :date (LocalDate/parse "2014-12-26")
          :substitute-day false}
         (boxing-day-holiday 2014)))

  (is (= {:name "New Year's Day"
          :date (LocalDate/parse "2015-01-01")
          :substitute-day false}
         (new-years-day-holiday 2015)))
  (is (= {:name "Good Friday"
          :date (LocalDate/parse "2015-04-03")}
         (good-friday-holiday 2015)))
  (is (= {:name "Easter Monday"
          :date (LocalDate/parse "2015-04-06")}
         (easter-monday-holiday 2015)))
  (is (= {:name "Early May bank holiday"
          :date (LocalDate/parse "2015-05-04")}
         (early-may-bank-holiday 2015)))
  (is (= {:name "Spring bank holiday"
          :date (LocalDate/parse "2015-05-25")}
         (spring-bank-holiday 2015)))
  (is (= {:name "Summer bank holiday"
          :date (LocalDate/parse "2015-08-31")}
         (summer-bank-holiday 2015)))
  (is (= {:name "Christmas Day"
          :date (LocalDate/parse "2015-12-25")
          :substitute-day false}
         (christmas-day-holiday 2015)))
  (is (= {:name "Boxing Day"
          :date (LocalDate/parse "2015-12-28")
          :substitute-day true}
         (boxing-day-holiday 2015)))

  (is (= {:name "New Year's Day"
          :date (LocalDate/parse "2016-01-01")
          :substitute-day false}
         (new-years-day-holiday 2016)))
  (is (= {:name "Good Friday"
          :date (LocalDate/parse "2016-03-25")}
         (good-friday-holiday 2016)))
  (is (= {:name "Easter Monday"
          :date (LocalDate/parse "2016-03-28")}
         (easter-monday-holiday 2016)))
  (is (= {:name "Early May bank holiday"
          :date (LocalDate/parse "2016-05-02")}
         (early-may-bank-holiday 2016)))
  (is (= {:name "Spring bank holiday"
          :date (LocalDate/parse "2016-05-30")}
         (spring-bank-holiday 2016)))
  (is (= {:name "Summer bank holiday"
          :date (LocalDate/parse "2016-08-29")}
         (summer-bank-holiday 2016)))
  (is (= {:name "Boxing Day"
          :date (LocalDate/parse "2016-12-26")
          :substitute-day false}
         (boxing-day-holiday 2016)))
  (is (= {:name "Christmas Day"
          :date (LocalDate/parse "2016-12-27")
          :substitute-day true}
         (christmas-day-holiday 2016)))

  (is (= {:name "New Year's Day"
          :date (LocalDate/parse "2017-01-02")
          :substitute-day true}
         (new-years-day-holiday 2017)))
  (is (= {:name "Good Friday"
          :date (LocalDate/parse "2017-04-14")}
         (good-friday-holiday 2017)))
  (is (= {:name "Easter Monday"
          :date (LocalDate/parse "2017-04-17")}
         (easter-monday-holiday 2017)))
  (is (= {:name "Early May bank holiday"
          :date (LocalDate/parse "2017-05-01")}
         (early-may-bank-holiday 2017))))
