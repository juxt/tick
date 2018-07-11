;; Copyright © 2016-2018, JUXT LTD.

(ns tick.alpha.api.dates-test
  (:refer-clojure :exclude [dec < range <= min long int > extend - time / >= inc + max complement atom swap-vals! reset-vals! compare-and-set! reset! swap! second group-by conj])
  (:require
   [clojure.spec.alpha :as s]
    #?(:clj [clojure.test :refer :all]
       :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
    #?@(:cljs
        [[tick.js-joda :refer [Date Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime ChronoUnit ChronoField TemporalAdjusters Temporal TemporalAmount]]
         [cljsjs.js-joda-timezone]])
   [tick.alpha.api :as t :refer [with-clock] :refer-macros [with-clock]])
  #?(:clj (:import [java.time Clock LocalTime LocalDateTime])))

;; See doc/dates.adoc

(deftest time-construction-test
  (testing "(time)"
    (is (instance? LocalTime (t/time))))
  (testing "(time \"4pm\")"
    (is (instance? LocalTime (t/time "4pm")))
    (is (= "16:00" (str (t/time "4pm")))))
  (testing "(midnight)"
    (is (instance? LocalTime (t/midnight)))
    (is (= "00:00" (str (t/midnight)))))
  (testing "(noon)"
    (is (instance? LocalTime (t/noon)))
    (is (= "12:00" (str (t/noon))))))

(deftest date-construction-test
  (is (instance? LocalDateTime (t/noon (t/today))))
  (with-clock (-> (t/date "2018-02-14") (t/at "10:00"))
    (testing "(noon (today))"
      (is (= "2018-02-14T12:00" (str (t/noon (t/today))))))
    (testing "(noon (date))"
      (is (= "2018-02-14T12:00" (str (t/noon (t/date))))))))

;; TODO: Clock tests
;; Create with a value for a fixed clock. Value can be a time or a zone

(deftest clock-test
  (testing "clock"
    (with-clock (-> (t/date "2018-02-14") (t/at "10:00") (t/in "America/New_York"))
      (testing "(clock) return type"
        (is (instance? Clock (t/clock))))
      #?(:clj ; Clock.offset not yet implemented in js-joda - https://github.com/js-joda/js-joda/issues/245
         (testing "Time shifting the clock back by 2 hours"
           (is (= "2018-02-14T13:00:00Z" (str (t/instant (t/<< (t/clock) (t/new-duration 2 :hours))))))))))

  (testing "Creating a clock with a zone, and returning that zone"
    (is (= "America/New_York" (str (t/zone (t/clock (t/zone "America/New_York")))))))

  (testing "Creation of clock with fixed instant"
    (is (= "2017-10-31T16:00:00Z" (str (t/instant (t/clock "2017-10-31T16:00:00Z")))))))


;; TODO: tick function

;; TODO: Atomic clocks
