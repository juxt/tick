;; Copyright © 2016-2018, JUXT LTD.

(ns tick.alpha.api.dates-test
  (:refer-clojure :exclude [dec < range <= min long int > extend - time / >= inc + max complement atom swap-vals! reset-vals! compare-and-set! reset! swap! second group-by conj])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer :all]
   [tick.alpha.api :refer :all])
  (:import [java.time Clock LocalTime LocalDateTime]))

;; See doc/dates.adoc

(deftest time-construction-test
  (testing "(time)"
    (is (instance? LocalTime (time))))
  (testing "(time \"4pm\")"
    (is (instance? LocalTime (time "4pm")))
    (is (= "16:00" (str (time "4pm")))))
  (testing "(midnight)"
    (is (instance? LocalTime (midnight)))
    (is (= "00:00" (str (midnight)))))
  (testing "(noon)"
    (is (instance? LocalTime (noon)))
    (is (= "12:00" (str (noon))))))

(deftest date-construction-test
  (is (instance? LocalDateTime (noon (today))))
  (with-clock (-> (date "2018-02-14") (at "10:00"))
    (testing "(noon (today))"
      (is (= "2018-02-14T12:00" (str (noon (today))))))
    (testing "(noon (date))"
      (is (= "2018-02-14T12:00" (str (noon (date))))))))

;; TODO: Clock tests
;; Create with a value for a fixed clock. Value can be a time or a zone

(deftest clock-test
  (testing "clock"
    (with-clock (-> (date "2018-02-14") (at "10:00") (in "America/New_York"))
      (testing "(clock) return type"
        (is (instance? Clock (clock))))
      (testing "Time shifting the clock back by 2 hours"
        (is (= "2018-02-14T13:00:00Z" (str (instant (<< (clock) (duration 2 :hours)))))))))

  (testing "Creating a clock with a zone, and returning that zone"
    (is (= "America/New_York" (str (zone (clock (zone "America/New_York")))))))

  (testing "Creation of clock with fixed instant"
    (is (= "2017-10-31T16:00:00Z" (str (instant (clock "2017-10-31T16:00:00Z")))))))


;; TODO: tick function

;; TODO: Atomic clocks
