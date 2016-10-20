;; Copyright © 2016, JUXT LTD.

(ns tick.core-test
  (:require
   [clojure.test :refer :all]
   [tick.core :refer :all])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime]
   [java.time.temporal ChronoField ChronoUnit]))

(def T0 (parse "2012-12-04T05:21:00Z" "Europe/London"))
(def T1 (.plusSeconds T0 10))

(deftest periodic-seq-test
  (let [sq (periodic-seq T0 (minutes 1))]
    (testing "sq starts with start time"
      (is (= T0 (first sq))))
    (testing "sq moves forward by 10 minutes"
      (is (= (parse "2012-12-04T05:31:00Z" "Europe/London") (first (drop 10 sq)))))))

(defn acceptable-hours [zdt]
  (let [h (.getHour zdt)]
    (<= 7 h 21)))

(deftest composition-test
  (testing "Filter by acceptable hours"
    (is (= 62 (count
               (->> (periodic-seq (fixed-clock T0) (hours 1))
                    (take 100)
                    (filter acceptable-hours)))))))

(deftest easter-test
  (is (easter-sunday? (parse "2017-04-16T12:00:00Z" "Europe/London")))
  (is (good-friday? (parse "2017-04-14T12:00:00Z" "Europe/London")))
  (is (easter-monday? (parse "2017-04-17T12:00:00Z" "Europe/London")))
  (is (not (easter-sunday? (parse "2018-04-16T12:00:00Z" "Europe/London")))))

(let [clock (clock)
      now (.atZone (.instant clock) (ZoneId/of "Europe/London"))
      timeline (take 10 (periodic-seq now (seconds 1)))
      runner (map-t println timeline)]
  (start runner clock)
  )
