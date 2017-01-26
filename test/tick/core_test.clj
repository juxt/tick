;; Copyright © 2016, JUXT LTD.

(ns tick.core-test
  (:require
   [clojure.test :refer :all]
   [tick.core :refer :all])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate LocalDateTime]
   [java.time.temporal ChronoField ChronoUnit]))

(def LONDON (ZoneId/of "Europe/London"))

(def T0 (-> "2012-12-04T05:21:00" LocalDateTime/parse (.atZone LONDON)))
(def T1 (.plusSeconds T0 10))

(deftest periodic-seq-test
  (let [sq (periodic-seq T0 (minutes 1))]
    (testing "sq starts with start time"
      (is (= T0 (first sq))))
    (testing "sq moves forward by 10 minutes"
      (is (= (-> "2012-12-04T05:31:00" LocalDateTime/parse (.atZone LONDON))
             (first (drop 10 sq)))))))

(defn acceptable-hours [zdt]
  (let [h (.getHour zdt)]
    (<= 7 h 21)))

(deftest composition-test
  (testing "Filter by acceptable hours"
    (is (= 62 (count
               (->> (periodic-seq T0 (hours 1))
                    (take 100)
                    (filter acceptable-hours)))))))

(deftest easter-test
  (is (easter-sunday? (-> "2017-04-16" LocalDate/parse)))
  (is (good-friday? (LocalDate/parse "2017-04-14")))
  (is (easter-monday? (-> "2017-04-17" LocalDate/parse)))
  (is (not (easter-sunday? (-> "2018-04-19" LocalDate/parse))))
  (is (every? easter-sunday? (easter-sundays (-> "1900-01-01" LocalDate/parse))))
  (is (every? good-friday? (good-fridays (-> "1900-01-01" LocalDate/parse))))
  (is (every? easter-monday? (easter-mondays (-> "1900-01-01" LocalDate/parse)))))

(deftest merge-timelines-test
  (let [merged
        (merge-timelines
         [(take 10 (periodic-seq (.plus (just-now) (seconds 10)) (minutes 1)))
          (take 10 (periodic-seq (just-now) (minutes 1)))])]
    (is (distinct? merged))
    (is (= 20 (count merged)))))
