;; Copyright © 2016, JUXT LTD.

(ns tick.timeline-test
  (:require
   [clojure.test :refer :all]
   [tick.timeline :refer [merge-timelines periodic-seq]]
   [tick.clock :refer [just-now]]
   [tick.core :refer [seconds minutes hours]])
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

(deftest merge-timelines-test
  (let [merged
        (merge-timelines
         [(take 10 (periodic-seq (.plus (just-now) (seconds 10)) (minutes 1)))
          (take 10 (periodic-seq (just-now) (minutes 1)))])]
    (is (distinct? merged))
    (is (= 20 (count merged)))))
