;; Copyright © 2016, JUXT LTD.

(ns tick.deprecated.timeline-test
  (:require
   [clojure.test :refer :all]
   [tick.deprecated.timeline :refer [interleave-timelines periodic-seq timeline sequencer]]
   [tick.deprecated.clock :refer [just-now]]
   [tick.deprecated.cal :as cal]
   [tick.core :refer [new-duration]])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate LocalDateTime]
   [java.time.temporal ChronoField ChronoUnit]))

(def LONDON (ZoneId/of "Europe/London"))

(def T0 (-> "2012-12-04T05:21:00" LocalDateTime/parse (.atZone LONDON)))
(def T1 (.plusSeconds T0 10))

(deftest ^:deprecated periodic-seq-test
  (let [sq (periodic-seq T0 (new-duration 1 :minutes))]
    (testing "sq starts with start time"
      (is (= T0 (first sq))))
    (testing "sq moves forward by 10 minutes"
      (is (= (-> "2012-12-04T05:31:00" LocalDateTime/parse (.atZone LONDON))
             (first (drop 10 sq)))))))

(defn acceptable-hours [zdt]
  (let [h (.getHour zdt)]
    (<= 7 h 21)))

(deftest ^:deprecated composition-test
  (testing "Filter by acceptable hours"
    (is (= 62 (count
               (->> (periodic-seq T0 (new-duration 1 :hours))
                    (take 100)
                    (filter acceptable-hours)))))))

(deftest ^:deprecated interleave-timelines-test
  (let [merged
        (interleave-timelines
         (take 10 (timeline (periodic-seq (.plus (just-now) (new-duration 10 :seconds)) (new-duration 1 :minutes))))
         (take 10 (timeline (periodic-seq (just-now) (new-duration 1 :minutes)))))]
    (is (distinct? merged))
    (is (= 20 (count merged)))))

(deftest ^:deprecated sequencer-test
  (is (= 0 (:tick/seq (first (sequence (sequencer)
                                       (interleave-timelines
                                        (timeline (map cal/easter-monday (iterate inc 2012)))
                                        (timeline (map cal/good-friday (iterate inc 2012)))))))))
  (is (= 10 (:tick/seq (first (sequence (sequencer 10)
                                        (interleave-timelines
                                         (timeline (map cal/easter-monday (iterate inc 2012)))
                                         (timeline (map cal/good-friday (iterate inc 2012))))))))))
