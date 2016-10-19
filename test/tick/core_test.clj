(ns tick.core-test
  (:require
   [clojure.test :refer :all]
   [tick.core :refer :all])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month]
   [java.time.temporal ChronoField]))

;; TODO: All instants must be in timezones

(def T0 (parse "2012-12-04T05:21:00Z"))
(def T1 (.plusSeconds T0 10))

(def LONDON (ZoneId/of "Europe/London"))

(deftest periodic-seq-test
  (let [sq (periodic-seq (fixed-clock T0) (minutes 1))]
    (testing "sq starts with start time"
      (is (= T0 (first sq))))
    (testing "sq moves forward by 10 minutes"
      (is (= (parse "2012-12-04T05:31:00Z") (first (drop 10 sq)))))))

(deftest schedule-test
  (let [at (atom [])
        schedule (atom (periodic-seq (fixed-clock T0) (seconds 1)))
        d (drainer (fixed-clock T1) #(swap! at conj %))]
    (swap! schedule d)
    (is (not (empty? @at)))
    (testing "Inclusive of T0 and the instant ten seconds ahead of the start-time."
      (is (= 11 (count @at))))
    (testing "First element is T0"
      (is (= T0 (first @at))))
    (testing "Last element is T1"
      (is (= T1 (last @at))))))

;; Rather than drainer, can we use something like tmap (time-map) or map-past ?

(defn acceptable-hours [zid]
  (fn [t]
    (let [h (.getHour (java.time.ZonedDateTime/ofInstant t zid))]
      (<= 7 h 21))))

(deftest composition-test
  (testing "Filter by acceptable hours"
    (is (= 62 (count
               (->> (periodic-seq (fixed-clock T0) (hours 1))
                    (take 100)
                    (filter (acceptable-hours (ZoneId/of "Europe/London")))))))))

(deftest easter-test
  (is ((easter-sunday? LONDON) (parse "2017-04-16T12:00:00Z")))
  (is ((good-friday? LONDON) (parse "2017-04-14T12:00:00Z")))
  (is ((easter-monday? LONDON) (parse "2017-04-17T12:00:00Z")))
  (is (not ((easter-sunday? LONDON) (parse "2018-04-16T12:00:00Z")))))


;; Need to use ZonedDateTime really, rather than instant.

;; Convert with ZonedDateTime/ofInstant

(time (count (take 200 (filter (easter-sunday? LONDON) (periodic-seq (fixed-clock T0) (days 1))))))
