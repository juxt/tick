(ns tick.core-test
  (:require
   [clojure.test :refer :all]
   [tick.core :refer :all])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime]
   [java.time.temporal ChronoField ChronoUnit]))

;; TODO: All instants must be in timezones

(def LONDON (ZoneId/of "Europe/London"))

(def T0 (parse "2012-12-04T05:21:00Z" "Europe/London"))
(def T1 (.plusSeconds T0 10))



(deftest periodic-seq-test
  (let [sq (periodic-seq (fixed-clock T0) (minutes 1))]
    (testing "sq starts with start time"
      (is (= T0 (first sq))))
    (testing "sq moves forward by 10 minutes"
      (is (= (parse "2012-12-04T05:31:00Z" "Europe/London") (first (drop 10 sq)))))))

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


;; Need to use ZonedDateTime really, rather than instant.

;; Convert with ZonedDateTime/ofInstant

#_(fixed-clock T0)

#_(take 10 (periodic-seq (fixed-clock T0) (days 2)))

#_(def next-easters (comp (filter easter-sunday?)
                         (take 200)))

#_(->> (periodic-seq (fixed-clock T0) (days 1))
     (eduction next-easters)
     )


(def a
  (let [clock (clock)]
    (new-clock-tracker
     clock
     (take 10 (periodic-seq clock (seconds 1)))
     println
     (new java.util.concurrent.ScheduledThreadPoolExecutor 16))

    ))

;;(:tick/future-timeline (deref a))
