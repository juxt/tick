;; Copyright © 2016-2017, JUXT LTD.

(ns tick.api-test
  (:require
    [clojure.test
     :refer [deftest is testing run-tests]
     :refer-macros [deftest is testing run-tests]]
    [tick.core :as t]
    [tick.locale-en-us]
    [cljc.java-time.clock]
    [cljc.java-time.instant]
    [cljc.java-time.day-of-week]
    [cljc.java-time.month]
    [cljc.java-time.year]))

(deftest time-construction-test
  (testing "(time)"
    (is (t/time? (t/time))))
  (testing "(midnight)"
    (is (t/time? (t/midnight)))
    (is (= "00:00" (str (t/midnight)))))
  (testing "(noon)"
    (is (t/time? (t/noon)))
    (is (= "12:00" (str (t/noon))))))

(deftest date-construction-test
  (is (= (t/date "2018-01-11")
        (t/date (t/instant 1515691416624))))
  (is (t/date-time? (t/noon (t/today))))
  (t/with-clock (-> (t/date "2018-02-14") (t/at "10:00"))
    (testing "(noon (today))"
      (is (= "2018-02-14T12:00" (str (t/noon (t/today))))))
    (testing "(noon (date))"
      (is (= "2018-02-14T12:00" (str (t/noon (t/date))))))))

;; TODO: Clock tests
;; Create with a value for a fixed clock. Value can be a time or a zone

(deftest clock-test
  (testing "clock"
    (t/with-clock (-> (t/date "2018-02-14") (t/at "10:00") (t/in "America/New_York"))
      (testing "(clock) return type"
        (is (t/clock? (t/clock))))
      (testing "Time shifting the clock back by 2 hours"
        (is (= "2018-02-14T13:00:00Z" (str (t/instant (t/<< (t/clock) (t/new-duration 2 :hours)))))))
      (testing "with instant"
        (is (= (t/zone (t/clock (t/instant)))
              (t/zone "America/New_York"))))))

  (testing "Converting using with-clock"
    (t/with-clock (t/clock (t/zone "America/New_York"))
      (testing "inst to zoned-date-time"
        (is (= (t/zoned-date-time #inst"2019-08-07T16:00")
              (t/zoned-date-time "2019-08-07T12:00-04:00[America/New_York]"))))
      (testing "date-time to zoned-date-time"
        (is (= (t/zoned-date-time (t/date-time "2019-08-07T12:00"))
              (t/zoned-date-time "2019-08-07T12:00-04:00[America/New_York]"))))
      (testing "date-time to offset-date-time"
        (is (= (t/offset-date-time (t/date-time "2019-08-07T12:00"))
              (t/offset-date-time "2019-08-07T12:00-04:00"))))))

  (testing "Creating a clock with a zone, and returning that zone"
    (is (= "America/New_York" (str (t/zone (t/clock (t/zone "America/New_York")))))))

  (testing "Creation of clock with fixed instant"
    (is (= "2017-10-31T16:00:00Z" (str (t/instant (t/clock "2017-10-31T16:00:00Z")))))))


(deftest constructor-test
  (is (t/year? (t/year 2017)))
  (is (= 2017 (cljc.java-time.year/get-value (t/year 2017))))
  (is (t/month? (t/month 12)))
  (is (= t/DECEMBER (t/month 12)))
  (is (= (t/new-date 3030 3 3)
         (t/date "3030-03-03")))
  (is (-> (t/new-duration 1000 :millis)
          (t/inst)
          (t/instant)
          (cljc.java-time.instant/to-epoch-milli)
          (= 1000)))
  (is (= (t/new-year-month 2020 7)
         (t/year-month  "2020-07"))))

(deftest extraction-test
  (is (= 2 (t/int t/FEBRUARY)))
  (is (= 2 (t/int t/TUESDAY)))
  (is (= t/AUGUST (t/month (t/date-time "2017-08-08T12:00:00"))))
  (is (= t/AUGUST (t/month (t/year-month "2017-08"))))
  (is (= (t/year 2019) (t/year (t/zoned-date-time "2019-09-05T00:00:00+02:00[Europe/Oslo]"))))
  (is (= (t/year 2019) (t/year (t/offset-date-time "2019-09-05T00:00:00-03:00"))))
  (is (= (t/zone-offset "-04:00")
         (t/zone-offset (t/zoned-date-time "2019-03-15T15:00-04:00[America/New_York]"))))
  (is (= (t/zone-offset "-04:00")
         (t/zone-offset (t/offset-date-time "2019-03-15T15:00-04:00")))))

;; Point-in-time tests
(deftest today-test
  (t/with-clock (cljc.java-time.clock/fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= (t/instant "2017-08-08T12:00:00Z") (t/now)))
    (is (= (t/date "2017-08-08") (t/today)))
    (is (= (t/date "2017-08-07") (t/yesterday)))
    (is (= (t/date "2017-08-09") (t/tomorrow)))
    (is (= 8 (t/day-of-month (t/today))))
    (is (= 2017 (t/int (t/year))))
    (is (= (t/date-time "2017-08-08T12:00:00") (t/noon (t/today))))
    (is (= (t/date-time "2017-08-08T00:00:00") (t/midnight (t/today))))))



(deftest instant-test
  (testing "instant basics"
    (is (t/instant? (t/instant (t/now))))
    (is (t/instant? (t/instant (str cljc.java-time.instant/min))))
    (is (t/instant? (t/instant (t/zoned-date-time))))))

(deftest offset-date-time-test
  (let [t "2018-09-24T18:57:08.996+01:00"]
    (testing "offset date time basics"
      (is (t/offset-date-time? (t/offset-date-time (t/now))))
      (is (t/offset-date-time? (t/offset-date-time t)))
      (is (t/offset-date-time? (t/offset-date-time (t/date-time))))
      (is (t/offset-date-time? (t/offset-date-time (t/zoned-date-time)))))))

(deftest zoned-date-time-test
  (is (t/zoned-date-time? (t/zoned-date-time "2020-12-15T12:00:10Z[Europe/London]")))
  (is (t/zoned-date-time? (t/zoned-date-time "2020-12-15T12:00:10+04:00[Europe/London]"))))

(deftest fields-test
  (let [xs [(t/now)
            (t/zoned-date-time)
            (t/offset-date-time)
            (t/date-time)
            (t/date)
            (t/time)
            (t/year)
            (t/year-month)]]
    (doseq [x xs]
      (let [fields (t/fields x)
            fields-map (into {} fields)]
        (is (not-empty fields-map))
        (doseq [[f v] fields-map]
          (is (= v (get fields f)))
          (is (= :foo (get fields :bar :foo))))))))

(deftest formatting-test
  (testing "all predefined formatters exist"
    (doseq [pre-defined (vals t/predefined-formatters)]
      (is pre-defined)))
  (let [d "3030-05-03"]
    (is (= d (t/format :iso-local-date (t/date d))))
    (is (= d (t/format (t/formatter :iso-local-date) (t/date d))))
    (is (= d (t/format (t/formatter "YYYY-MM-dd") (t/date d))))
    #?(:clj
       (is (= "3030-mai-03" (t/format (t/formatter "YYYY-MMM-dd" java.util.Locale/FRENCH) 
                              (t/date d)))))))

(deftest epoch-test
  (is (= (cljc.java-time.instant/parse "1970-01-01T00:00:00Z") (t/epoch))))

;; Period arithmetic

(deftest addition-test
  (is (= (t/new-duration 5 :seconds) (t/+ (t/new-duration 2 :seconds) (t/new-duration 3 :seconds))))
  (is (= (t/new-duration 2 :minutes) (t/+ (t/new-duration 90 :seconds) (t/new-duration 30 :seconds)))))

(deftest subtraction-test
  (is (= (t/new-duration 3 :seconds) (t/- (t/new-duration 5 :seconds) (t/new-duration 2 :seconds)))))

;; Between test
(deftest between-test
  (is
    (=
      (let [now (t/now)]
        (t/between
          (t/<< now (t/new-duration 10 :seconds))
          (t/>> now (t/new-duration 10 :seconds))))
      (t/new-duration 20 :seconds)))
  (is
    (= (t/new-duration 48 :hours)
      (t/between (t/beginning (t/today)) (t/end (t/tomorrow)))))
  (is
    (=
      (t/new-duration 2 :minutes)
      (t/between "2020-01-01T12:00" "2020-01-01T12:02")))
  (is
    (=
      (t/new-duration 30 :minutes)
      (t/between (t/new-time 11 0 0) (t/new-time 11 30 0))))
  (is
   (=
    (t/new-duration 2 :minutes)
    (t/between #inst "2020-01-01T12:00" #inst "2020-01-01T12:02")))

  (testing "LocalDate"
    (is (= (t/new-period 1 :days)
          (t/between (t/date "2020-01-01")
            (t/date "2020-01-02"))))))

;; Range test

(deftest range-test
  (is (t/midnight? (t/beginning (t/today))))
  (is (t/midnight? (t/end (t/today))))
  (is (t/midnight? (t/beginning (t/year))))
  (is (t/midnight? (t/end (t/year)))))

;; Units test

(deftest units-test
  (is (=
        {:seconds 0, :nanos 1}
        (t/units (t/new-duration 1 :nanos))))
  (is
    (=
      {:years 10, :months 0, :days 0}
      (t/units (t/new-period 10 :years)))))

;; Comparison test
(defn point-in-time-comparable [i]
  [i
   (t/inst i)
   (t/zoned-date-time i)
   (t/offset-date-time i)])

(deftest truncate-test 
  (let [dates [(t/instant) (t/zoned-date-time) (t/date-time)
               (t/offset-date-time) (t/time)]
        truncate-tos [:nanos
                     :micros
                     :millis
                     :seconds
                     :minutes
                     :hours
                     :half-days
                     :days     ]]
    (doseq [date dates 
            truncate-to truncate-tos]
      (is (t/truncate date truncate-to)))))

(deftest parse-test 
  (is (t/date? (t/parse-date "2020/02/02" (t/formatter "yyyy/MM/dd"))))
  (is (t/year? (t/parse-year "20" (t/formatter "yy"))))
  (is (t/year-month? (t/parse-year-month "20/02" (t/formatter "yy/MM"))))
  (is (t/date-time? (t/parse-date-time "2020/02/02:2002" (t/formatter "yyyy/MM/dd:HHmm"))))
  (is (t/time? (t/parse-time "2002" (t/formatter "HHmm"))))
  (is (t/zoned-date-time? (t/parse-zoned-date-time "2020/02/02:2002:Z" 
                            (t/formatter "yyyy/MM/dd:HHmm:VV"))))
  (is (t/offset-date-time? (t/parse-offset-date-time "2020/02/02:2002:-08:30" 
                             (t/formatter "yyyy/MM/dd:HHmm:VV")))))

(deftest comparison-test
  (let [point (t/truncate (t/instant) :millis)
        later (t/>> point (t/new-duration 1 :millis))]
    (testing "max-min"
      (is (= later (t/max point later point later)))
      (is (= point (t/min point later point later))))
    (testing "comparables not="
      (doseq [point (point-in-time-comparable point)]
        (testing "comparables ="
          (is (apply t/= point (point-in-time-comparable point)))
          (is (apply t/>= point (point-in-time-comparable point))))
        (is (apply t/<= point (point-in-time-comparable later))))
      (doseq [later (point-in-time-comparable later)]
        (is (apply t/>= later (point-in-time-comparable point))))
      
      (doseq [point (point-in-time-comparable point)
              later (point-in-time-comparable later)]
        (is (t/<= point later))
        (is (t/< point later))
        (is (t/>= later point))
        (is (t/> later point)))))
  
  (testing "ZonedDateTimes in different zones should be equals"
    (is (t/=
          (t/zoned-date-time "2017-10-31T16:00:00-04:00[America/New_York]")
          (t/zoned-date-time "2017-10-31T13:00:00-07:00[America/Los_Angeles]"))))

  (testing "ZoneDateTimes and OffsetDateTime should be equals if represents the same point in time"
    (is (t/=
          (t/zoned-date-time "2017-10-31T16:00:00-04:00[America/New_York]")
          (t/offset-date-time "2017-10-31T13:00-07:00"))))

  (testing "ZoneDateTimes and platform Date should be equals if represents the same point in time"
    (is (t/=
          (t/zoned-date-time "2017-10-31T16:00:00-04:00[America/New_York]")
          (t/inst "2017-10-31T20:00:00Z"))))

  (testing "Instants and ZonedDateTimes should be equals if represents the same point in time"
    (is (t/=
          (t/instant (t/clock "2017-10-31T16:00:00Z"))
          (t/zoned-date-time "2017-10-31T16:00:00Z[UTC]"))))
  (is
    (t/<
      (t/now)
      (t/>> (t/now) (t/new-duration 10 :seconds))
      (t/>> (t/now) (t/new-duration 20 :seconds))))
  (is
    (t/>
      (t/>> (t/now) (t/new-duration 20 :seconds))
      (t/>> (t/now) (t/new-duration 10 :seconds))
      (t/now)))
  (is (not
        (t/<
          (t/now)
          (t/>> (t/now) (t/new-duration 20 :seconds))
          (t/>> (t/now) (t/new-duration 10 :seconds)))))
  (let [at (t/now)]
    (is (t/<= at at (t/>> at (t/new-duration 1 :seconds))))
    (is (t/>= at at (t/<< at (t/new-duration 10 :seconds)))))

  (testing "durations"
    (is (t/> (t/new-duration 20 :seconds) (t/new-duration 10 :seconds)))
    (is (t/>= (t/new-duration 20 :seconds) (t/new-duration 20 :seconds)))
    (is (t/< (t/new-duration 10 :seconds) (t/new-duration 20 :seconds)))
    (is (t/<= (t/new-duration 20 :seconds) (t/new-duration 20 :seconds)))))


(deftest comparison-test-date
  (let [t1 #inst "2019-12-24"
        t2 #inst "2019-12-31"]

    (is (t/< t1 t2))
    (is (not (t/< t1 t1)))
    (is (not (t/< t2 t1)))

    (is (t/<= t1 t2))
    (is (t/<= t1 t1))
    (is (not (t/<= t2 t1)))

    (is (not (t/> t1 t2)))
    (is (not (t/> t1 t1)))
    (is (t/> t2 t1))

    (is (not (t/>= t1 t2)))
    (is (t/>= t1 t1))
    (is (t/>= t2 t1))))


(deftest day-of-week
  (let [days (fn [strings] (map t/day-of-week strings))]
    (is (every? #{cljc.java-time.day-of-week/sunday} (days ["sun" "sunday"])))
    (is (every? #{cljc.java-time.day-of-week/monday} (days ["mon" "monday"])))
    (is (every? #{cljc.java-time.day-of-week/tuesday} (days ["tue" "tues" "tuesday"])))
    (is (every? #{cljc.java-time.day-of-week/wednesday} (days ["wed" "weds" "wednesday"])))
    (is (every? #{cljc.java-time.day-of-week/thursday} (days ["thur" "thurs" "thursday"])))
    (is (every? #{cljc.java-time.day-of-week/friday} (days ["fri" "friday"])))
    (is (every? #{cljc.java-time.day-of-week/saturday} (days ["sat" "saturday"])))))

(deftest adjusters
  (is (= (t/date "2022-01-15")
         (t/day-of-week-in-month (t/date "2022-01-01") 3 t/SATURDAY)))
  (is (= (t/date "2022-01-01")
         (t/first-in-month (t/date "2022-01-01") t/SATURDAY)))
  (is (= (t/date "2022-01-29")
         (t/last-in-month (t/date "2022-01-01") t/SATURDAY)))
  (is (= (t/date "2022-01-08")
         (t/next (t/date "2022-01-01") t/SATURDAY)))
  (is (= (t/date "2022-01-01")
         (t/next-or-same (t/date "2022-01-01") t/SATURDAY)))
  (is (= (t/date "2021-12-25")
         (t/previous (t/date "2022-01-01") t/SATURDAY)))
  (is (= (t/date "2022-01-01")
         (t/previous-or-same (t/date "2022-01-01") t/SATURDAY))))

(deftest month
  (let [months (fn [strings] (map t/month strings))]
    (is (every? #{cljc.java-time.month/january} (months ["jan" "january"])))
    (is (every? #{cljc.java-time.month/february} (months ["feb" "february"])))
    (is (every? #{cljc.java-time.month/march} (months ["mar" "march"])))
    (is (every? #{cljc.java-time.month/april} (months ["apr" "april"])))
    (is (every? #{cljc.java-time.month/may} (months ["may"])))
    (is (every? #{cljc.java-time.month/june} (months ["jun" "june"])))
    (is (every? #{cljc.java-time.month/july} (months ["jul" "july"])))
    (is (every? #{cljc.java-time.month/august} (months ["aug" "august"])))
    (is (every? #{cljc.java-time.month/september} (months ["sep" "september"])))
    (is (every? #{cljc.java-time.month/october} (months ["oct" "october"])))
    (is (every? #{cljc.java-time.month/november} (months ["nov" "november"])))
    (is (every? #{cljc.java-time.month/december} (months ["dec" "december"])))))

;; Durations. Simple constructors to create durations of specific
;; units.

(deftest duration-test
  (is (= (t/new-duration 1e6 :nanos) (t/new-duration 1 :millis)))
  (is (= (t/new-duration 1e9 :nanos) (t/new-duration 1 :seconds)))
  (is (= (t/new-duration 1000 :millis) (t/new-duration 1 :seconds)))

  (is (= (t/new-duration 24 :hours) (t/duration (t/tomorrow)))))

;; Durations. Convenience functions to create durations of specific
;; units.
(deftest duration-functions-test
  (is (= (t/of-nanos 10) (t/new-duration 10 :nanos)))
  (is (= (t/of-micros 10) (t/new-duration 10 :micros))) ;java.time.Duration doesn't have ofMicros method
  (is (= (t/of-millis 10) (t/new-duration 10 :millis)))
  (is (= (t/of-seconds 10) (t/new-duration 10 :seconds)))
  (is (= (t/of-minutes 10) (t/new-duration 10 :minutes)))
  (is (= (t/of-hours 10) (t/new-duration 10 :hours))))


;; Periods. Convenience functions to create periods of specific
;; units.
(deftest period-functions-test
  (is (= (t/of-days 10) (t/new-period 10 :days)))
  (is (= (t/of-months 10) (t/new-period 10 :months)))
  (is (= (t/of-years 10) (t/new-period 10 :years))))


(deftest predicates-test
  (is (true? (t/clock? (t/clock))))
  (is (true? (t/day-of-week? t/MONDAY)))
  (is (true? (t/duration? (t/new-duration 1 :minutes))))
  (is (true? (t/instant? (t/instant))))
  (is (true? (t/date? (t/today))))
  (is (true? (t/date-time? (t/at (t/today) (t/new-time 0 0)))))
  (is (true? (t/time? (t/new-time 0 0))))
  (is (true? (t/month? t/MAY)))
  (is (true? (t/offset-date-time? (t/offset-date-time))))
  (is (true? (t/period? (t/new-period 1 :weeks))))
  (is (true? (t/year? (t/year))))
  (is (true? (t/year-month? (t/year-month))))
  (is (true? (t/zone? (t/zone))))
  (is (true? (t/zone-offset? (t/zone-offset (t/zoned-date-time)))))
  (is (true? (t/zoned-date-time? (t/zoned-date-time))))
  (is (false? (t/date? 16)))
  (is (false? (t/month? 16))))

(deftest in-test
  (is (= (t/zoned-date-time "2021-04-23T11:23:24.576270-04:00[America/Toronto]")
         (t/in (t/instant "2021-04-23T15:23:24.576270Z")
               (t/zone "America/Toronto"))))
  (is (= (t/zoned-date-time "2021-04-23T11:18:46.594720-04:00[America/Toronto]")
         (t/in (t/offset-date-time "2021-04-23T13:18:46.594720-02:00")
               (t/zone "America/Toronto"))))
  (is (= (t/zoned-date-time "2021-04-23T11:18:46.594720-04:00[America/Toronto]")
         (t/in (t/zoned-date-time "2021-04-23T08:18:46.594720-07:00[America/Los_Angeles]")
               (t/zone "America/Toronto")))))

(deftest divide-test
  (is
    ;; Duration -> Long -> Duration
    (= (t/new-duration 6 :hours) (t/divide (t/new-duration 6 :days) 24))
    ;; Duration -> Duration -> Long
    (= 63 (t/divide (t/new-duration 21 :days) (t/new-duration 8 :hours)))))
