;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.api-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer :all]
   [tick.alpha.api :as t]
   [tick.cal :as cal]))

(s/check-asserts true)

;; Point-in-time tests

(deftest today-test
  (t/with-clock (java.time.Clock/fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= (t/time "2017-08-08T12:00:00Z") (t/now)))
    (is (= (t/date "2017-08-08") (t/today)))
    (is (= (t/date "2017-08-07") (t/yesterday)))
    (is (= (t/date "2017-08-09") (t/tomorrow)))
    (is (= 2017 (t/int (t/year))))
    (is (= (t/time "2017-08-08T12:00:00") (t/noon (t/today))))
    (is (= (t/time "2017-08-08T00:00:00") (t/midnight (t/today))))))

;; Durations. Simple constructors to create durations of specific
;; units.

(deftest periods-test
  (is (= (t/nanos 1e6) (t/millis 1)))
  (is (= (t/nanos 1e9) (t/seconds 1)))
  (is (= (t/millis 1000) (t/seconds 1)))
  (is (= (t/weeks 2) (t/days 14)))
  (is (= (t/years 3) (t/months 36))))

;; Period arithmetic

(deftest addition-test
  (is (= (t/seconds 5) (t/+ (t/seconds 2) (t/seconds 3))))
  (is (= (t/minutes 2) (t/+ (t/seconds 90) (t/seconds 30)))))

(deftest subtraction-test
  (is (= (t/seconds 3) (t/- (t/seconds 5) (t/seconds 2)))))

;; Range test

(deftest range-test
  (is (t/midnight? (t/start (t/today))))
  (is (t/midnight? (t/end (t/today))))
  (is (t/midnight? (t/start (t/year))))
  (is (t/midnight? (t/end (t/year)))))

;; Comparison test

(deftest comparison-test
  (is
   (t/<
    (t/now)
    (t/+ (t/now) (t/seconds 10))
    (t/+ (t/now) (t/seconds 20))))
  (is
   (t/>
    (t/+ (t/now) (t/seconds 20))
    (t/+ (t/now) (t/seconds 10))
    (t/now)))
  (is (not
       (t/<
        (t/now)
        (t/+ (t/now) (t/seconds 20))
        (t/+ (t/now) (t/seconds 10))))))

;; AM/PM

(deftest am-test
  (t/with-clock (java.time.Clock/fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= [(t/time "2017-08-08T00:00:00")
            (t/time "2017-08-08T12:00:00")]
           (t/am (t/today))))
    (is (= [(t/time "2017-08-08T12:00:00")
            (t/time "2017-08-09T00:00:00")]
           (t/pm (t/today))))))

;; Duration test

(deftest duration-test
  (is (= 24 (t/hours (t/duration (t/tomorrow))))))

;; Dates test

(deftest dates-over-test
  (is (= 30 (count (t/dates-over "2017-09"))))
  (is (= (t/date "2017-09-01") (first (t/dates-over "2017-09"))))
  (is (= (t/date "2017-09-30") (last (t/dates-over "2017-09"))))
  (is (= 31 (count (t/dates-over "2017-10"))))
  (is (= 8 (count (t/dates-over (t/interval "2017-10-03" "2017-10-10")))))
  (is (= [(t/date "2017-09-10")] (t/dates-over (t/interval "2017-09-10T12:00" "2017-09-10T14:00"))))
  (is (= [(t/date "2017-09-10") (t/date "2017-09-11")] (t/dates-over (t/interval "2017-09-10T12:00" "2017-09-11T14:00"))))
  (is (= 2 (count (t/year-months-over (t/interval "2017-09-10" "2017-10-10")))))
  (is (= 3 (count (t/years-over (t/interval "2017-09-10T12:00" "2019")))))
  (is (= 3 (count (t/years-over (t/interval "2017-09-10T12:00" "2019-02"))))))

;; Let's count some days over Easter 2017.

(deftest holiday-counting-test
  (is (=
       ;; Easter is mid-April
       (t/date "2017-04-16")
       (cal/easter-sunday 2017)))

  (is (= t/tuesday (t/day (t/date "2017-04-11"))))
  (is (= t/wednesday (t/day (t/date "2017-04-19"))))

  ;; Let's take a vacation
  (let [vacation (t/interval (t/date "2017-04-11") (t/date "2017-04-19"))]
    ;; This is normally 9 days
    (is (= 9 (t/days (t/duration vacation))))

    (let [year (t/year 2017)
          public-holidays (map (comp t/interval :date) (cal/holidays-in-england-and-wales year))
          weekends (map t/interval (filter cal/weekend? (t/dates-over (t/interval year))))
          non-working-days (t/union public-holidays weekends)
          vacation-set (t/difference [vacation] non-working-days)
          working-days-absent (t/days (apply t/+ (map t/duration vacation-set)))]

      ;; But really it's just the Tues, Weds, Thurs of the first week (because Good Friday is a holiday in England, and Monday being a bank holiday, just the following Tuesday and Wednesday count. That totals 5.
      (is (= 5 working-days-absent))

      ;; OK, now we want to display the vacation on a monthly calendar,
      ;; and want to determine what to display for Thursday 13th April. Does our vacation include this Thursday?
      (let [day (t/date "2017-04-13")]
        (is (= t/thursday (t/day day)))
        ;; Intersections are powerful. If we only booked off a
        ;; half-day on this thursday, the intersection would return
        ;; this half-day.
        (is (not-empty (t/intersection [(t/interval day)] vacation-set))))

      ;; Another use of intersections can be to determine the number of vacation days taken in 2017. But let's make things a little harder by booking time over the new year period.
      (let [festive-break (t/interval (t/date "2017-12-20") (t/date "2018-01-07"))

            vacation-set
            (-> vacation-set ; start with our original vacation set
                ;; Let's add this to our vacation set
                (t/union vacation-set [festive-break])
                ;; We'll subtract the actual public holidays and
                ;; weekends, these shouldn't be deducted from the
                ;; number of vacation days.
                (t/difference non-working-days)
                ;; We only want to know the total vacation day count for 2017
                (t/intersection [(t/interval "2017")]))]

        ;; The festive vacation days in 2017 are 20th, 21th, 22nd, 27th, 28th, 29th.
        ;; Added to the 5 days we've already taken, that makes 11
        (is (= (+ 5 6) (t/days (apply t/+ (map t/duration vacation-set)))))))))

#_(t/partition-by-date)

#_((t/at-zone (t/interval [#inst "2017-04-24T23:00" #inst "2017-04-20T23:00"]) "Europe/London" ))

#_(t/interval [#inst "2017-07-30T23:00" #inst "2017-08-11T23:00"])

#_(t/partition-by-date
 (t/interval [#inst "2017-07-30T23:00" #inst "2017-08-11T23:00"])
 )

#_(t/interval [#inst "2017-04-24T23:00" #inst "2017-04-20T23:00"])

#_(t/dates (t/interval "2017-09-10T12:00" "2017-09-11T14:00"))

#_(t/interval "2017-09-10T12:00" "2017-09-10T14:00")

#_(t/dates (t/interval "2017-09-10T12:00" "2017-09-10T14:00"))


#_(t/time "2017-08-19T02:00:00Z")

#_(defn inst-at-hour [date hour]
  (Date/from (.toInstant (.atZone (.atTime date hour 0) LONDON))))


#_(deftest partition-by-date-test

  )


#_(dates (interval (t/year-month "2017-12") (t/year-month "2018-03")))

#_(t/time "2017-07-30T12:00:00")

#_(t/date (t/at (t/date "2017-07-30") (t/time "4pm")))

;;(t/midnight? (t/date (t/at (t/date "2017-07-30") (t/time "4pm"))))

#_(interval (t/date "2017-07-30")
          (t/date "2017-08-11"))

#_(interval (t/at (t/date "2017-07-30") (t/time "4pm"))
        (t/date "2017-08-11"))

#_(dates (interval (t/at (t/date "2017-07-30") (t/time "4pm"))
                 (t/date "2017-08-11")))

#_(sort-by first
         (group-by-date
          (interval (t/at (t/date "2017-07-30") (t/time "4pm"))
                    (t/date "2017-08-11"))))

#_(t/start
 (t/year-month "2017-09"))

#_(interval
 (t/start
  (t/year-month "2017-09"))
 (t/end
  (t/year-month "2017-09")))



#_(t/interval "2017-09-10T14:00" "2017-10-30T08:00")

#_(t/inc (t/date (second (t/interval "2017-09-10T14:00" "2017-10-30T08:00"))))

#_(t/year-months (t/interval "2017-09-10T14:00" "2017-10-30T08:00"))

#_(t/group-by t/dates (t/interval "2017-09-10T14:00" "2017-10-30T08:00"))

#_(t/group-by t/dates "2017-10")

#_(t/dates (t/year-month "2017-09"))

#_(t/partition-by-date (t/interval (t/year-month "2017-09")))

#_(t/group-by-date (t/interval (t/year-month "2017-09")))

#_(t/interval (t/interval (t/year-month "2017-09")))

#_(tick.interval/dates (t/interval (t/year-month "2017-09")))


;;(partition-by-date )


#_(-> (t/year-month "2017-09") t/interval second t/year-month t/inc t/start)

#_(t/start (second (t/interval "2017-09")))


#_(t/year-months (t/interval "2017-09"))

#_(t/dates (t/interval "2017-09"))

#_(range 10 10)

;; Here's a fun test that combines tick's calendar to calculate the
;; number of working days in 2017 (252). It demonstrates how to use union to combine sets of intervals together, and intersection to remove non-working-days from the year.
(deftest working-days-in-a-year-test
  (let [year (t/year 2017)
        holidays (map (comp t/interval :date) (cal/holidays-in-england-and-wales year))
        weekends (map t/interval (filter cal/weekend? (t/dates-over (t/interval year))))]
    (is (t/ordered-disjoint-intervals? holidays))
    (is (t/ordered-disjoint-intervals? weekends))
    (let [non-working-days (t/union holidays weekends)]
      (is (t/ordered-disjoint-intervals? non-working-days))
      (is (= 113 (count non-working-days)))
      (is (= 365 (count (t/dates-over (t/interval (t/year 2017))))))

      ;; 252 is the number of working days in 2017 in England & Wales.
      ;; We can calculate this by subtracting the number of holidays, by count, and by using intersection.
      (is (= 252 (- 365 113)))
      (is (= 252 (t/days (reduce t/+ (map t/duration (t/difference [(t/interval (t/year 2017))] non-working-days)))))))))
