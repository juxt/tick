;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.api-test
  (:require
    [clojure.spec.alpha :as s]
    #?(:clj [clojure.test :refer :all]
       :cljs [cljs.test :refer-macros [deftest is testing run-tests]])
    [tick.alpha.api :as t]
    [tick.interop :as t.i]
    #?(:clj
    [tick.deprecated.cal :as cal])
    #?(:cljs
       [java.time :refer [Date Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime ChronoUnit ChronoField TemporalAdjusters Temporal TemporalAmount]]))
  #?(:clj
     (:import
       [java.util Date]
       [java.time Clock Instant Duration Period ZoneId LocalDate LocalTime LocalDateTime Year YearMonth Month OffsetDateTime ZoneId ZonedDateTime]
       [java.time.temporal Temporal TemporalAmount])))

(s/check-asserts true)

;; Constructor test

(deftest constructor-test
  (is (= Year (type (t/year 2017))))
  (is (= 2017 (.getValue (t/year 2017))))
  (is (= Month (type (t/month 12))))
  (is (= t/DECEMBER (t/month 12))))

;; Point-in-time tests
(deftest today-test
  (t/with-clock (. Clock fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= #jsr310/instant "2017-08-08T12:00:00Z" (t/now)))
    (is (= (t/date "2017-08-08") (t/today)))
    (is (= (t/date "2017-08-07") (t/yesterday)))
    (is (= (t/date "2017-08-09") (t/tomorrow)))
    (is (= 8 (t/day-of-month (t/today))))
    (is (= 2017 (t/int (t/year))))
    (is (= (t/date-time "2017-08-08T12:00:00") (t/noon (t/today))))
    (is (= (t/date-time "2017-08-08T00:00:00") (t/midnight (t/today))))))

(deftest offset-date-time-test
  (let [t "2018-09-24T18:57:08.996+01:00"
        instance-type #?(:clj OffsetDateTime
                         :cljs ZonedDateTime)]
    (testing "is zdt in cljs, odt in clj - because odt doesnt yet exist in js-joda"
      (is (instance? instance-type (t/parse t)))
      (is (instance? instance-type (t/offset-date-time (t/now))))
      (is (instance? instance-type (t/offset-date-time t)))
      (is (instance? instance-type (t/offset-date-time (t/date-time))))
      (is (instance? instance-type (t/offset-date-time (t/zoned-date-time)))))))

(deftest formatting-test 
  (let [d "3030-05-03"]
    (is (= d (t/format :iso-local-date (t/parse d))))
    (is (= d (t/format (t/formatter :iso-local-date) (t/parse d))))
    (is (= d (t/format (t/formatter "YYYY-MM-dd") (t/parse d))))
    #?(:clj
       (is (= "3030-mai-03" (t/format (t/formatter "YYYY-MMM-dd" java.util.Locale/FRENCH) (t/parse d)))))))

(deftest epoch-test
  (is (= (. Instant parse "1970-01-01T00:00:00Z") (t/epoch))))

;; Period arithmetic

(deftest addition-test
  (is (= (t/new-duration 5 :seconds) (t/+ (t/new-duration 2 :seconds) (t/new-duration 3 :seconds))))
  (is (= (t/new-duration 2 :minutes) (t/+ (t/new-duration 90 :seconds) (t/new-duration 30 :seconds)))))

(deftest subtraction-test
  (is (= (t/new-duration 3 :seconds) (t/- (t/new-duration 5 :seconds) (t/new-duration 2 :seconds)))))

;; Range test

(deftest range-test
  (is (t/midnight? (t/beginning (t/today))))
  (is (t/midnight? (t/end (t/today))))
  (is (t/midnight? (t/beginning (t/year))))
  (is (t/midnight? (t/end (t/year)))))

;; Units test

(deftest units-test
  (is
    (=
      {:years 10, :months 0, :days 0}
      (t/units (t/new-period 10 :years)))))

;; Comparison test

(deftest comparison-test
  (is
    (t/<
      (t/now)
      (t/+ (t/now) (t/new-duration 10 :seconds))
      (t/+ (t/now) (t/new-duration 20 :seconds))))
  (is
    (t/>
      (t/+ (t/now) (t/new-duration 20 :seconds))
      (t/+ (t/now) (t/new-duration 10 :seconds))
      (t/now)))
  (is (not
        (t/<
          (t/now)
          (t/+ (t/now) (t/new-duration 20 :seconds))
          (t/+ (t/now) (t/new-duration 10 :seconds)))))
  (is (t/<= (t/now) (t/now) (t/+ (t/now) (t/new-duration 1 :seconds))))
  (is (t/>= (t/now) (t/now) (t/- (t/now) (t/new-duration 10 :seconds)))))

(deftest am-test
  (t/with-clock (. Clock fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= (t/new-interval (t/date-time "2017-08-08T00:00:00")
             (t/date-time "2017-08-08T12:00:00"))
          (t/am (t/today))))
    (is (= (t/new-interval (t/date-time "2017-08-08T12:00:00")
             (t/date-time "2017-08-09T00:00:00"))
          (t/pm (t/today))))))

;; Durations. Simple constructors to create durations of specific
;; units.

(deftest duration-test
  (is (= (t/new-duration 1e6 :nanos) (t/new-duration 1 :millis)))
  (is (= (t/new-duration 1e9 :nanos) (t/new-duration 1 :seconds)))
  (is (= (t/new-duration 1000 :millis) (t/new-duration 1 :seconds)))

  (is (= (t/new-duration 24 :hours) (t/duration (t/tomorrow)))))

;; TODO: Interval testing

(deftest division-test
  (is (= 365 (count (t/divide-by t/date (t/year 2017)))))
  (is (= 12 (count (t/divide-by t/year-month (t/year 2017)))))
  (is (= 30 (count (t/divide-by t/date "2017-09"))))
  (is (= (t/date "2017-09-01") (first (t/divide-by t/date "2017-09"))))
  (is (= (t/date "2017-09-30") (last (t/divide-by t/date "2017-09"))))
  (is (= 31 (count (t/divide-by t/date "2017-10"))))
  (is (= 8 (count (t/divide-by t/date (t/bounds "2017-10-03" "2017-10-10")))))
  (is (= [(t/date "2017-09-10")] (t/divide-by t/date (t/bounds "2017-09-10T12:00" "2017-09-10T14:00"))))
  (is (= [(t/date "2017-09-10") (t/date "2017-09-11")] (t/divide-by t/date (t/bounds "2017-09-10T12:00" "2017-09-11T14:00"))))
  (is (= 2 (count (t/divide-by t/year-month (t/bounds "2017-09-10" "2017-10-10")))))
  (is (= 3 (count (t/divide-by t/year (t/bounds "2017-09-10T12:00" "2019")))))
  (is (= 3 (count (t/divide-by t/year (t/bounds "2017-09-10T12:00" "2019-02")))))
  (is (= 24 (count (t/divide-by (t/new-duration 1 :hours) (t/date "2017-09-10"))))))

;; TODO: Divide by duration

;; Concur test

(deftest concur-test
  (is
    (= 2
       (t/hours
         (t/duration
           (t/concur (t/new-interval (t/at (t/today) "16:00")
                                     (t/end (t/today)))
                     (t/today)
                     (t/new-interval (t/at (t/today) "20:00")
                                     (t/at (t/today) "22:00"))))))))

;; Let's count some days over Easter 2017.
#?(:clj                                 ;cal not in cljs
   (deftest holiday-counting-test
     (is (=
           ;; Easter is mid-April
           (t/date "2017-04-16")
           (cal/easter-sunday 2017)))

     (is (= t/TUESDAY (t/day-of-week (t/date "2017-04-11"))))
     (is (= t/WEDNESDAY (t/day-of-week (t/date "2017-04-19"))))

     ;; Let's take a vacation
     (let [vacation (t/bounds (t/date "2017-04-11") (t/date "2017-04-19"))]
       ;; This is normally 9 days
       (is (= 9 (t/days (t/duration vacation))))

       (let [year (t/year 2017)
             public-holidays (map :date (cal/holidays-in-england-and-wales year))
             ;;_ (println "public-holidays:" public-holidays)
             weekends (filter cal/weekend? (t/divide-by t/date year))
             ;;_ (println "weekends:" weekends)
             non-working-days (t/union public-holidays weekends)
             vacation-set (t/difference [vacation] non-working-days)
             working-days-absent (t/days (apply t/+ (map t/duration vacation-set)))]

         ;; But really it's just the Tues, Weds, Thurs of the first week (because Good Friday is a holiday in England, and Monday being a bank holiday, just the following Tuesday and Wednesday count. That totals 5.
         (is (= 5 working-days-absent))

         ;; OK, now we want to display the vacation on a monthly calendar,
         ;; and want to determine what to display for Thursday 13th April. Does our vacation include this Thursday?
         (let [day (t/date "2017-04-13")]
           (is (= t/THURSDAY (t/day-of-week day)))
           ;; Intersections are powerful. If we only booked off a
           ;; half-day on this thursday, the intersection would return
           ;; this half-day.
           (is (not-empty (t/intersection [day] vacation-set))))

         ;; Another use of intersections can be to determine the number of vacation days taken in 2017. But let's make things a little harder by booking time over the new year period.
         (let [festive-break (t/bounds (t/date "2017-12-20") (t/date "2018-01-07"))

               vacation-set
               (-> vacation-set ; start with our original vacation set
                   ;; Let's add this to our vacation set
                   (t/union vacation-set [festive-break])
                   ;; We'll subtract the actual public holidays and
                   ;; weekends, these shouldn't be deducted from the
                   ;; number of vacation days.
                   (t/difference non-working-days)
                   ;; We only want to know the total vacation day count for 2017
                   (t/intersection [(t/bounds "2017")]))]

           ;; The festive vacation days in 2017 are 20th, 21th, 22nd, 27th, 28th, 29th.
           ;; Added to the 5 days we've already taken, that makes 11
           (is (= (+ 5 6) (t/days (apply t/+ (map t/duration vacation-set))))))))))

#_(t/partition-by-date)

#_((t/at-zone (t/new-interval [#inst "2017-04-24T23:00" #inst "2017-04-20T23:00"]) "Europe/London" ))

#_(t/new-interval [#inst "2017-07-30T23:00" #inst "2017-08-11T23:00"])

#_(t/partition-by-date
    (t/new-interval [#inst "2017-07-30T23:00" #inst "2017-08-11T23:00"])
    )

#_(t/new-interval [#inst "2017-04-24T23:00" #inst "2017-04-20T23:00"])

#_(t/dates (t/new-interval "2017-09-10T12:00" "2017-09-11T14:00"))

#_(t/new-interval "2017-09-10T12:00" "2017-09-10T14:00")

#_(t/dates (t/new-interval "2017-09-10T12:00" "2017-09-10T14:00"))


#_(t/time "2017-08-19T02:00:00Z")

#_(defn inst-at-hour [date hour]
    (Date/from (.toInstant (.atZone (.atTime date hour 0) LONDON))))


#_(deftest partition-by-date-test

    )


#_(dates (new-interval (t/year-month "2017-12") (t/year-month "2018-03")))

#_(t/time "2017-07-30T12:00:00")

#_(t/date (t/at (t/date "2017-07-30") (t/time "4pm")))

;;(t/midnight? (t/date (t/at (t/date "2017-07-30") (t/time "4pm"))))

#_(new-interval (t/date "2017-07-30")
    (t/date "2017-08-11"))

#_(new-interval (t/at (t/date "2017-07-30") (t/time "4pm"))
    (t/date "2017-08-11"))

#_(dates (new-interval (t/at (t/date "2017-07-30") (t/time "4pm"))
           (t/date "2017-08-11")))

#_(sort-by first
    (group-by-date
      (new-interval (t/at (t/date "2017-07-30") (t/time "4pm"))
        (t/date "2017-08-11"))))

#_(t/beginning
    (t/year-month "2017-09"))

#_(new-interval
    (t/beginning
      (t/year-month "2017-09"))
    (t/end
      (t/year-month "2017-09")))



#_(t/new-interval "2017-09-10T14:00" "2017-10-30T08:00")

#_(t/inc (t/date (second (t/new-interval "2017-09-10T14:00" "2017-10-30T08:00"))))

#_(t/year-months (t/new-interval "2017-09-10T14:00" "2017-10-30T08:00"))

#_(t/group-by t/dates (t/new-interval "2017-09-10T14:00" "2017-10-30T08:00"))

#_(t/group-by t/dates "2017-10")

#_(t/dates (t/year-month "2017-09"))

#_(t/partition-by-date (t/new-interval (t/year-month "2017-09")))

#_(t/group-by-date (t/new-interval (t/year-month "2017-09")))

#_(t/new-interval (t/new-interval (t/year-month "2017-09")))

#_(tick.interval/dates (t/new-interval (t/year-month "2017-09")))


;;(partition-by-date )


#_(-> (t/year-month "2017-09") t/new-interval second t/year-month t/inc t/beginning)

#_(t/beginning (second (t/new-interval "2017-09")))


#_(t/year-months (t/new-interval "2017-09"))

#_(t/dates (t/new-interval "2017-09"))

#_(range 10 10)

;; Here's a fun test that combines tick's calendar to calculate the
;; number of working days in 2017 (252). It demonstrates how to use union to combine sets of intervals together, and intersection to remove non-working-days from the year.
#?(:clj ;cal not in cljs
   (deftest working-days-in-a-year-test
     (let [year (t/year 2017)
           holidays (map (comp t/bounds :date) (cal/holidays-in-england-and-wales year))
        weekends (map t/bounds (filter cal/weekend? (t/divide-by t/date year)))]
       (is (t/ordered-disjoint-intervals? holidays))
       (is (t/ordered-disjoint-intervals? weekends))
       (let [non-working-days (t/union holidays weekends)]
         (is (t/ordered-disjoint-intervals? non-working-days))
         (is (= 113 (count non-working-days)))
      (is (= 365 (count (t/divide-by t/date (t/year 2017)))))

         ;; 252 is the number of working days in 2017 in England & Wales.
         ;; We can calculate this by subtracting the number of holidays, by count, and by using intersection.
      (is (= 252 (- 365 113)))
      (is (= 252 (t/days (reduce t/+ (map t/duration (t/difference [(t/bounds year)] non-working-days))))))))))


;; Do not disturb tests

;; Example: We mustn't disturb people between 10pm and 7am the following morning, in their locale.

(defn moment [t]
  (t/new-interval
    t
    (t/+ t (t/new-duration 3 :seconds))))

;; TODO: Think about conversions between single instants and intervals. Feather? Widen? Smudge?

;; Can we disturb?
(deftest cannot-disturb-test
  (let
      [disturb-interval [(t/new-interval (t/time "07:00") (t/time "22:00"))]
       no-disturb-interval (t/complement disturb-interval)
       can-disturb? (fn [t] (not (some #(t/coincident? % t) no-disturb-interval)))
       ]
      (is (not (can-disturb? (t/time "3:00"))))
      (is (not (can-disturb? (t/time "7:00"))))
      (is (can-disturb? (t/time "7:01")))
      (is (can-disturb? (t/time "12:00")))
      (is (can-disturb? (t/time "21:59")))
      (is (not (can-disturb? (t/time "22:00"))))
      (is (not (can-disturb? (t/time "00:00"))))))



;; TODO: An 'on' test with an interval
#_(t/on
  (t/new-interval (t/time "07:00") (t/time "22:00"))
  (t/today))


;; Weekend
#_(let
    [disturb-interval [(t/new-interval (t/time "07:00") (t/time "21:00"))]
     no-disturb-interval (t/complement disturb-interval)
     t (t/time "6:00")]
  (not (some #(t/coincident? % t) no-disturb-interval)))


#_(t/complement [(t/new-interval (t/time "07:00") (t/time "22:00"))])

#_(t/complement [(t/new-interval (t/time "07:00") (t/time "22:00"))])



#_(not (some (partial t/concur (moment (t/time "21:59:57")))
           (t/complement [(t/new-interval (t/time "07:00") (t/time "22:00"))])))

#_(t/at-zone (t/midnight (t/today)) "Europe/Berlin")


#_(t/time (t/as-local (t/now) "Europe/Berlin"))


#_(t/complement [(t/new-interval (t/time "07:00") (t/time "22:00"))])


#_(t/complement [(t/new-interval (t/time "07:00") (t/time "22:00"))])



#_(tick.interval/concur?
  (t/complement [(t/new-interval (t/time "07:00") (t/time "22:00"))])
  (moment (t/time (t/as-local (t/now) "Europe/Berlin"))))



#_(.toLocalTime (t/as-local (t/now) "Europe/Berlin"))

#_(t/time)


;; TODO: 'Ago' tests


;; Did something happen in the last ten minutes


;;(t/between (t/now) (t/now))



;; TODO: interval construction: since, until


;; I think we should make an interval type

;; TODO: +/- should NOT work for intervals, because +/- mean to 'add' an interval to another to make an interval set/seq.

;; (t/>> (tick.interval/as-interval (t/today)) (t/minutes 4))
