;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.api-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer :all]
   [tick.alpha.api :as t]))

(s/check-asserts true)

;; Durations. Simple constructors to create durations of specific
;; units.

(deftest periods-test
  (is (= (t/nanos 1e6) (t/millis 1)))
  (is (= (t/nanos 1e9) (t/seconds 1)))
  (is (= (t/millis 1000) (t/seconds 1)))
  (is (= (t/weeks 2) (t/days 14))))

;; Period arithmetic

(deftest addition-test
  (is (= (t/seconds 5) (t/+ (t/seconds 2) (t/seconds 3))))
  (is (= (t/minutes 2) (t/+ (t/seconds 90) (t/seconds 30)))))

(deftest subtraction-test
  (is (= (t/seconds 3) (t/- (t/seconds 5) (t/seconds 2)))))

;; Constructors

(deftest today-test
  (t/with-clock (java.time.Clock/fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= (t/date "2017-08-08") (t/today)))
    (is (= (t/date "2017-08-07") (t/yesterday)))
    (is (= (t/date "2017-08-09") (t/tomorrow)))))

(deftest now-test
  (t/with-clock (java.time.Clock/fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= (t/time "2017-08-08T12:00:00Z") (t/now)))))

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

;; Imagine we have some holidays defined as a pair of insts, in the
;; London timezone. Here we define a holiday between Monday 31st July
;; and Friday 11th August (inclusive).

(let [holidays [;; 31st July to 11th August inclusive
                [#inst "2017-07-30T23:00" #inst "2017-08-11T23:00" "Sicily"]
                ;; 21st to 24th April inclusive
                [#inst "2017-04-24T23:00" #inst "2017-04-20T23:00" "Isle of Wight"]]]
  (->> holidays
       (map t/interval)
       (map #(t/localtime % t/LONDON))
       (map t/group-by-date)
       ))

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
