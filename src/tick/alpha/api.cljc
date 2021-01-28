;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.api
  (:refer-clojure
   :exclude [+ - * inc dec max min conj
             range time int long complement
             < <= > >= << >>
             extend
             atom swap! swap-vals! compare-and-set!
             reset! reset-vals!
             second
             group-by divide format] )
  (:require
    [tick.core :as core]
    [tick.format :as t.f]
    [tick.protocols :as p]
    [cljc.java-time.duration]
    [cljc.java-time.zone-offset]
    [cljc.java-time.day-of-week]
    [cljc.java-time.month]
    #?(:clj tick.file) ; To ensure protocol extension
    [tick.interval :as interval]
    #?@(:cljs
       [[java.time.format :refer [DateTimeFormatter]]]))
  #?(:cljs
     (:require-macros
       [tick.alpha.api :refer [with-clock]]))
  #?(:clj
     (:import
       [java.time LocalDate]
       [java.time.format DateTimeFormatter])))

;; This API is optimises convenience, API stability and (type) safety
;; over performance. Where performance is critical, use tick.core and
;; friends.

;; Construction

(def new-time core/new-time)
(def new-date core/new-date)
(def new-year-month core/new-year-month)

;; Surfacing some useful constants

(def unit-map core/unit-map)

;; Point-in-time 'demo' functions

(defn now [] (core/now))
(defn today [] (core/today))
(defn tomorrow [] (core/tomorrow))
(defn yesterday [] (core/yesterday))

;; Conversions, with 0-arity defaults

(defn time
  ([] (core/time (now)))
  ([v] (core/time v)))

(defn date
  ([] (today))
  ([v] (core/date v)))

(defn inst
  ([] (core/inst (now)))
  ([v] (core/inst v)))

(defn instant
  ([] (core/instant (now)))
  ([v] (core/instant v)))

(defn date-time
  ([] (core/date-time (now)))
  ([v] (core/date-time v)))

(defn offset-date-time
  ([] (core/offset-date-time (now)))
  ([v] (core/offset-date-time v)))

(defn zoned-date-time
  ([] (core/zoned-date-time (now)))
  ([v] (core/zoned-date-time v)))

;; Extraction

(defn nanosecond [t] (core/nanosecond t))
(defn microsecond [t] (core/microsecond t))
(defn millisecond [t] (core/millisecond t))
(defn second [t] (core/second t))
(defn minute [t] (core/minute t))
(defn hour [t] (core/hour t))

(defn day-of-week
  ([] (core/day-of-week (today)))
  ([v] (core/day-of-week v)))

(defn day-of-month
  ([] (core/day-of-month (today)))
  ([v] (core/day-of-month v)))

(defn month
  ([] (core/month (today)))
  ([v] (core/month v)))

(defn year
  ([] (core/year (today)))
  ([v] (core/year v)))

(defn year-month
  ([] (core/year-month (today)))
  ([v] (core/year-month v)))

(defn zone
  ([] (core/current-zone))
  ([z] (core/zone z)))

(defn zone-offset
  ([offset] (core/zone-offset offset))
  ([hours minutes] (cljc.java-time.zone-offset/of-hours-minutes hours minutes))
  ([hours minutes seconds] (cljc.java-time.zone-offset/of-hours-minutes-seconds hours minutes seconds)))

;; Reification

(defn on [t d] (p/on t (date d)))
(defn at [d t] (p/at d (time t)))
(defn in [ldt z] (p/in ldt (zone z)))
(defn offset-by [ldt offset] (p/offset-by ldt (zone-offset offset)))

;; Constants

(def MONDAY cljc.java-time.day-of-week/monday)
(def TUESDAY cljc.java-time.day-of-week/tuesday)
(def WEDNESDAY cljc.java-time.day-of-week/wednesday)
(def THURSDAY cljc.java-time.day-of-week/thursday)
(def FRIDAY cljc.java-time.day-of-week/friday)
(def SATURDAY cljc.java-time.day-of-week/saturday)
(def SUNDAY cljc.java-time.day-of-week/sunday)

(def JANUARY cljc.java-time.month/january )
(def FEBRUARY cljc.java-time.month/february)
(def MARCH cljc.java-time.month/march)
(def APRIL cljc.java-time.month/april)
(def MAY cljc.java-time.month/may)
(def JUNE cljc.java-time.month/june)
(def JULY cljc.java-time.month/july)
(def AUGUST cljc.java-time.month/august)
(def SEPTEMBER cljc.java-time.month/september)
(def OCTOBER cljc.java-time.month/october)
(def NOVEMBER cljc.java-time.month/november)
(def DECEMBER cljc.java-time.month/december)

(defn beginning [v] (core/beginning v))
(defn end [v] (core/end v))
(defn duration [v] (core/duration v))

(def coincident? core/coincident?)

(def noon core/noon)
(def midnight core/midnight)
(def midnight? core/midnight?)
(def epoch core/epoch)

(def fields core/fields)
(def with core/with)
(def ago core/ago)
(def hence core/hence)

;; Zones

(def UTC (zone "UTC"))
;(def LONDON (zone "Europe/London"))

;; Parsing

(def ^{:doc "Do not use this function if you know the expected format of the string
that you want to parse. This is partly because for example t/instant, t/date etc  will
be much faster, but also because if the string you pass it is not in the format you
expect, this function may still convert it into some entity that you weren't expecting.

If you have a string in a non-standard format, use a formatter and the parse fn of they entity you want.

For example:

(cljc.java-time.local-date/parse \"20200202\" (t/formatter \"yyyyMMdd\"))
"} parse p/parse)

;; Arithmetic
(defn inc [t]
  (core/inc t))

(defn dec [t]
  (core/dec t))

(defn >> [t amt]
  (core/>> t amt))

(defn << [t amt]
  (core/<< t amt))

(defn +
  "Sum amounts of time.

  Prefer >> for moving forward a date/time"
  ([] cljc.java-time.duration/zero)
  ([arg] arg)
  ([arg & args]
   (if (or (core/duration? arg) (core/period? arg))
     (reduce core/+ arg args)
     (do
       (assert (= 1 (count args)))
       (>> arg (first args))))))

(defn -
  "Subtract amounts of time.

  Prefer << for moving backward a date/time"
  ([] cljc.java-time.duration/zero)
  ([arg] (core/negated arg))
  ([arg & args]
   (if (or (core/duration? arg) (core/period? arg))
     (reduce core/- arg args)
     (do
       (assert (= 1 (count args)))
       (<< arg (first args))))))

(def max core/max)
(def min core/min)

(def min-of-type core/min-of-type)
(def max-of-type core/max-of-type)

(def range core/range)

(defn int [arg] (core/int arg))
(defn long [arg] (core/long arg))

;; Lengths of time (durations & periods)

(defn nanos [v] (core/nanos v))
(defn micros [v] (core/micros v))
(defn millis [v] (core/millis v))
(defn seconds [v] (core/seconds v))
(defn minutes [v] (core/minutes v))
(defn hours [v] (core/hours v))
(defn days [v] (core/days v))
(defn months [v] (core/months v))
(defn years [v] (core/years v))

;; Units
(def units core/units)

;; Truncation
(def truncate core/truncate)

;; Comparisons

(defn <
  ([_x] true)
  ([x y] (core/< x y))
  ([x y & more] (if (core/< x y)
                  (if (next more)
                    (recur y (first more) (next more))
                    (core/< y (first more)))
                  false)))

(defn <=
  ([_x] true)
  ([x y] (core/<= x y))
  ([x y & more] (if (core/<= x y)
                  (if (next more)
                    (recur y (first more) (next more))
                    (core/<= y (first more)))
                  false)))

(defn >
  ([_x] true)
  ([x y] (core/> x y))
  ([x y & more] (if (core/> x y)
                  (if (next more)
                    (recur y (first more) (next more))
                    (core/> y (first more)))
                  false)))

(defn >=
  ([_x] true)
  ([x y] (core/>= x y))
  ([x y & more] (if (core/>= x y)
                  (if (next more)
                    (recur y (first more) (next more))
                    (core/>= y (first more)))
                  false)))

;; TODO: Multiplication (of durations)

;; Clocks

;; Fixing the clock used for `today` and `now`.

(defn clock
  ([] (core/current-clock))
  ([i] (core/clock i)))

(defmacro with-clock [^java.time.Clock clock & body]
  `(binding [tick.core/*clock* (core/clock ~clock)]
     ~@body))

;(def tick core/tick)
(def atom core/atom)
(def swap! core/swap!)
(def swap-vals! core/swap-vals!)
(def compare-and-set! core/compare-and-set!)
(def reset! core/reset!)
(def reset-vals! core/reset-vals!)

;; Intervals

(defn new-interval [x y]
  (interval/new-interval x y))

(defn extend [ival & durations]
  (reduce interval/extend ival durations))

(defn scale [ival & durations]
  (reduce interval/extend ival durations))

(def ^{:doc "Return an interval which forms the bounding-box of the given arguments."}
  bounds interval/bounds)

(defn am [^LocalDate date] (interval/am date))
(defn pm [^LocalDate date] (interval/pm date))

(defn relation [i1 i2]
  (interval/relation i1 i2))

(defn new-duration
  [n u]
  (core/new-duration n u))

(defn new-period
  [n u]
  (core/new-period n u))

(defn between [v1 v2] (core/between v1 v2))

(defn concur
  ([] nil)
  ([x] x)
  ([x & args]
   (reduce interval/concur x args)))

(defn concurrencies [& intervals]
  (apply interval/concurrencies intervals))

;; Divisions

(defn divide-by [divisor t]
  (core/divide t divisor))

;; Alternative useful for -> threading
(defn divide [t divisor]
  (core/divide t divisor))

;; Temporal adjusters

#_(defn adjust [t adjuster]
  (core/adjust t adjuster))

;; Useful functions

#_(defn dates-over [interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (interval/dates-over interval)))

#_(defn year-months-over [interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (interval/year-months-over interval)))

#_(defn years-over [interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (interval/years-over interval)))

;; Note: Not sure about partition here for an individual interval. Should reserve for interval sets.

#_(defn segment-by [f interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (interval/segment-by f interval)))

#_(defn segment-by-date [interval]
  (segment-by interval/dates-over interval))

#_(defn group-segments-by [f interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (interval/group-segments-by f interval)))

#_(defn group-segments-by-date [interval]
  (group-segments-by interval/dates-over interval))

;; Interval sets

(def ordered-disjoint-intervals? interval/ordered-disjoint-intervals?)
(def unite interval/unite)
(def normalize interval/normalize)
(def union interval/union)
(def conj interval/conj)
(def intersection interval/intersection)
(def intersects? interval/intersects?)
(def difference interval/difference)
(def complement interval/complement)
(def group-by interval/group-by)

;; Formatting
(defn format
  ([o] (t.f/format o))
  ([fmt o]
    (t.f/format fmt o)))

(defn ^DateTimeFormatter formatter
  "Constructs a DateTimeFormatter out of either a

  * format string - \"YYYY/mm/DD\" \"YYY HH:MM\" etc.
  or
  * formatter name - :iso-instant :iso-date etc"
  ([fmt]
   (t.f/formatter fmt))
  ([fmt locale]
    (t.f/formatter fmt locale)))

(defn clock?
  "Return whether the provided value `v` is a clock"
  [v] (core/clock? v))
(defn day-of-week?
  "Return whether the provided value `v` is a day of the week"
  [v] (core/day-of-week? v))
(defn duration?
  "Return whether the provided value `v` is a duration"
  [v] (core/duration? v))
(defn instant?
  "Return whether the provided value `v` is an instant"
  [v] (core/instant? v))
(defn date?
  "Return whether the provided value `v` is a date"
  [v] (core/date? v))
(defn date-time?
  "Return whether the provided value `v` is a date time"
  [v] (core/date-time? v))
(defn time?
  "Return whether the provided value `v` is a time"
  [v] (core/time? v))
(defn month?
  "Return whether the provided value `v` is a month"
  [v] (core/month? v))
(defn offset-date-time?
  "Return whether the provided value `v` is an offset date time"
  [v] (core/offset-date-time? v))
(defn period?
  "Return whether the provided value `v` is a period"
  [v] (core/period? v))
(defn year?
  "Return whether the provided value `v` is a year"
  [v] (core/year? v))
(defn year-month?
  "Return whether the provided value `v` is a year month"
  [v] (core/year-month? v))
(defn zone?
  "Return whether the provided value `v` is a zone time zone"
  [v] (core/zone? v))
(defn zone-offset?
  "Return whether the provided value `v` is a zone offset"
  [v] (core/zone-offset? v))
(defn zoned-date-time?
  "Return whether the provided value `v` is a zoned date time"
  [v] (core/zoned-date-time? v))
(defn interval?
  "Return whether the provided value `v` is an interval"
  [v] (core/interval? v))
