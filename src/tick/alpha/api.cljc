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
    [clojure.spec.alpha :as s]
    [tick.core :as core]
    [tick.format :as t.f]
    #?(:clj tick.file) ; To ensure protocol extension
    #?(:clj [net.cgrand.macrovich :as macros])
    [tick.interval :as interval]
    [clojure.set :as set]
    #?@(:cljs
       [
        [java.time :refer [Duration ZoneId LocalTime LocalDate DayOfWeek Month ZoneOffset]]
        [java.time.format :refer [DateTimeFormatter]]]))
  #?(:cljs
     (:require-macros
       [net.cgrand.macrovich :as macros]
       [tick.alpha.api :refer [with-clock]]))
  #?(:clj
     (:import
       [java.time Duration ZoneId LocalTime LocalDate DayOfWeek Month ZoneOffset]
       [java.time.format DateTimeFormatter])))

;; This API is optimises convenience, API stability and (type) safety
;; over performance. Where performance is critical, use tick.core and
;; friends.

;; clojure.spec assertions are used to check correctness, but these
;; are disabled by default (except when testing).

;; Construction

(def new-time core/new-time)
(def new-date core/new-date)

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
  ([hours minutes] (. ZoneOffset ofHoursMinutes hours minutes))
  ([hours minutes seconds] (. ZoneOffset ofHoursMinutesSeconds hours minutes seconds)))

;; Reification

(defn on [t d] (core/on t (date d)))
(defn at [d t] (core/at d (time t)))
(defn in [ldt z] (core/in ldt (zone z)))
(defn offset-by [ldt offset] (core/offset-by ldt (zone-offset offset)))

;; Constants

(def MONDAY (. DayOfWeek -MONDAY))
(def TUESDAY (. DayOfWeek -TUESDAY))
(def WEDNESDAY (. DayOfWeek -WEDNESDAY))
(def THURSDAY (. DayOfWeek -THURSDAY))
(def FRIDAY (. DayOfWeek -FRIDAY))
(def SATURDAY (. DayOfWeek -SATURDAY))
(def SUNDAY (. DayOfWeek -SUNDAY))

(def JANUARY (. Month -JANUARY))
(def FEBRUARY (. Month -FEBRUARY))
(def MARCH (. Month -MARCH))
(def APRIL (. Month -APRIL))
(def MAY (. Month -MAY))
(def JUNE (. Month -JUNE))
(def JULY (. Month -JULY))
(def AUGUST (. Month -AUGUST))
(def SEPTEMBER (. Month -SEPTEMBER))
(def OCTOBER (. Month -OCTOBER))
(def NOVEMBER (. Month -NOVEMBER))
(def DECEMBER (. Month -DECEMBER))

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

(def parse core/parse)

;; Arithmetic

(defn +
  ([] (. Duration -ZERO))
  ([arg] arg)
  ([arg & args]
   (reduce #(core/+ %1 %2) arg args)))

(defn -
  ([] (. Duration -ZERO))
  ([arg] (core/negated arg))
  ([arg & args]
   (reduce #(core/- %1 %2) arg args)))

(defn inc [t]
  (core/inc t))

(defn dec [t]
  (core/dec t))

(defn >> [t amt]
  (core/>> t amt))

(defn << [t amt]
  (core/<< t amt))

(def max core/max)
(def min core/min)

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
  ([x] true)
  ([x y] (core/< x y))
  ([x y & more] (if (core/< x y)
                  (if (next more)
                    (recur y (first more) (next more))
                    (core/< y (first more)))
                  false)))

(defn <=
  ([x] true)
  ([x y] (core/<= x y))
  ([x y & more] (if (core/<= x y)
                  (if (next more)
                    (recur y (first more) (next more))
                    (core/<= y (first more)))
                  false)))

(defn >
  ([x] true)
  ([x y] (core/> x y))
  ([x y & more] (if (core/> x y)
                  (if (next more)
                    (recur y (first more) (next more))
                    (core/> y (first more)))
                  false)))

(defn >=
  ([x] true)
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
