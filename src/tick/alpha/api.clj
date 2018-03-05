;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.api
  (:refer-clojure
   :exclude [+ - * / inc dec max min
             range time int long complement
             < <= > >= << >>
             extend
             atom swap! swap-vals! compare-and-set!
             reset! reset-vals!
             second
             ])
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :as core]
   tick.file ; To ensure protocol extension
   [tick.interval :as interval]
   [clojure.set :as set])
  (:import
   [java.time Clock Duration ZoneId LocalTime LocalDate DayOfWeek Month]))

;; This API is optimises convenience, API stability and (type) safety
;; over performance. Where performance is critical, use tick.core and
;; friends.

;; clojure.spec assertions are used to check correctness, but these
;; are disabled by default (except when testing).

;; Surfacing some useful constants

(def unit-map core/unit-map)

;; Point-in-time 'demo' functions

(defn now [] (core/now))
(defn today [] (core/today))
(defn tomorrow [] (core/tomorrow))
(defn yesterday [] (core/yesterday))

;; Constants

(def monday DayOfWeek/MONDAY)
(def tuesday DayOfWeek/TUESDAY)
(def wednesday DayOfWeek/WEDNESDAY)
(def thursday DayOfWeek/THURSDAY)
(def friday DayOfWeek/FRIDAY)
(def saturday DayOfWeek/SATURDAY)
(def sunday DayOfWeek/SUNDAY)

(def january Month/JANUARY)
(def february Month/FEBRUARY)
(def march Month/MARCH)
(def april Month/APRIL)
(def may Month/MAY)
(def june Month/JUNE)
(def july Month/JULY)
(def august Month/AUGUST)
(def september Month/SEPTEMBER)
(def october Month/OCTOBER)
(def november Month/NOVEMBER)
(def december Month/DECEMBER)

;; Construction and coercion

(defn date
  ([] (core/date (today)))
  ([v] (core/date v)))
(defn time
  ([] (core/time (now)))
  ([v] (core/time v)))

(defn millisecond [t] (core/millisecond t))
(defn second [t] (core/second t))
(defn minute [t] (core/minute t))
(defn hour [t] (core/hour t))

(defn day
  ([] (core/day (today)))
  ([v] (core/day v)))
(defn day-of-month
  ([] (core/day-of-month (today)))
  ([v] (core/day-of-month v)))
(defn inst
  ([] (core/inst (now)))
  ([v] (core/inst v)))
(defn instant
  ([] (core/instant (now)))
  ([v] (core/instant v)))
(defn offset-date-time
  ([] (core/offset-date-time (now)))
  ([v] (core/offset-date-time v)))
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
  ([] (ZoneId/systemDefault))
  ([z] (core/zone z)))

(defn zone-offset [offset] (core/zone-offset offset))

(defn zoned-date-time [v] (core/zoned-date-time v))
(defn local-date-time [v] (core/local-date-time v))

(defn beginning [v] (core/beginning v))
(defn end [v] (core/end v))
(def coincident? core/coincident?)

;; Time

(defn on [t d] (core/on t (date d)))
(defn at [d t] (core/at d (time t)))
(defn in [ldt z] (core/in ldt (zone z)))
(defn offset-by [ldt offset] (core/offset-by ldt (zone-offset offset)))

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
(def LONDON (zone "Europe/London"))

;; Parsing

(def parse core/parse)

;; Arithmetic

(defn +
  ([arg] arg)
  ([arg & args]
   (reduce #(core/+ %1 %2) arg args)))

(defn -
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

;; Durations

(defn nanos [v] (core/nanos v))
(defn micros [v] (core/micros v))
(defn millis [v] (core/millis v))
(defn seconds [v] (core/seconds v))
(defn minutes [v] (core/minutes v))
(defn hours [v] (core/hours v))
(defn days [v] (core/days v))

;; Periods
(defn weeks [v] (core/weeks v))
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

(def tick core/tick)
(def atom core/atom)
(def swap! core/swap!)
(def swap-vals! core/swap-vals!)
(def compare-and-set! core/compare-and-set!)
(def reset! core/reset!)
(def reset-vals! core/reset-vals!)

;; Intervals

(defn interval [x y]
  (interval/interval x y))

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

(defn duration
  [v1 v2]
  (core/duration v1 v2))

(defn period
  [v1 v2]
  (core/period v1 v2))

(def length core/length)

(defn concur
  ([] nil)
  ([x] x)
  ([x & args]
   (reduce interval/concur x args)))

;; Divisions

(defn / [x divisor]
  (core// x divisor))

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

(defn ordered-disjoint-intervals?
  "An interval set is an ordered collection of disjoint
  intervals. This predicate can be used to ensure a value conforms to
  this definition, because some functions depend on this property
  holding."
  [s]
  (interval/ordered-disjoint-intervals? s))

(defn union [& colls]
  "Return a set that is the union of the input interval sets"
  {:pre [(s/assert (s/coll-of ordered-disjoint-intervals?) colls)]}
  (apply interval/union colls))

(defn intersection [& colls]
  "Return a set that is the intersection of the input interval sets"
  {:pre [(s/assert (s/coll-of ordered-disjoint-intervals?) colls)]}
  (apply interval/intersection colls))

(defn difference [& colls]
  "Return a set that is the intersection of the input interval sets"
  {:pre [(s/assert (s/coll-of ordered-disjoint-intervals?) colls)]}
  (apply interval/difference colls))

(defn complement [coll]
  {:pre [(s/assert ordered-disjoint-intervals? coll)]}
  (interval/complement coll))
