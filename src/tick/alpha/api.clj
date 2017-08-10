;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.api
  (:refer-clojure :exclude [+ - inc dec max min range time partition-by group-by])
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :as core]
   [tick.cal :as cal]
   [tick.interval :as interval])
  (:import
   [java.time Duration ZoneId LocalTime LocalDate]))

;; This API is optimises convenience, API stability and (type) safety
;; over performance. Where performance is critical, use tick.core and
;; friends.

;; clojure.spec assertions are used to check correctness, but these
;; are disabled by default (except when testing).

(defn nanos [n] (core/nanos n))
(defn millis [n] (core/millis n))
(defn seconds [n] (core/seconds n))
(defn minutes [n] (core/minutes n))
(defn hours [n] (core/hours n))
(defn days [n] (core/days n))
(defn weeks [n] (core/weeks n))
(defn now [] (core/now))
(defn today [] (core/today))
(defn tomorrow [] (core/tomorrow))
(defn yesterday [] (core/yesterday))

(defn + [arg & args]
  (reduce #(core/+ %1 %2) arg args))

(defn - [arg & args]
  (reduce #(core/- %1 %2) arg args))

(defn inc [arg]
  (core/inc arg))

(defn dec [arg]
  (core/dec arg))

(defn max [arg & args]
  (reduce #(core/max %1 %2) arg args))

(defn min [arg & args]
  (reduce #(core/min %1 %2) arg args))

(def range core/range)

;; Constructors

(defn date [v] (core/date v))
(defn inst [v] (core/inst v))
(defn instant [v] (core/instant v))
(defn offset-date-time [v] (core/offset-date-time v))
(defn year [v] (core/year v))
(defn year-month [v] (core/year-month v))
(defn zone [z] (core/zone z))
(defn zoned-date-time [z] (core/zoned-date-time z))

;; Time

(defn time [v] (core/time v))
(defn on [t d] (core/on (time t) (date d)))
(defn at [d t] (core/at (date d) (time t)))
(defn start [v] (core/start v))
(defn end [v] (core/end v))
(defn midnight? [v] (core/midnight? v))

;; Zones

(def UTC (zone "UTC"))
(def LONDON (zone "Europe/London"))

;; Intervals

(defn interval
  "Return an interval which forms the bounding-box of the given arguments."
  ([v] (interval/interval v))
  ([v1 & args] (apply interval/interval v1 args)))

;; An interval is just a vector with at least 2 entries. The 3rd entry
;; onwards are free to use by the caller.
(defn interval? [v] (and (vector? v) (>= (count v) 2)))

(defn duration [interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (interval/duration interval)))

(defn intersection [x y]
  (interval/intersection x y))

(defn dates [interval]
  (interval/dates (interval/interval interval)))

(defn year-months [interval]
  (interval/year-months (interval/interval interval)))

(defn years [interval]
  (interval/years (interval/interval interval)))

(defn partition-by [f interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (interval/partition-by f interval)))

(defn partition-by-date [interval]
  (partition-by dates interval))

(defn group-by [f interval]
  (let [interval (interval/interval interval)]
    (s/assert :tick.interval/interval interval)
    (sort (interval/group-by f interval))))

(defn group-by-date [interval]
  (group-by dates interval))

;; Fixing the clock used for `today` and `now`.

(defmacro with-clock [^java.time.Clock clock & body]
  `(binding [tick.core/*clock* ~clock]
     ~@body))
