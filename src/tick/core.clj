;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [+ - inc dec max min range])
  (:require
   [clojure.spec.alpha :as s])
  (:import
   [java.util Date]
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate ZoneId]
   [java.time.format DateTimeFormatter]
   [java.time.temporal ChronoUnit]))

(defn nanos [n]
  (Duration/ofNanos n))

(defn millis [n]
  (Duration/ofMillis n))

(defn seconds [n]
  (Duration/ofSeconds n))

(defn minutes [n]
  (Duration/ofMinutes n))

(defn hours [n]
  (Duration/ofHours n))

(defn days [n]
  (Duration/ofDays n))

(defn weeks [n]
  (Duration/ofDays (* 7 n)))

(defn now [] (Instant/now))

(s/def ::instant #(instance? Instant %))

(defprotocol IConstructors
  (instant [_] "Make a java.time.Instant instance.")
  (local-date [_] [_ zone] "Make a java.time.Instant instance.")
  (zone [_] "Make a java.time.ZoneId instance."))

(extend-protocol IConstructors
  Instant
  (instant [i] i)
  (local-date
    ([i] (throw (ex-info "Needs zone" {})))
    ([i zone] (.. i (atZone zone) toLocalDate)))
  String
  (instant [s] (Instant/from (.parse (DateTimeFormatter/ISO_INSTANT) s)))
  (local-date
    ([s] (LocalDate/parse s))
    ([s zone] (local-date (instant s) zone)))
  (zone [s] (ZoneId/of s))
  Date
  (instant [d] (.toInstant d))
  (local-date
    ([d] (throw (ex-info "Needs zone" {})))
    ([d zone] (local-date (instant d) zone)))
  ZoneId
  (zone [z] z)
  ZonedDateTime
  (instant [zdt] (.toInstant zdt)))

(defprotocol ITimeArithmetic
  (+ [_ _] "Add time")
  (- [_ _] "Subtract time")
  (inc [_] "Increment time")
  (dec [_] "Decrement time")
  (max [_ _] "Return maximum")
  (min [_ _] "Return minimum")
  (range [_] [_ _] [_ _ _] "Returns a lazy seq of times from start (inclusive) to end (exclusive, nil means forever), by step, where start defaults to 0, step to 1, and end to infinity."))

(extend-type Instant
  ITimeArithmetic
  (+ [t x] (.plus t x))
  (- [t x] (.minus t x))
  (inc [t] (+ t (seconds 1)))
  (dec [t] (- 1 (seconds 1)))
  (max [x y] (if (>= (.toEpochMilli x) (.toEpochMilli y))
                 x y))
  (min [x y] (if (<= (.toEpochMilli x) (.toEpochMilli y))
                 x y))
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(.isBefore % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(.isBefore % to))))))

(extend-type LocalDate
  ITimeArithmetic
  (+ [t x] (.plusDays t x))
  (- [t x] (.minusDays t x))
  (inc [t] (.plusDays t 1))
  (dec [t] (.minusDays t 1))
  (max [x y] (if (>= (.toEpochDay x) (.toEpochDay y)) x y))
  (min [x y] (if (<= (.toEpochDay x) (.toEpochDay y)) x y))
  (range
    ([from] (iterate #(.plusDays % 1) from))
    ([from to] (cond->> (iterate #(.plusDays % 1) from)
                 to (take-while #(.isBefore % to) )))
    ([from to step] (cond->> (iterate #(.plusDays % step) from)
                      to (take-while #(.isBefore % to))))))


(defn local-dates
  "Return a lazy sequence of the local-dates (inclusive) that the
  given interval spans."
  [interval zone]
  (range (local-date (first interval) zone)
         (inc (local-date (second interval) zone))))
