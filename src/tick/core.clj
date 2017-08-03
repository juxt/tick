;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [+ - inc dec max min range])
  (:require
   [clojure.spec.alpha :as s])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate]
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

(defprotocol IInstant
  (instant [_] "Make java.time.Instant instance"))

(extend-protocol IInstant
  String
  (instant [s] (Instant/from (.parse (DateTimeFormatter/ISO_INSTANT) s))))

(defprotocol ITimeArithmetic
  (plus-t [_ _] "Add time")
  (minus-t [_ _] "Subtract time")
  (inc-t [_] "Increment time")
  (dec-t [_] "Decrement time")
  (max-t [_ _] "Return maximum")
  (min-t [_ _] "Return minimum")
  (range-t [_] [_ _] [_ _ _] "Returns a lazy seq of times from start (inclusive) to end (exclusive, nil means forever), by step, where start defaults to 0, step to 1, and end to infinity."))

(extend-type Instant
  ITimeArithmetic
  (plus-t [t x] (.plus t x))
  (minus-t [t x] (.minus t x))
  (inc-t [t] (plus-t t (seconds 1)))
  (dec-t [t] (minus-t 1 (seconds 1)))
  (max-t [x y] (if (>= (.toEpochMilli x) (.toEpochMilli y))
                 x y))
  (min-t [x y] (if (<= (.toEpochMilli x) (.toEpochMilli y))
                 x y))
  (range-t
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(.isBefore % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(.isBefore % to))))))

(extend-type LocalDate
  ITimeArithmetic
  (plus-t [t x] (.plusDays t x))
  (minus-t [t x] (.minusDays t x))
  (inc-t [t] (.plusDays t 1))
  (dec-t [t] (.minusDays t 1))
  (max-t [x y] (if (>= (.toEpochDay x) (.toEpochDay y)) x y))
  (min-t [x y] (if (<= (.toEpochDay x) (.toEpochDay y)) x y))
  (range-t
    ([from] (iterate #(.plusDays % 1) from))
    ([from to] (cond->> (iterate #(.plusDays % 1) from)
                 to (take-while #(.isBefore % to) )))
    ([from to step] (cond->> (iterate #(.plusDays % step) from)
                      to (take-while #(.isBefore % to))))))
