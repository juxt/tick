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
  (inst [_] "Make a java.util.Date instance.")
  (instant [_] "Make a java.time.Instant instance.")
  (date [_] [_ zone] "Make a java.time.LocalDate instance.")
  (zone [_] "Make a java.time.ZoneId instance."))

(extend-protocol IConstructors

  Instant
  (inst [i] (Date/from i))
  (instant [i] i)
  (date
    ([i] (throw (ex-info "Needs zone" {})))
    ([i zone] (.. i (atZone zone) toLocalDate)))

  String
  (inst [s] (inst (instant s)))
  (instant [s] (Instant/from (.parse (DateTimeFormatter/ISO_INSTANT) s)))
  (date
    ([s] (LocalDate/parse s))
    ([s zone] (date (instant s) zone)))
  (zone [s] (ZoneId/of s))

  Date
  (inst [d] d)
  (instant [d] (.toInstant d))
  (date
    ([d] (throw (ex-info "Needs zone" {})))
    ([d zone] (date (instant d) zone)))

  ZoneId
  (zone [z] z)

  ZonedDateTime
  (inst [zdt] (inst (instant zdt)))
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

(extend-type Duration
  ITimeArithmetic
  (+ [t x] (.plus t x))
  (- [t x] (.minus t x))
  (inc [t] (.plusSeconds t 1))
  (dec [t] (.minusSeconds t 1))
  ;; TODO: Copy with nanoseconds
  #_(max [x y] (if (>= (.getSeconds x) (.getSeconds y)) x y))
  #_(min [x y] (if (<= (.getSeconds x) (.getSeconds y)) x y)))
