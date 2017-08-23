;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [+ - inc dec max min range time int long < >])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [tick.core :as core])
  (:import
   [java.util Date]
   [java.time Clock ZoneId ZoneOffset Instant Duration DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime]
   [java.time.format DateTimeFormatter]
   [java.time.temporal ChronoUnit]))

(def ^{:dynamic true} *clock* nil)

(defn now []
  (if *clock*
    (Instant/now *clock*)
    (Instant/now)))

(defn today []
  (if *clock*
    (LocalDate/now *clock*)
    (LocalDate/now)))

(s/def ::instant #(instance? Instant %))

(defn parse-day [input]
  (condp re-matches (str/lower-case input)
    #"(mon)(day)?" DayOfWeek/MONDAY
    #"(tue)(s|sday)?" DayOfWeek/TUESDAY
    #"(wed)(s|nesday)?" DayOfWeek/WEDNESDAY
    #"(thur)(s|sday)?" DayOfWeek/THURSDAY
    #"(fri)(day)?" DayOfWeek/FRIDAY
    #"(sat)(urday)?" DayOfWeek/SATURDAY
    #"(sun)(day)?" DayOfWeek/SUNDAY
    nil))

(defn parse-month [input]
  (condp re-matches (str/lower-case input)
    #"(jan)(uary)?" Month/JANUARY
    #"(feb)(ruary)?" Month/FEBRUARY
    #"(mar)(ch)?" Month/MARCH
    #"(apr)(il)?" Month/APRIL
    #"may" Month/MAY
    #"(jun)(e)?" Month/JUNE
    #"(jul)(y)?" Month/JULY
    #"(aug)(ust)?" Month/AUGUST
    #"(sep)(tember)?" Month/SEPTEMBER
    #"(oct)(tober)?" Month/OCTOBER
    #"(nov)(ember)?" Month/NOVEMBER
    #"(dec)(ember)?" Month/DECEMBER
    nil))

(defprotocol IParseable
  (parse [_] "Parse to most applicable instance."))

(extend-protocol IParseable
  String
  (parse [s]
    (condp re-matches s
      #"(\d{1,2})\s*(am|pm)"
      :>> (fn [[_ h ap]] (LocalTime/of (cond-> (Integer/parseInt h) (= "pm" ap) (clojure.core/+ 12)) 0))
      #"(\d{1,2})"
      :>> (fn [[_ h]] (LocalTime/of (Integer/parseInt h) 0))
      #"\d{2}:\d{2}\S*"
      :>> (fn [s] (LocalTime/parse s))
      #"(\d{1,2}):(\d{2})"
      :>> (fn [[_ h m]] (LocalTime/of (Integer/parseInt h) (Integer/parseInt m)))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z"
      :>> (fn [s] (Instant/parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}"
      :>> (fn [s] (OffsetDateTime/parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}\[\w+/\w+\]"
      :>> (fn [s] (ZonedDateTime/parse s))
      #"\d{4}-\d{2}-\d{2}T\S*"
      :>> (fn [s] (LocalDateTime/parse s))
      #"\d{4}-\d{2}-\d{2}"
      :>> (fn [s] (LocalDate/parse s))
      #"\d{4}-\d{2}"
      :>> (fn [s] (YearMonth/parse s))
      #"\d{4}"
      :>> (fn [s] (Year/parse s))
      (throw (ex-info "Unparseable time string" {:input s})))))

(defprotocol IConstructors
  (date [_] "Make a java.time.LocalDate instance.")
  (day [_] "Make a java.time.DayOfWeek instance.")
  (inst [_] "Make a java.util.Date instance.")
  (instant [_] "Make a java.time.Instant instance.")
  (int [_] "Return value as integer")
  (long [_] "Return value as long")
  (month [_] "Make a java.time.Month instance.")
  (offset-date-time [_] "Make a java.time.OffsetDateTime instance.")
  (year [_] "Make a java.time.Year instance.")
  (year-month [_] "Make a java.time.YearMonth instance.")
  (zone [_] "Make a java.time.ZoneId instance.")
  (zoned-date-time [_] "Make a java.time.ZonedDateTime instance."))

(extend-protocol IConstructors
  Object
  (int [v] (clojure.core/int v))
  (long [v] (clojure.core/long v))

  clojure.lang.Fn
  (date [f] (date (f)))
  (day [f] (day (f)))
  (inst [f] (inst (f)))
  (instant [f] (instant (f)))
  (int [f] (int (f)))
  (long [f] (long (f)))
  (month [f] (month (f)))
  (offset-date-time [f] (offset-date-time (f)))
  (year [f] (year (f)))
  (year-month [f] (year-month (f)))
  (zone [f] (zone (f)))
  (zoned-date-time [f] (zone (f)))

  Instant
  (inst [i] (Date/from i))
  (instant [i] i)
  (date [i] (date (zoned-date-time i)))
  (day [i] (day (date i)))
  (month [d] (month (date d)))
  (zoned-date-time [i] (.atZone i ZoneOffset/UTC))
  (int [i] (.getNano i))
  (long [i] (.getEpochSecond i))

  String
  (inst [s] (inst (instant s)))
  (instant [s] (instant (parse s)))
  (day [s] (or (parse-day s) (day (date s))))
  (date [s] (date (parse s)))
  (month [s] (parse-month s))
  (year [s] (year (parse s)))
  (year-month [s] (year-month (parse s)))
  (zone [s] (ZoneId/of s))
  (int [s] (.getNano (instant s)))
  (long [s] (.getEpochSecond (instant s)))

  Number
  (day [n] (DayOfWeek/of n))
  (month [n] (Month/of n))
  (instant [n] (Instant/ofEpochSecond n))
  (year [n] (Year/of n))

  LocalDate
  (date [d] d)
  (day [d] (.getDayOfWeek d))
  (month [d] (Month/from d))
  (year-month [d] (YearMonth/of (.getYear d) (.getMonthValue d)))
  (year [d] (Year/of (.getYear d)))

  Month
  (int [m] (.getValue m))

  LocalDateTime
  (date [dt] (.toLocalDate dt))
  (day [dt] (day (date dt)))
  (year-month [dt] (year-month (date dt)))
  (year [dt] (year (date dt)))

  Date
  (inst [d] d)
  (instant [d] (.toInstant d))
  (date [d] (date (zoned-date-time (instant d)))) ; implicit conversion to UTC
  (year-month [d] (year-month (date d)))
  (year [d] (year (date d)))

  YearMonth
  (year-month [ym] ym)
  (year [ym] (year (.getYear ym)))

  Year
  (year [y] y)
  (int [y] (.getValue y))

  ZoneId
  (zone [z] z)

  ZonedDateTime
  (inst [zdt] (inst (instant zdt)))
  (instant [zdt] (.toInstant zdt))
  (date [zdt] (.toLocalDate zdt))
  (zone [zdt] (.getZone zdt)))

(defprotocol IDuration
  (nanos [_] "Return the given quantity in nanoseconds.")
  (millis [_] "Return the given quantity in milliseconds.")
  (seconds [_] "Return the given quantity in seconds.")
  (minutes [_] "Return the given quantity in minutes.")
  (hours [_] "Return the given quantity in hours.")
  (days [_] "Return the given quantity in days.")
  (weeks [_] "Return the given quantity in weeks.")
  (months [_] "Return the given quantity in approximate months.")
  (years [_] "Return the given quantity in approximate years."))

(extend-protocol IDuration
  Number
  (nanos [n] (Duration/ofNanos n))
  (millis [n] (Duration/ofMillis n))
  (seconds [n] (Duration/ofSeconds n))
  (minutes [n] (Duration/ofMinutes n))
  (hours [n] (Duration/ofHours n))
  (days [n] (Duration/ofDays n))
  (weeks [n] (Duration/ofDays (* n 7)))
  (months [n] (.multipliedBy (years n) 12))
  (years [n] (Duration/ofDays (/ n 365.25)))

  Duration
  (nanos [d] (.toNanos d))
  (millis [d] (.toMillis d))
  (seconds [d] (.getSeconds d))
  (minutes [d] (.toMinutes d))
  (hours [d] (.toHours d))
  (days [d] (.toDays d))
  (weeks [d] (.dividedBy (days d) 7))
  (months [d] (.multipliedBy (years d) 12))
  (years [d] (.dividedBy (days d) 365.24)))

(defprotocol IDurationCoercion
  (duration [_] [_ _] "Return the duration of the given value, or between a pair of values, if appropriate."))

(extend-protocol IDurationCoercion
  Duration
  (duration [d] d)
  nil
  (duration ([_] nil) ([_ _] nil))
  clojure.lang.PersistentVector
  (duration [i1 i2]
    (Duration/between (instant i1) (instant i2))))

(definline < [x y] `(.isBefore ~x ~y))
(definline > [x y] `(.isAfter ~x ~y))

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
  (dec [t] (- t (seconds 1)))
  (max [x y] (if (neg? (compare x y)) y x))
  (min [x y] (if (neg? (compare x y)) x y))
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type ZonedDateTime
  ITimeArithmetic
  (+ [t x] (.plus t x))
  (- [t x] (.minus t x))
  (inc [t] (+ t (seconds 1)))
  (dec [t] (- t (seconds 1)))
  (max [x y] (if (neg? (compare x y)) y x))
  (min [x y] (if (neg? (compare x y)) x y))
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type LocalDate
  ITimeArithmetic
  (+ [t x] (if (number? x) (.plusDays t x) (.plus t x)))
  (- [t x] (if (number? x) (.minusDays t x) (.minus t x)))
  (inc [t] (.plusDays t 1))
  (dec [t] (.minusDays t 1))
  (max [x y] (if (neg? (compare x y)) y x))
  (min [x y] (if (neg? (compare x y)) x y))
  (range
    ([from] (iterate #(.plusDays % 1) from))
    ([from to] (cond->> (iterate #(.plusDays % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plusDays % step) from)
                      to (take-while #(< % to))))))

(extend-type LocalDateTime
  ITimeArithmetic
  (+ [t x] (.plus t x))
  (- [t x] (.minus t x))
  (inc [t] (+ t (seconds 1)))
  (dec [t] (- t (seconds 1)))
  (max [x y] (if (neg? (compare x y)) y x))
  (min [x y] (if (neg? (compare x y)) x y))
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type YearMonth
  ITimeArithmetic
  (+ [t x] (if (number? x) (.plusMonths t x) (.plus t x)))
  (- [t x] (if (number? x) (.minusMonths t x) (.minus t x)))
  (inc [t] (.plusMonths t 1))
  (dec [t] (.minusMonths t 1))
  (max [x y] (if (neg? (compare x y)) y x))
  (min [x y] (if (neg? (compare x y)) x y))
  (range
    ([from] (iterate #(.plusMonths % 1) from))
    ([from to] (cond->> (iterate #(.plusMonths % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type Year
  ITimeArithmetic
  (+ [t x] (if (number? x) (.plusYears t x) (.plus t x)))
  (- [t x] (if (number? x) (.minusYears t x) (.minus t x)))
  (inc [t] (.plusYears t 1))
  (dec [t] (.minusYears t 1))
  (max [x y] (if (neg? (compare x y)) y x))
  (min [x y] (if (neg? (compare x y)) x y))
  (range
    ([from] (iterate #(.plusYears % 1) from))
    ([from to] (cond->> (iterate #(.plusYears % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type Duration
  ITimeArithmetic
  (+ [t x] (.plus t x))
  (- [t x] (.minus t x))
  (inc [t] (.plusSeconds t 1))
  (dec [t] (.minusSeconds t 1))
  (max [x y] (if (neg? (compare x y)) y x))
  (min [x y] (if (neg? (compare x y)) x y)))

(defn tomorrow []
  (+ (today) 1))

(defn yesterday []
  (- (today) 1))

(defprotocol ITime
  (time [s] "Constructor of an instant, inst, java.time.LocalTime or java.time.LocalDateTime?")
  (local? [t] "Is the time a java.time.LocalTime or java.time.LocalDateTime?"))

(defprotocol ITimeRange
  (start [_] "Return the start of a time period.")
  (end [_] "Return the end of a time period."))

(extend-protocol IDurationCoercion
  Object
  (duration [v] (Duration/between (start v) (end v))))

(extend-protocol ITime
  String
  (time [s] (time (parse s)))

  Number
  (time [i]
    (LocalTime/of i 0))

  Date
  (time [d] (instant d))
  (local? [d] false)

  Instant
  (time [i] i)
  (local? [i] false)

  LocalDateTime
  (time [i] i)
  (local? [i] true)

  LocalTime
  (time [i] i)
  (local? [i] true))

(extend-protocol ITimeRange
  String
  (start [s] (start (time s)))
  (end [s] (end (time s)))

  Number
  (start [n] (start (time n)))
  (end [n] (end (time n)))

  LocalDate
  (start [date] (.atStartOfDay date))
  (end [date] (.atStartOfDay (inc date)))

  Year
  (start [year] (start (.atMonth year 1)))
  (end [year] (end (.atMonth year 12)))

  YearMonth
  (start [ym] (start (.atDay ym 1)))
  (end [ym] (end (.atEndOfMonth ym)))

  Instant
  (start [i] i)
  (end [i] i)

  Date
  (start [i] (instant i))
  (end [i] (instant i))

  LocalDateTime
  (start [time] time)
  (end [end] end))

(defn on [^LocalTime time ^LocalDate date]
  (.atTime date time))

(defn at [^LocalDate date ^LocalTime time]
  (.atTime date time))

(defn midnight [^LocalDate date]
  (at date (LocalTime/MIDNIGHT)))

(defn noon [^LocalDate date]
  (at date (LocalTime/NOON)))

(defn midnight? [^LocalDateTime t]
  (.isZero (Duration/between t (start (date t)))))

(defprotocol IAtZone
  (at-zone [t zone] "Put time at zone")
  (to-local [t] [t zone] "Convert to local time at zone."))

(extend-protocol IAtZone
  LocalDateTime
  (at-zone [t zone] (.atZone t zone))
  (to-local
    ([t] t)
    ([t zone] (to-local (at-zone t zone))))
  Instant
  (at-zone [t zone] (.atZone t zone))
  (to-local
    ([t] (throw (ex-info "Error, zone required" {})))
    ([t zone] (to-local (at-zone t zone))))
  ZonedDateTime
  (at-zone [t zone] (.withZoneSameInstant t zone))
  (to-local
    ([t] (.toLocalDateTime t))
    ([t zone] (to-local (at-zone t zone))))
  Date
  (at-zone [t zone] (at-zone (instant t) zone))
  (to-local
    ([t] (throw (ex-info "Error, zone required" {})))
    ([t zone] (to-local (at-zone t zone)))))

(defprotocol MinMax
  (min-of-type [_] "Return the min")
  (max-of-type [_] "Return the max"))

(extend-protocol MinMax
  LocalDateTime
  (min-of-type [_] (LocalDateTime/MIN))
  (max-of-type [_] (LocalDateTime/MAX))
  Instant
  (min-of-type [_] (Instant/MIN))
  (max-of-type [_] (Instant/MAX))
  ZonedDateTime
  (min-of-type [_] (Instant/MIN))
  (max-of-type [_] (Instant/MAX))
  nil
  (min-of-type [_] (Instant/MIN))
  (max-of-type [_] (Instant/MAX)))
