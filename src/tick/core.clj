;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [+ - / inc dec max min range time int long < <= > >= next])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str])
  (:import
   [java.util Date]
   [java.time Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime]
   [java.time.format DateTimeFormatter]
   [java.time.temporal ChronoUnit ChronoField TemporalAdjusters]
   [clojure.lang ILookup]))

(def ^{:dynamic true} *clock* nil)

(defn now []
  (if *clock*
    (Instant/now *clock*)
    (Instant/now)))

(defn today []
  (if *clock*
    (LocalDate/now *clock*)
    (LocalDate/now)))

(defn epoch []
  (java.time.Instant/EPOCH))

(defprotocol ITimeReify
  (on [_ _] "Set time be ON a date")
  (at [_ _] "Set date to be AT a time")
  (in [_ _] "Set a date-time to be in a time-zone"))

(defn midnight
  ([] (LocalTime/MIDNIGHT))
  ([^LocalDate date]
   (at date (LocalTime/MIDNIGHT))))

(defn noon
  ([] (LocalTime/NOON))
  ([^LocalDate date]
   (at date (LocalTime/NOON))))

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
  (time [_] "Make a java.time.LocalTime instance.")
  (day [_] "Make a java.time.DayOfWeek instance.")
  (day-of-month [_] "Return value of the day in the month as an integer.")
  (inst [_] "Make a java.util.Date instance.")
  (instant [_] "Make a java.time.Instant instance.")
  (int [_] "Return value as integer")
  (long [_] "Return value as long")
  (month [_] "Make a java.time.Month instance.")
  (year [_] "Make a java.time.Year instance.")
  (year-month [_] "Make a java.time.YearMonth instance.")
  (zone [_] "Make a java.time.ZoneId instance.")
  (zone-offset [_] "Make a java.time.ZoneOffset instance.")
  (zoned-date-time [_] "Make a java.time.ZonedDateTime instance.")
  (offset-date-time [_] "Make a java.time.OffsetDateTime instance.")
  (local-date-time [_] "Make a java.time.LocalDateTime instance."))

(extend-protocol IConstructors
  Object
  (int [v] (clojure.core/int v))
  (long [v] (clojure.core/long v))

  clojure.lang.Fn
  (date [f] (date (f)))
  (time [f] (time (f)))
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
  (zoned-date-time [f] (zoned-date-time (f)))
  (local-date-time [f] (local-date-time (f)))

  Instant
  (date [i] (date (zoned-date-time i)))
  (time [i] (time (zoned-date-time i)))
  (day [i] (day (date i)))
  (inst [i] (Date/from i))
  (instant [i] i)
  (int [i] (.getNano i))
  (long [i] (.getEpochSecond i))
  (month [i] (month (date i)))
  (year [i] (year (date i)))
  (year-month [i] (year-month (date i)))
  (zoned-date-time [i] (.atZone i ZoneOffset/UTC))

  String
  (inst [s] (inst (instant s)))
  (instant [s] (instant (parse s)))
  (day [s] (or (parse-day s) (day (date s))))
  (date [s] (date (parse s)))
  (time [s] (time (parse s)))
  (month [s] (parse-month s))
  (year [s] (year (parse s)))
  (year-month [s] (year-month (parse s)))
  (zone [s] (ZoneId/of s))
  (zone-offset [s] (ZoneOffset/of s))
  (int [s] (.getNano (instant s)))
  (long [s] (.getEpochSecond (instant s)))
  (local-date-time [s] (local-date-time (parse s)))

  Number
  (day [n] (DayOfWeek/of n))
  (month [n] (Month/of n))
  (instant [n] (Instant/ofEpochMilli n))
  (year [n] (Year/of n))

  LocalDate
  (date [d] d)
  (day [d] (.getDayOfWeek d))
  (day-of-month [d] (.getDayOfMonth d))
  (month [d] (Month/from d))
  (year-month [d] (YearMonth/of (.getYear d) (.getMonthValue d)))
  (year [d] (Year/of (.getYear d)))

  LocalTime
  (time [t] t)

  Month
  (int [m] (.getValue m))

  LocalDateTime
  (date [dt] (.toLocalDate dt))
  (time [dt] (.toLocalTime dt))
  (day [dt] (day (date dt)))
  (day-of-month [dt] (day-of-month (date dt)))
  (year-month [dt] (year-month (date dt)))
  (year [dt] (year (date dt)))
  (local-date-time [ldt] ldt)

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
  (date [zdt] (.toLocalDate zdt))
  (time [zdt] (.toLocalTime zdt))
  (inst [zdt] (inst (instant zdt)))
  (instant [zdt] (.toInstant zdt))
  (zone [zdt] (.getZone zdt)))

;; Fields

(def field-map
  {:aligned-day-of-week-in-month ChronoField/ALIGNED_DAY_OF_WEEK_IN_MONTH
   :aligned-day-of-week-in-year ChronoField/ALIGNED_DAY_OF_WEEK_IN_YEAR
   :aligned-week-of-month ChronoField/ALIGNED_WEEK_OF_MONTH
   :aligned-week-of-year ChronoField/ALIGNED_WEEK_OF_YEAR
   :ampm-of-day ChronoField/AMPM_OF_DAY
   :clock-hour-of-ampm ChronoField/CLOCK_HOUR_OF_AMPM
   :clock-hour-of-day ChronoField/CLOCK_HOUR_OF_DAY
   :day-of-month ChronoField/DAY_OF_MONTH
   :day-of-week ChronoField/DAY_OF_WEEK
   :day-of-year ChronoField/DAY_OF_YEAR
   :epoch-day ChronoField/EPOCH_DAY
   :era ChronoField/ERA
   :hour-of-ampm ChronoField/HOUR_OF_AMPM
   :hour-of-day ChronoField/HOUR_OF_DAY
   :instant-seconds ChronoField/INSTANT_SECONDS
   :micro-of-day ChronoField/MICRO_OF_DAY
   :micro-of-second ChronoField/MICRO_OF_SECOND
   :milli-of-day ChronoField/MILLI_OF_DAY
   :milli-of-second ChronoField/MILLI_OF_SECOND
   :minute-of-day ChronoField/MINUTE_OF_DAY
   :minute-of-hour ChronoField/MINUTE_OF_HOUR
   :month-of-year ChronoField/MONTH_OF_YEAR
   :nano-of-day ChronoField/NANO_OF_DAY
   :nano-of-second ChronoField/NANO_OF_SECOND
   :offset-seconds ChronoField/OFFSET_SECONDS
   :proleptic-month ChronoField/PROLEPTIC_MONTH
   :second-of-day ChronoField/SECOND_OF_DAY
   :second-of-minute ChronoField/SECOND_OF_MINUTE
   :year ChronoField/YEAR
   :year-of-era ChronoField/YEAR_OF_ERA})

(deftype FieldsLookup [t]
  ILookup
  (valAt [_ fld]
    (when-let [f (get field-map fld)]
      (.getLong t f)))
  (valAt [_ fld notfound]
    (if-let [f (get field-map fld)]
      (try
        (.getLong t f)
        (catch java.time.temporal.UnsupportedTemporalTypeException e
          notfound))
      notfound)))

(defn fields [t]
  (->FieldsLookup t))

;; With

(defn with
  "Adjust a temporal with an adjuster or field"
  ([t adj]
   (.with t adj)
   )
  ([t fld new-value]
   (when-let [f (get field-map fld)]
     (.with t f new-value))))

;; Built-in adjusters

(defn day-of-week-in-month
  ([ordinal day-of-week] (TemporalAdjusters/dayOfWeekInMonth ordinal (day day-of-week)))
  ([t ordinal day-of-week] (with t (day-of-week-in-month ordinal day-of-week))))

(defn first-day-of-month
  ([] (TemporalAdjusters/firstDayOfMonth))
  ([t] (with t (first-day-of-month))))

(defn first-day-of-next-month
  ([] (TemporalAdjusters/firstDayOfNextMonth))
  ([t] (with t (first-day-of-next-month))))

(defn first-day-of-next-year
  ([] (TemporalAdjusters/firstDayOfNextYear))
  ([t] (with t (first-day-of-next-year))))

(defn first-day-of-year
  ([] (TemporalAdjusters/firstDayOfYear))
  ([t] (with t (first-day-of-year))))

(defn first-in-month
  ([day-of-week] (TemporalAdjusters/firstInMonth (day day-of-week)))
  ([t day-of-week] (with t (first-in-month day-of-week))))

(defn last-day-of-month
  ([] (TemporalAdjusters/lastDayOfMonth))
  ([t] (with t (last-day-of-month))))

(defn last-day-of-year
  ([] (TemporalAdjusters/lastDayOfYear))
  ([t] (with t (last-day-of-year))))

(defn last-in-month
  ([day-of-week] (TemporalAdjusters/lastInMonth (day day-of-week)))
  ([t day-of-week] (with t (last-in-month day-of-week))))

(defn next
  ([day-of-week] (TemporalAdjusters/next (day day-of-week)))
  ([t day-of-week] (with t (next day-of-week))))

(defn next-or-same
  ([day-of-week] (TemporalAdjusters/nextOrSame (day day-of-week)))
  ([t day-of-week] (with t (next-or-same day-of-week))))

(defn previous
  ([day-of-week] (TemporalAdjusters/previous (day day-of-week)))
  ([t day-of-week] (with t (previous day-of-week))))

(defn previous-or-same
  ([day-of-week] (TemporalAdjusters/previousOrSame (day day-of-week)))
  ([t day-of-week] (with t (previous-or-same day-of-week))))

;; Comparison

(defprotocol ITimeComparison
  (< [x y] "Is x before y?")
  (<= [x y] "Is x before or at the same time as y?")
  (> [x y] "Is x after y?")
  (>= [x y] "Is x after or at the same time as y?"))

(extend-protocol ITimeComparison
  Object
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y))))

;; Units

(def unit-map
  {:nanos ChronoUnit/NANOS
   :micros ChronoUnit/MICROS
   :millis ChronoUnit/MILLIS
   :seconds ChronoUnit/SECONDS
   :minutes ChronoUnit/MINUTES
   :hours ChronoUnit/HOURS
   :half-days ChronoUnit/HALF_DAYS
   :days ChronoUnit/DAYS
   :weeks ChronoUnit/WEEKS
   :months ChronoUnit/MONTHS
   :years ChronoUnit/YEARS
   :decades ChronoUnit/DECADES
   :centuries ChronoUnit/CENTURIES
   :millennia ChronoUnit/MILLENNIA
   :eras ChronoUnit/ERAS
   :forever ChronoUnit/FOREVER})

(def reverse-unit-map (into {} (map vec (map reverse unit-map))))

(defn units [x]
  (into {}
        (for [tu (.getUnits x)
              :let [k (reverse-unit-map tu)]
              :when k]
          [k (.get x tu)])))

(defn truncate [x u]
  (when-let [u (get unit-map u)]
    (.truncatedTo x  u)))

;; Durations

(defprotocol IDuration
  (nanos [_] "Return the given quantity in nanoseconds.")
  (micros [_] "Return the given quantity in microseconds.")
  (millis [_] "Return the given quantity in milliseconds.")
  (seconds [_] "Return the given quantity in seconds.")
  (minutes [_] "Return the given quantity in minutes.")
  (hours [_] "Return the given quantity in hours.")
  (days [_] "Return the given quantity in days."))

(extend-protocol IDuration
  Number
  (nanos [n] (Duration/ofNanos n))
  (micros [n] (Duration/ofNanos (* n 1000)))
  (millis [n] (Duration/ofMillis n))
  (seconds [n] (Duration/ofSeconds n))
  (minutes [n] (Duration/ofMinutes n))
  (hours [n] (Duration/ofHours n))
  (days [n] (Duration/ofDays n))

  Duration
  (nanos [d] (.toNanos d))
  (micros [d] (Long/divideUnsigned (nanos d) 1000))
  (millis [d] (.toMillis d))
  (seconds [d] (.getSeconds d))
  (minutes [d] (.toMinutes d))
  (hours [d] (.toHours d))
  (days [d] (.toDays d)))

(defn duration [n u]
  (let [unit (unit-map u)]
    (assert unit (str "Not a unit: " u))
    (Duration/of n unit)))

(defprotocol IPeriod
  (weeks [_] "Return the given quantity in weeks.")
  (months [_] "Return the given quantity in months.")
  (years [_] "Return the given quantity in years."))

(extend-protocol IPeriod
  Number
  (weeks [n] (Period/ofWeeks n))
  (months [n] (Period/ofMonths n))
  (years [n] (Period/ofYears n)))

(defn period [n u]
  (case u
    :days (Period/ofDays n)
    :weeks (Period/ofWeeks n)
    :months (Period/ofMonths n)
    :years (Period/ofYears n)))

;; Arithmetic

(defprotocol ITimeArithmetic
  (+ [_ _] "Add to time")
  (- [_] [_ _] "Subtract from time, or negate"))

(extend-protocol ITimeArithmetic
  Object
  (+ [t d] (.plus t d))
  (- [d] (.negated d))
  (- [t d] (.minus t d)))

(defprotocol ITimeIncDec
  (inc [_] "Increment time")
  (dec [_] "Decrement time"))

(defprotocol ITimeRangeable
  (range [_] [_ _] [_ _ _] "Returns a lazy seq of times from start (inclusive) to end (exclusive, nil means forever), by step, where start defaults to 0, step to 1, and end to infinity."))

(defn greater [x y]
  (if (neg? (compare x y)) y x))

(defn max [arg & args]
  (reduce #(greater %1 %2) arg args))

(defn lesser [x y]
  (if (neg? (compare x y)) x y))

(defn min [arg & args]
  (reduce #(lesser %1 %2) arg args))

(extend-type Instant
  ITimeIncDec
  (inc [t] (+ t (seconds 1)))
  (dec [t] (- t (seconds 1)))

  ITimeRangeable
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type ZonedDateTime
  ITimeIncDec
  (inc [t] (+ t (seconds 1)))
  (dec [t] (- t (seconds 1)))

  ITimeRangeable
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type LocalDate
  ITimeIncDec
  (inc [t] (.plusDays t 1))
  (dec [t] (.minusDays t 1))

  ITimeRangeable
  (range
    ([from] (iterate #(.plusDays % 1) from))
    ([from to] (cond->> (iterate #(.plusDays % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plusDays % step) from)
                      to (take-while #(< % to))))))

(defn tomorrow []
  (inc (today)))

(defn yesterday []
  (dec (today)))

(extend-type LocalDateTime
  ITimeIncDec
  (inc [t] (+ t (seconds 1)))
  (dec [t] (- t (seconds 1)))

  ITimeRangeable
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type YearMonth
  ITimeIncDec
  (inc [t] (.plusMonths t 1))
  (dec [t] (.minusMonths t 1))

  ITimeRangeable
  (range
    ([from] (iterate #(.plusMonths % 1) from))
    ([from to] (cond->> (iterate #(.plusMonths % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type Year
  ITimeIncDec
  (inc [t] (.plusYears t 1))
  (dec [t] (.minusYears t 1))

  ITimeRangeable
  (range
    ([from] (iterate #(.plusYears % 1) from))
    ([from to] (cond->> (iterate #(.plusYears % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(defprotocol IDivisible
  (/ [_ _] "Divide time"))

(extend-protocol IDivisible
  String
  (/ [s d] (/ (parse s) d)))

(defprotocol IDivisibleDuration
  (divide-duration [divisor duration] "Divide a duration"))

(extend-protocol IDivisibleDuration
  Long
  (divide-duration [n duration] (.dividedBy duration n))
  Duration
  (divide-duration [divisor duration]
    (clojure.core// (.getSeconds duration) (.getSeconds divisor))))

(extend-type Duration
  ITimeIncDec
  (inc [d] (.plusSeconds d 1))
  (dec [d] (.minusSeconds d 1))

  IDivisible
  (/ [d x] (divide-duration x d)))

(defprotocol ITimeSpan
  (beginning [_] "Return the beginning of a span of time")
  (end [_] "Return the end of a span of time"))

(defprotocol ITimeBetween
  (between [t1 t2] "Return the most appropriate type of value that
  represents the gap between two times"))

(extend-protocol ITimeBetween
  Instant
  (between [inst _] (throw (ex-info "TODO" {}))))

(defn length
  "Return the distance between the beginning and end as a duration or
  period"
  [v] (Duration/between (beginning v) (end v)))

;; TODO: Test concurrent? in tick.core-test

(defn coincident?
  "Does the span of time contain the given event? If the given event
  is itself a span, then t must wholly contain the beginning and end
  of the event."
  [t event]
  (and
    (not= 1 (compare (beginning t) (beginning event)))
    (not= 1 (compare (end event) (end t)))))

(extend-protocol ITimeSpan
  String
  (beginning [s] (beginning (parse s)))
  (end [s] (end (parse s)))

  Number
  (beginning [n] (beginning (time n)))
  (end [n] (end (time n)))

  LocalDate
  (beginning [date] (.atStartOfDay date))
  (end [date] (.atStartOfDay (inc date)))

  Year
  (beginning [year] (beginning (.atMonth year 1)))
  (end [year] (end (.atMonth year 12)))

  YearMonth
  (beginning [ym] (beginning (.atDay ym 1)))
  (end [ym] (end (.atEndOfMonth ym)))

  Instant
  (beginning [i] i)
  (end [i] i)

  ZonedDateTime
  (beginning [i] i)
  (end [i] i)

  OffsetDateTime
  (beginning [i] i)
  (end [i] i)

  Date
  (beginning [i] (instant i))
  (end [i] (instant i))

  LocalDateTime
  (beginning [x] x)
  (end [x] x)

  LocalTime
  (beginning [x] x)
  (end [x] x)

  nil
  (beginning [_] nil)
  (end [_] nil))

;; Private
(defprotocol ILocalDateTimeReify
  (in* [arg ldt] "Reify a LocalDateTime"))

(extend-protocol ITimeReify
  LocalTime
  (on [t date] (.atTime date t))
  OffsetTime
  (on [t date] (.atTime date t))
  LocalDate
  (at [date t] (.atTime date (time t)))
  LocalDateTime
  (in [ldt z] (.atZone ldt (zone z)))
  Instant
  (in [t z] (in [t z] (.atZone t (zone z))))
  ZonedDateTime
  (in [t z] (.withZoneSameInstant t (zone z)))
  Date
  (in [t z] (in (instant t) (zone z))))

(defprotocol ILocalTime
  (local? [t] "Is the time a java.time.LocalTime or java.time.LocalDateTime?"))

(extend-protocol ILocalTime
  Date
  (local? [d] false)

  Instant
  (local? [i] false)

  LocalDateTime
  (local? [i] true)

  LocalTime
  (local? [i] true)

  nil
  (local? [_] nil))

(defprotocol MinMax
  (min-of-type [_] "Return the min")
  (max-of-type [_] "Return the max"))

(extend-protocol MinMax
  LocalTime
  (min-of-type [_] (LocalTime/MIN))
  (max-of-type [_] (LocalTime/MAX))
  LocalDateTime
  (min-of-type [_] (LocalDateTime/MIN))
  (max-of-type [_] (LocalDateTime/MAX))
  Instant
  (min-of-type [_] (Instant/MIN))
  (max-of-type [_] (Instant/MAX))
  ;; TODO: This may cause surprises - see clojure/java-time. We should
  ;; change the semantics of nil to not imply epoch, forever, or
  ;; whatever.
  nil
  (min-of-type [_] (Instant/MIN))
  (max-of-type [_] (Instant/MAX)))


;; first/last using java.time.temporal/TemporalAdjuster
;; See also java.time.temporal/TemporalAdjusters

;; java.time.temporal/TemporalAmount

#_(defn adjust [t adjuster]
  (.with t adjuster))

;; adjust

;; Conversions

;; Ago/hence

(defn ago [dur]
  (- (now) dur))

(defn hence [dur]
  (+ (now) dur))

(defn midnight? [^LocalDateTime t]
  (.isZero (Duration/between t (beginning (date t)))))
