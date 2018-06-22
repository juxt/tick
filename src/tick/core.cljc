;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [+ - / inc dec max min range time int long < <= > >= next >> << atom swap! swap-vals! compare-and-set! reset! reset-vals! second divide])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [tick.interop :as t.i]
    #?(:cljs
       [tick.js-joda :refer [Date Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime ChronoUnit ChronoField TemporalAdjusters]]))
  #?(:clj
     (:import
       [java.util Date]
       [java.time Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime]
       [java.time.format DateTimeFormatter]
       [java.time.temporal ChronoUnit ChronoField TemporalAdjusters]
       [clojure.lang ILookup Seqable])))

(def ^{:dynamic true} *clock* nil)

(defn now []
  (if *clock*
    (. Instant now *clock*)
    (. Instant now)))

(defn today []
  (if *clock*
    (. LocalDate now *clock*)
    (. LocalDate now)))

(defn epoch []
  (t.i/static-prop Instant EPOCH))

(defprotocol ITimeReify
  (on [time date] "Set time be ON a date")
  (at [date time] "Set date to be AT a time")
  (in [dt zone] "Set a date-time to be in a time-zone")
  (offset-by [dt amount] "Set a date-time to be offset by an amount"))

(defn midnight
  ([] (t.i/static-prop LocalTime MIDNIGHT))
  ([^LocalDate date]
   (at date (t.i/static-prop LocalTime MIDNIGHT))))

(defn noon
  ([] (t.i/static-prop LocalTime NOON))
  ([^LocalDate date]
   (at date (t.i/static-prop LocalTime NOON))))

(s/def ::instant #(instance? Instant %))

(defn parse-day [input]
  (condp re-matches (str/lower-case input)
    #"(mon)(day)?" (t.i/static-prop DayOfWeek MONDAY)
    #"(tue)(s|sday)?" (t.i/static-prop DayOfWeek TUESDAY)
    #"(wed)(s|nesday)?" (t.i/static-prop DayOfWeek WEDNESDAY)
    #"(thur)(s|sday)?" (t.i/static-prop DayOfWeek THURSDAY)
    #"(fri)(day)?" (t.i/static-prop DayOfWeek FRIDAY)
    #"(sat)(urday)?" (t.i/static-prop DayOfWeek SATURDAY)
    #"(sun)(day)?" (t.i/static-prop DayOfWeek SUNDAY)
    nil))

(defn parse-month [input]
  (condp re-matches (str/lower-case input)
    #"(jan)(uary)?" (t.i/static-prop Month JANUARY)
    #"(feb)(ruary)?" (t.i/static-prop Month FEBRUARY)
    #"(mar)(ch)?" (t.i/static-prop Month MARCH)
    #"(apr)(il)?" (t.i/static-prop Month APRIL)
    #"may" (t.i/static-prop Month MAY)
    #"(jun)(e)?" (t.i/static-prop Month JUNE)
    #"(jul)(y)?" (t.i/static-prop Month JULY)
    #"(aug)(ust)?" (t.i/static-prop Month AUGUST)
    #"(sep)(tember)?" (t.i/static-prop Month SEPTEMBER)
    #"(oct)(tober)?" (t.i/static-prop Month OCTOBER)
    #"(nov)(ember)?" (t.i/static-prop Month NOVEMBER)
    #"(dec)(ember)?" (t.i/static-prop Month DECEMBER)
    nil))

(defprotocol IParseable
  (parse [_] "Parse to most applicable instance."))

(defn parse-int [x]
  #?(:clj (Integer/parseInt x)
     :cljs (js/Number x)))

(extend-protocol IParseable
  #?(:clj String :cljs string)
  (parse [s]
    (condp re-matches s
      #"(\d{1,2})\s*(am|pm)"
      :>> (fn [[_ h ap]] (. LocalTime of (cond-> (parse-int h) (= "pm" ap) (clojure.core/+ 12)) 0))
      #"(\d{1,2})"
      :>> (fn [[_ h]] (. LocalTime of (parse-int h) 0))
      #"\d{2}:\d{2}\S*"
      :>> (fn [s] (. LocalTime parse s))
      #"(\d{1,2}):(\d{2})"
      :>> (fn [[_ h m]] (. LocalTime of (parse-int h) (parse-int m)))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z"
      :>> (fn [s] (. Instant parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}"
      :>> (fn [s] (. OffsetDateTime parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}\[\w+/\w+\]"
      :>> (fn [s] (. ZonedDateTime parse s))
      #"\d{4}-\d{2}-\d{2}T\S*"
      :>> (fn [s] (. LocalDateTime parse s))
      #"\d{4}-\d{2}-\d{2}"
      :>> (fn [s] (. LocalDate parse s))
      #"\d{4}-\d{2}"
      :>> (fn [s] (. YearMonth parse s))
      #"\d{4}"
      :>> (fn [s] (. Year parse s))
      (throw (ex-info "Unparseable time string" {:input s})))))

(defprotocol ICoercions
  (date [_] "Make a java.time.LocalDate instance.")
  (time [_] "Make a java.time.LocalTime instance.")
  (nanosecond [_] "Return the millisecond field of the given time")
  (microsecond [_] "Return the millisecond field of the given time")
  (millisecond [_] "Return the millisecond field of the given time")
  (second [_] "Return the second field of the given time")
  (minute [_] "Return the minute field of the given time")
  (hour [_] "Return the hour field of the given time")
  ;; Should this be day-of-week or at least an alias?
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
  (local-date-time [_] "Make a java.time.LocalDateTime instance.")
  (local-time [_] "Make a java.time.LocalTime instance.")
  (temporal-value [_] "Coerce to a preferred value"))

(defn current-zone
  "Return the current zone, which can be overridden by the *clock* dynamic var"
  []
  (if-let [clk *clock*]
    (.getZone clk)
    (. ZoneId systemDefault)))

(extend-protocol ICoercions
  #?(:clj Object :cljs object)
  (int [v] (#?(:clj clojure.core/int :cljs parse-int)  v))
  (long [v] (#?(:clj clojure.core/long :cljs parse-int) v))

  #?(:clj clojure.lang.Fn :cljs function)
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
  (local-time [f] (local-time (f)))
  (temporal-value [f] (temporal-value (f)))

  Instant
  (date [i] (date (zoned-date-time i)))
  (time [i] (time (zoned-date-time i)))
  (nanosecond [t] (nanosecond (zoned-date-time t)))
  (microsecond [t] (microsecond (zoned-date-time t)))
  (millisecond [t] (millisecond (zoned-date-time t)))
  (second [t] (second (zoned-date-time t)))
  (minute [t] (minute (zoned-date-time t)))
  (hour [t] (hour (zoned-date-time t)))
  (day [i] (day (date i)))
  (day-of-month [i] (day-of-month (date i)))
  (inst [i] #?(:clj (Date/from i) :cljs (new Date. (.toEpochMilli i))))
  (instant [i] i)
  (int [i] (t.i/getter nano i))
  (long [i] (t.i/getter epochSecond i))
  (month [i] (month (date i)))
  (year [i] (year (date i)))
  (year-month [i] (year-month (date i)))
  (local-date-time [i] (local-date-time (zoned-date-time i)))
  (local-time [i] (.toLocalTime (zoned-date-time i)))
  (zoned-date-time [i] (. ZonedDateTime ofInstant i (current-zone)))
  (offset-date-time [i] (. OffsetDateTime ofInstant i (current-zone)))
  (temporal-value [i] i)

  #?(:clj String :cljs string)
  (inst [s] (inst (instant s)))
  (instant [s] (instant (parse s)))
  (day [s] (or (parse-day s) (day (date s))))
  (day-of-month [s] (day-of-month (date s)))
  (date [s] (date (parse s)))
  (time [s] (time (parse s)))
  (month [s] (or (parse-month s) (month (date s))))
  (year [s] (year (parse s)))
  (year-month [s] (year-month (parse s)))
  (zone [s] (. ZoneId of s))
  (zone-offset [s] (. ZoneOffset of s))
  (int [s] ((t.i/getter nano (instant s))))
  (long [s] ((t.i/getter epochSecond (instant s))))
  (local-date-time [s] (. LocalDateTime parse s))
  (local-time [s] (. LocalTime parse s))
  (zoned-date-time [s] (. ZonedDateTime parse s))
  (offset-date-time [s] (. OffsetDateTime parse s))
  (temporal-value [s] (temporal-value (parse s)))

  #?(:clj Number :cljs number)
  (day [n] (. DayOfWeek of n))
  (month [n] (. Month of n))
  (instant [n] (. Instant ofEpochMilli n))
  (year [n] (. Year of n))
  (zone-offset [s] (. ZoneOffset ofHours s))
  (local-time [n] (. LocalTime of n 0))

  LocalDate
  (date [d] d)
  (day [d] (t.i/getter dayOfWeek d))
  (day-of-month [d] (t.i/getter dayOfMonth d))
  (month [d] (. Month from d))
  (year-month [d] (. YearMonth of (t.i/getter year d) (t.i/getter monthValue d)))
  (year [d] (. Year of (t.i/getter year d)))
  (temporal-value [d] d)

  LocalTime
  (time [t] t)
  (nanosecond [t] (.get t (t.i/static-prop ChronoField NANO_OF_SECOND)))
  (microsecond [t] (.get t (t.i/static-prop ChronoField MICRO_OF_SECOND)))
  (millisecond [t] (.get t (t.i/static-prop ChronoField MILLI_OF_SECOND)))
  (second [t] (t.i/getter second t))
  (minute [t] (t.i/getter minute t))
  (hour [t] (t.i/getter hour t))
  (local-time [t] t)
  (temporal-value [t] t)

  Month
  (int [m] (t.i/getter value m)) ;todo

  LocalDateTime
  (date [dt] (.toLocalDate dt))
  (time [dt] (.toLocalTime dt))
  (second [t] (t.i/getter second t))
  (minute [t] (t.i/getter minute t))
  (hour [t] (t.i/getter hour t))
  (day [dt] (day (date dt)))
  (day-of-month [dt] (day-of-month (date dt)))
  (year-month [dt] (year-month (date dt)))
  (year [dt] (year (date dt)))
  (local-date-time [ldt] ldt)
  (offset-date-time [ldt] (.atOffset ldt (. ZoneOffset systemDefault)))
  (zoned-date-time [ldt] (.atZone ldt (. ZoneId systemDefault)))
  (local-time [ldt] (.toLocalTime ldt))
  (temporal-value [ldt] ldt)

  Date
  (inst [d] d)
  (instant [d] #?(:clj (.toInstant d) :cljs (.ofEpochMilli Instant (.getTime d))))
  (date [d] (date (zoned-date-time (instant d)))) ; implicit conversion to UTC
  (year-month [d] (year-month (date d)))
  (year [d] (year (date d)))
  (local-date-time [d] (local-date-time (instant d)))
  (local-time [d] (local-time (local-date-time d)))
  (zoned-date-time [d] (zoned-date-time (instant d)))
  (offset-date-time [d] (offset-date-time (instant d)))
  (temporal-value [d] (instant d))

  YearMonth
  (year-month [ym] ym)
  (year [ym] (year (t.i/getter year ym)))
  (temporal-value [ym] ym)

  Year
  (year [y] y)
  (int [y] (t.i/getter value y))
  (temporal-value [y] y)

  ZoneId
  (zone [z] z)

  ZoneOffset
  (zone-offset [z] z)

  OffsetDateTime
  (time [odt] (.toLocalTime odt))
  (local-date-time [odt] (.toLocalDateTime odt))
  (offset-date-time [odt] odt)
  (zoned-date-time [odt] (.toZonedDateTime odt))
  (local-time [odt] (.toLocalTime odt))
  (temporal-value [odt] odt)

  ;; TODO: Add local-time
  ZonedDateTime
  (date [zdt] (.toLocalDate zdt))
  (time [zdt] (.toLocalTime zdt))
  (nanosecond [t] (.get t (t.i/static-prop ChronoField NANO_OF_SECOND)))
  (microsecond [t] (.get t (t.i/static-prop ChronoField MICRO_OF_SECOND)))
  (millisecond [t] (.get t (t.i/static-prop ChronoField MILLI_OF_SECOND)))
  (second [t] (t.i/getter second t))
  (minute [t] (t.i/getter minute t))
  (hour [t] (t.i/getter hour t))
  (day [t] (t.i/getter dayOfWeek t))
  (day-of-month [t] (t.i/getter dayOfMonth t))
  (inst [zdt] (inst (instant zdt)))
  (instant [zdt] (.toInstant zdt))
  (month [zdt] (t.i/getter month zdt))
  (local-date-time [zdt] (.toLocalDateTime zdt))
  (local-time [zdt] (.toLocalTime zdt))
  (offset-date-time [zdt] (.toOffsetDateTime zdt))
  (zoned-date-time [zdt] zdt)
  (zone [zdt] (t.i/getter zone zdt))
  (temporal-value [zdt] zdt))

;; Fields

(def field-map
  {:aligned-day-of-week-in-month (t.i/static-prop ChronoField ALIGNED_DAY_OF_WEEK_IN_MONTH)
   :aligned-day-of-week-in-year (t.i/static-prop ChronoField ALIGNED_DAY_OF_WEEK_IN_YEAR)
   :aligned-week-of-month (t.i/static-prop ChronoField ALIGNED_WEEK_OF_MONTH)
   :aligned-week-of-year (t.i/static-prop ChronoField ALIGNED_WEEK_OF_YEAR)
   :ampm-of-day (t.i/static-prop ChronoField AMPM_OF_DAY)
   :clock-hour-of-ampm (t.i/static-prop ChronoField CLOCK_HOUR_OF_AMPM)
   :clock-hour-of-day (t.i/static-prop ChronoField CLOCK_HOUR_OF_DAY)
   :day-of-month (t.i/static-prop ChronoField DAY_OF_MONTH)
   :day-of-week (t.i/static-prop ChronoField DAY_OF_WEEK)
   :day-of-year (t.i/static-prop ChronoField DAY_OF_YEAR)
   :epoch-day (t.i/static-prop ChronoField EPOCH_DAY)
   :era (t.i/static-prop ChronoField ERA)
   :hour-of-ampm (t.i/static-prop ChronoField HOUR_OF_AMPM)
   :hour-of-day (t.i/static-prop ChronoField HOUR_OF_DAY)
   :instant-seconds (t.i/static-prop ChronoField INSTANT_SECONDS)
   :micro-of-day (t.i/static-prop ChronoField MICRO_OF_DAY)
   :micro-of-second (t.i/static-prop ChronoField MICRO_OF_SECOND)
   :milli-of-day (t.i/static-prop ChronoField MILLI_OF_DAY)
   :milli-of-second (t.i/static-prop ChronoField MILLI_OF_SECOND)
   :minute-of-day (t.i/static-prop ChronoField MINUTE_OF_DAY)
   :minute-of-hour (t.i/static-prop ChronoField MINUTE_OF_HOUR)
   :month-of-year (t.i/static-prop ChronoField MONTH_OF_YEAR)
   :nano-of-day (t.i/static-prop ChronoField NANO_OF_DAY)
   :nano-of-second (t.i/static-prop ChronoField NANO_OF_SECOND)
   :offset-seconds (t.i/static-prop ChronoField OFFSET_SECONDS)
   :proleptic-month (t.i/static-prop ChronoField PROLEPTIC_MONTH)
   :second-of-day (t.i/static-prop ChronoField SECOND_OF_DAY)
   :second-of-minute (t.i/static-prop ChronoField SECOND_OF_MINUTE)
   :year (t.i/static-prop ChronoField YEAR)
   :year-of-era (t.i/static-prop ChronoField YEAR_OF_ERA)})

(deftype FieldsLookup [t]
  #?(:clj Seqable :cljs ISeqable)
  (#?(:cljs -seq :clj seq) [_]
    (->> field-map
         (keep (fn [[k v]]
                 (let [cf (get field-map k)]
                   (when (.isSupported t cf)
                     [k (t.i/getter long t cf)]))))
         (into {})
         seq))
  ILookup
  (#?(:clj valAt :cljs -lookup) [_ fld]
    (when-let [f (get field-map fld)]
      (t.i/getter long t f)))
  (#?(:clj valAt :cljs -lookup) [_ fld notfound]
    (if-let [f (get field-map fld)]
      (try
        (t.i/getter long t f)
        (catch #?(:clj java.time.temporal.UnsupportedTemporalTypeException :cljs js/Error) e
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
  ([ordinal day-of-week] (. TemporalAdjusters dayOfWeekInMonth ordinal (day day-of-week)))
  ([t ordinal day-of-week] (with t (day-of-week-in-month ordinal day-of-week))))

(defn first-day-of-month
  ([] (. TemporalAdjusters firstDayOfMonth))
  ([t] (with t (first-day-of-month))))

(defn first-day-of-next-month
  ([] (. TemporalAdjusters firstDayOfNextMonth))
  ([t] (with t (first-day-of-next-month))))

(defn first-day-of-next-year
  ([] (. TemporalAdjusters firstDayOfNextYear))
  ([t] (with t (first-day-of-next-year))))

(defn first-day-of-year
  ([] (. TemporalAdjusters firstDayOfYear))
  ([t] (with t (first-day-of-year))))

(defn first-in-month
  ([day-of-week] (. TemporalAdjusters firstInMonth (day day-of-week)))
  ([t day-of-week] (with t (first-in-month day-of-week))))

(defn last-day-of-month
  ([] (. TemporalAdjusters lastDayOfMonth))
  ([t] (with t (last-day-of-month))))

(defn last-day-of-year
  ([] (. TemporalAdjusters lastDayOfYear))
  ([t] (with t (last-day-of-year))))

(defn last-in-month
  ([day-of-week] (. TemporalAdjusters lastInMonth (day day-of-week)))
  ([t day-of-week] (with t (last-in-month day-of-week))))

(defn next
  ([day-of-week] (. TemporalAdjusters next (day day-of-week)))
  ([t day-of-week] (with t (next day-of-week))))

(defn next-or-same
  ([day-of-week] (. TemporalAdjusters nextOrSame (day day-of-week)))
  ([t day-of-week] (with t (next-or-same day-of-week))))

(defn previous
  ([day-of-week] (. TemporalAdjusters previous (day day-of-week)))
  ([t day-of-week] (with t (previous day-of-week))))

(defn previous-or-same
  ([day-of-week] (. TemporalAdjusters previousOrSame (day day-of-week)))
  ([t day-of-week] (with t (previous-or-same day-of-week))))

;; Comparison

(defprotocol ITimeComparison
  (< [x y] "Is x before y?")
  (<= [x y] "Is x before or at the same time as y?")
  (> [x y] "Is x after y?")
  (>= [x y] "Is x after or at the same time as y?"))

(extend-protocol ITimeComparison
  Instant
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  LocalDateTime
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  ;Date
  ;(-compare [x y] (.compareTo x y))
  LocalDate
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  LocalTime
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  LocalDateTime
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  OffsetDateTime
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  ZonedDateTime
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  Year
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y)))
  YearMonth
  (< [x y] (.isBefore x y))
  (<= [x y] (not (.isAfter x y)))
  (> [x y] (.isAfter x y))
  (>= [x y] (not (.isBefore x y))))

#?(:cljs
   (extend-protocol IComparable
     Instant
     (-compare [x y] (.compareTo x y))
     LocalDateTime
     (-compare [x y] (.compareTo x y))
     Date
     (-compare [x y] (.compareTo x y))
     LocalDate
     (-compare [x y] (.compareTo x y))
     LocalTime
     (-compare [x y] (.compareTo x y))
     LocalDateTime
     (-compare [x y] (.compareTo x y))
     OffsetDateTime
     (-compare [x y] (.compareTo x y))
     ZonedDateTime
     (-compare [x y] (.compareTo x y))
     Year
     (-compare [x y] (.compareTo x y))
     YearMonth
     (-compare [x y] (.compareTo x y))))

#?(:cljs
   (extend-protocol IEquiv
     Instant
     (-equiv [x y] (.equals x y))
     LocalDateTime
     (-equiv [x y] (.equals x y))
     Date
     (-equiv [x y] (= (.getTime x) (.getTime y)))
     LocalDate
     (-equiv [x y] (.equals x y))
     LocalTime
     (-equiv [x y] (.equals x y))
     LocalDateTime
     (-equiv [x y] (.equals x y))
     OffsetDateTime
     (-equiv [x y] (.equals x y))
     ZonedDateTime
     (-equiv [x y] (.equals x y))
     Year
     (-equiv [x y] (.equals x y))
     YearMonth
     (-equiv [x y] (.equals x y))
     Duration
     (-equiv [x y] (.equals x y))))

;; Units

(def unit-map
  {:nanos (t.i/static-prop ChronoUnit NANOS)
   :micros (t.i/static-prop ChronoUnit MICROS)
   :millis (t.i/static-prop ChronoUnit MILLIS)
   :seconds (t.i/static-prop ChronoUnit SECONDS)
   :minutes (t.i/static-prop ChronoUnit MINUTES)
   :hours (t.i/static-prop ChronoUnit HOURS)
   :half-days (t.i/static-prop ChronoUnit HALF_DAYS)
   :days (t.i/static-prop ChronoUnit DAYS)
   :weeks (t.i/static-prop ChronoUnit WEEKS)
   :months (t.i/static-prop ChronoUnit MONTHS)
   :years (t.i/static-prop ChronoUnit YEARS)
   :decades (t.i/static-prop ChronoUnit DECADES)
   :centuries (t.i/static-prop ChronoUnit CENTURIES)
   :millennia (t.i/static-prop ChronoUnit MILLENNIA)
   :eras (t.i/static-prop ChronoUnit ERAS)
   :forever (t.i/static-prop ChronoUnit FOREVER)})

(def reverse-unit-map (into {} (map vec (map reverse unit-map))))

(defn units [x]
  (into {}
        (for [tu (t.i/getter units x)
              :let [k (reverse-unit-map tu)]
              :when k]
          [k (.get x tu)])))

(defn truncate [x u]
  (when-let [u (get unit-map u)]
    (.truncatedTo x  u)))

;; Durations & Periods

(defprotocol ITimeLength
  (nanos [_] "Return the given quantity in nanoseconds.")
  (micros [_] "Return the given quantity in microseconds.")
  (millis [_] "Return the given quantity in milliseconds.")
  (seconds [_] "Return the given quantity in seconds.")
  (minutes [_] "Return the given quantity in minutes.")
  (hours [_] "Return the given quantity in hours.")
  (days [_] "Return the given quantity in days.")
  (months [_] "Return the given quantity in months.")
  (years [_] "Return the given quantity in years."))

(extend-protocol ITimeLength
  Duration
  (nanos [d] (.toNanos d))
  (micros [d] (#?(:clj Long/divideUnsigned :cljs cljs.core//) (nanos d) 1000))
  (millis [d] (.toMillis d))
  (seconds [d] (t.i/getter seconds d))
  (minutes [d] (.toMinutes d))
  (hours [d] (.toHours d))
  (days [d] (.toDays d))

  Period
  (days [p] (.getDays p))
  (months [p] (.getMonths p))
  (years [p] (.getYears p)))

(defn make-duration [n u]
  (let [unit (unit-map u)]
    (assert unit (str "Not a unit: " u))
    (. Duration of n unit)))

(defn make-period [n u]
  (case u
    :days (. Period ofDays n)
    :weeks (. Period ofWeeks n)
    :months (. Period ofMonths n)
    :years (. Period ofYears n)))

;; Coercions

(extend-protocol ICoercions
  Duration
  (zone-offset [d] (. ZoneOffset ofTotalSeconds (make-duration 1 :seconds))))

;; Clocks

(defn current-clock []
  (or
    *clock*
    (. Clock systemDefaultZone)))

(defprotocol IClock
  (clock [_] "Make a clock"))

(extend-protocol IClock
  Instant
  (clock [i] (. Clock fixed i (. ZoneId systemDefault)))

  ZonedDateTime
  (clock [zdt] (. Clock fixed (.toInstant zdt) (t.i/getter zone zdt)))

  #?(:clj Object :cljs object)
  (clock [o] (clock (zoned-date-time o)))

  Clock
  (clock [clk] clk)

  ZoneId
  (clock [z] (. Clock system z))

  #?(:clj String :cljs string)
  (clock [s] (clock (parse s))))

(defn advance
  ([clk]
   (advance clk (make-duration 1 :seconds)))
  ([clk dur]
    (. Clock tick clk dur)))

(extend-protocol ICoercions
  Clock
  (instant [clk] (.instant clk))
  (zone [clk] (t.i/getter zone clk)))

(extend-protocol ITimeReify
  Clock
  (in [clk zone] (.withZone clk zone)))

;; Atomic clocks :)

(defrecord AtomicClock [*clock]
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] (instant @*clock))
  IClock
  (clock [_] @*clock))

#?(:clj
   (do
     (prefer-method print-method clojure.lang.IPersistentMap clojure.lang.IDeref)
     (prefer-method print-method java.util.Map clojure.lang.IDeref))
   ;todo  - for cljs
   )

(defn atom
  ([clk] (->AtomicClock (clojure.core/atom clk)))
  ([] (atom (current-clock))))

(defn swap! [at f & args]
  (apply clojure.core/swap! (:*clock at) f args))

(defn swap-vals! [at f & args]
  (apply clojure.core/swap-vals! (:*clock at) f args))

(defn compare-and-set! [at oldval newval]
  (apply clojure.core/compare-and-set! (:*clock at) oldval newval))

(defn reset! [at newval]
  (apply clojure.core/reset! (:*clock at) newval))

(defn reset-vals! [at newval]
  (apply clojure.core/reset-vals! (:*clock at) newval))

;; Arithmetic

(defprotocol ITimeArithmetic
  (+ [t d] "Add to time")
  (- [t d] "Subtract from time, or negate"))

(defn minus_
  ([d] (.negated d))
  ([t d] (.minus t d)))

(extend-protocol ITimeArithmetic
  #?(:clj Object :cljs object)
  (+ [t d] (.plus t d))
  (- [t d] (.minus t d)))

(defn negated
  "Return the duration as a negative duration"
  [d]
  (.negated d))

(defprotocol ITimeShift
  (forward-number [_ n] "Increment time")
  (forward-duration [_ d] "Increment time")
  (backward-number [_ n] "Decrement time")
  (backward-duration [_ d] "Decrement time"))

(extend-protocol ITimeShift
  Instant
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.minus t d))
  Date
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.plus t d))
  LocalDate
  (forward-number [t n] (.plusDays t n))
  (backward-number [t n] (.minusDays t n))
  LocalTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.plus t d))
  LocalDateTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.plus t d))
  OffsetDateTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.plus t d))
  ZonedDateTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.plus t d))
  Year
  (forward-number [t n] (.plusYears t n))
  (backward-number [t n] (.plusYears t n))
  YearMonth
  (forward-number [t n] (.plusMonths t n))
  (backward-number [t n] (.plusMonths t n))
  Clock
  (forward-duration [clk d] (. Clock offset clk d))
  (backward-duration [clk d] (. Clock offset clk (negated d))))

(defn >> [t n-or-d]
  (if (number? n-or-d)
    (forward-number t n-or-d)
    (forward-duration t n-or-d)))

(defn << [t n-or-d]
  (if (number? n-or-d)
    (backward-number t n-or-d)
    (backward-duration t n-or-d)))

(defprotocol ITimeRangeable
  (range [from] [from to] [from to step] "Returns a lazy seq of times from start (inclusive) to end (exclusive, nil means forever), by step, where start defaults to 0, step to 1, and end to infinity."))

(defn greater [x y]
  (if (neg? (compare x y)) y x))

(defn max [arg & args]
  (reduce #(greater %1 %2) arg args))

(defn lesser [x y]
  (if (neg? (compare x y)) x y))

(defn min [arg & args]
  (reduce #(lesser %1 %2) arg args))

(extend-type Instant
  ITimeRangeable
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type ZonedDateTime
  ITimeRangeable
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type LocalDate
  ITimeRangeable
  (range
    ([from] (iterate #(.plusDays % 1) from))
    ([from to] (cond->> (iterate #(.plusDays % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plusDays % step) from)
                      to (take-while #(< % to))))))

(defn inc [t] (forward-number t 1))
(defn dec [t] (backward-number t 1))

(defn tomorrow []
  (forward-number (today) 1))

(defn yesterday []
  (backward-number (today) 1))

(extend-type LocalDateTime
  ITimeRangeable
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type YearMonth
  ITimeRangeable
  (range
    ([from] (iterate #(.plusMonths % 1) from))
    ([from to] (cond->> (iterate #(.plusMonths % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type Year
  ITimeRangeable
  (range
    ([from] (iterate #(.plusYears % 1) from))
    ([from to] (cond->> (iterate #(.plusYears % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(defprotocol IDivisible
  (divide [t divisor] "Divide time"))

(extend-protocol IDivisible
  #?(:clj String :cljs string)
  (divide [s d] (divide (parse s) d)))

     (defprotocol IDivisibleDuration
       (divide-duration [divisor duration] "Divide a duration"))

     (extend-protocol IDivisibleDuration
       #?(:clj Long :cljs number)
       (divide-duration [n duration] (.dividedBy duration n))
       Duration
       (divide-duration [divisor duration]
          #?(:clj (clojure.core// (t.i/getter seconds duration) (t.i/getter seconds divisor))
             :cljs (cljs.core// (.seconds duration) (.seconds divisor))) ))

(extend-type Duration
  IDivisible
  (divide [d x] (divide-duration x d)))

(defprotocol ITimeSpan
  (beginning [_] "Return the beginning of a span of time")
  (end [_] "Return the end of a span of time"))

(defn duration [x]
  (. Duration between (beginning x) (end x)))

(defn- beginning-composite [m]
  (let [{:tick/keys [beginning intervals]} m]
    (if intervals
      (apply min (map :tick/beginning intervals))
      beginning)))

(defn- end-composite [m]
  (let [{:tick/keys [end intervals]} m]
    (if intervals
      (apply max (map :tick/end intervals))
      end)))

#?(:clj
   (extend-protocol ITimeSpan
     clojure.lang.APersistentMap
     (beginning [m] (beginning-composite m))
     (end [m] (end-composite m))))

#?(:cljs
   (extend-protocol ITimeSpan
     PersistentArrayMap
     (beginning [m] (beginning-composite m))
     (end [m] (end-composite m))))

#?(:cljs
   (extend-protocol ITimeSpan
     PersistentHashMap
     (beginning [m] (beginning-composite m))
     (end [m] (end-composite m))))

;; Periods

(defprotocol IBetween
  (between [v1 v2] "Return the duration (or period) between two times"))

(extend-protocol IBetween
  LocalDate
  (between [v1 v2] (. Period between v1 (date v2)))
  Instant
  (between [v1 v2] (. Duration between v1 (instant v2)))
  LocalTime
  (between [v1 v2] (. Duration between v1 (local-time v2)))
  #?(:clj String :cljs string)
  (between [v1 v2] (between (parse v1) v2)))

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
  #?(:clj String :cljs string)
  (beginning [s] (beginning (parse s)))
  (end [s] (end (parse s)))

  #?(:clj Number :cljs number)
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

(extend-protocol ITimeReify
  LocalTime
  (on [t date] (.atTime date t))
  OffsetTime
  (on [t date] (.atTime date t))
  LocalDate
  (at [date t] (.atTime date (time t)))
  LocalDateTime
  (in [ldt z] (.atZone ldt z))
  (offset-by [ldt offset] (.atOffset ldt (zone-offset offset)))
  Instant
  (in [t z] (.atZone t z))
  (offset-by [t offset] (.atOffset t (zone-offset offset)))
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
  (min-of-type [_] (t.i/static-prop LocalTime MIN))
  (max-of-type [_]  (t.i/static-prop LocalTime MAX))
  LocalDateTime
  (min-of-type [_]  (t.i/static-prop LocalDateTime MIN))
  (max-of-type [_]  (t.i/static-prop LocalDateTime MAX))
  Instant
  (min-of-type [_]  (t.i/static-prop Instant MIN))
  (max-of-type [_]  (t.i/static-prop Instant MAX))
  ;; TODO: This may cause surprises - see clojure/java-time. We should
  ;; change the semantics of nil to not imply epoch, forever, or
  ;; whatever.
  nil
  (min-of-type [_]  (t.i/static-prop Instant MIN))
  (max-of-type [_]  (t.i/static-prop Instant MAX)))


;; first/last using java.time.temporal/TemporalAdjuster
;; See also java.time.temporal/TemporalAdjusters

;; java.time.temporal/TemporalAmount

#_(defn adjust [t adjuster]
  (.with t adjuster))

;; adjust

;; Conversions

;; Ago/hence

(defn ago [dur]
  (backward-duration (now) dur))

(defn hence [dur]
  (forward-duration (now) dur))

(defn midnight? [^LocalDateTime t]
  (.isZero (. Duration between t (beginning (date t)))))
