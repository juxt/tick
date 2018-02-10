;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [+ - / inc dec max min range time int long < <= > >=])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str])
  (:import
   [java.util Date]
   [java.time Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime]
   [java.time.format DateTimeFormatter]
   [java.time.temporal ChronoUnit]))

(def units
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

(def ^{:dynamic true} *clock* nil)

(defn now []
  (if *clock*
    (Instant/now *clock*)
    (Instant/now)))

(defn just-now []
  (.truncatedTo (now) (ChronoUnit/SECONDS)))

(defn today []
  (if *clock*
    (LocalDate/now *clock*)
    (LocalDate/now)))

(defprotocol ITimeArithmetic
  (+ [_ _] "Add time")
  (- [_ _] "Subtract time")
  (inc [_] "Increment time")
  (dec [_] "Decrement time")
  (maximum [_ _] "Return maximum")
  (minimum [_ _] "Return minimum"))

(defn tomorrow []
  (+ (today) 1))

(defn yesterday []
  (- (today) 1))

(defprotocol ITimeAt
  (on [_ _] "Set time be ON a date")
  (at [_ _] "Set date to be AT a time")  )

(defn midnight [^LocalDate date]
  (at date (LocalTime/MIDNIGHT)))

(defn noon [^LocalDate date]
  (at date (LocalTime/NOON)))

(defn epoch []
  (java.time.Instant/EPOCH))

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
  (zoned-date-time [f] (zone (f)))
  (local-date-time [f] (local-date-time (f)))

  Instant
  (inst [i] (Date/from i))
  (instant [i] i)
  (date [i] (date (zoned-date-time i)))
  (day [i] (day (date i)))
  (month [i] (month (date i)))
  (year [i] (year (date i)))
  (year-month [i] (year-month (date i)))
  (zoned-date-time [i] (.atZone i ZoneOffset/UTC))
  (int [i] (.getNano i))
  (long [i] (.getEpochSecond i))

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
  (int [s] (.getNano (instant s)))
  (long [s] (.getEpochSecond (instant s)))
  (local-date-time [s] (local-date-time (parse s)))

  Number
  (day [n] (DayOfWeek/of n))
  (month [n] (Month/of n))
  (instant [n] (Instant/ofEpochMilli n))
  (year [n] (Year/of n))

  LocalDate
  (time [d] (midnight d)) ; this helps iCalendar dates become intervals
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
  (inst [zdt] (inst (instant zdt)))
  (instant [zdt] (.toInstant zdt))
  (date [zdt] (.toLocalDate zdt))
  (zone [zdt] (.getZone zdt)))

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
  (let [unit (units u)]
    (assert unit (str "Not a unit: " u))
    (Duration/of n unit)))

(defn period [n u]
  (case u
    :days (Period/ofDays n)
    :weeks (Period/ofWeeks n)
    :months (Period/ofMonths n)
    :years (Period/ofYears n)))

(defprotocol ITimeBetween
  (between [t1 t2] "Return the most appropriate type of value that
  represents the gap between two times"))

(extend-protocol ITimeBetween
  Instant
  (between [inst _] (throw (ex-info "TODO" {}))))

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
  LocalTime
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
  (>= [x y] (not (.isBefore x y))))

(defn max [arg & args]
  (reduce #(maximum %1 %2) arg args))

(defn min [arg & args]
  (reduce #(minimum %1 %2) arg args))

(defprotocol ITimeRangeable
  (range [_] [_ _] [_ _ _] "Returns a lazy seq of times from start (inclusive) to end (exclusive, nil means forever), by step, where start defaults to 0, step to 1, and end to infinity."))

(defprotocol IDivisible
  (/ [_ _] "Divide time"))

(extend-protocol IDivisible
  String
  (/ [s d] (/ (parse s) d)))

(extend-type Instant
  ITimeArithmetic
  (+ [t x] (.plus t x))
  (- [t x] (.minus t x))
  (inc [t] (+ t (seconds 1)))
  (dec [t] (- t (seconds 1)))
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  ITimeRangeable
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
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  ITimeRangeable
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
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  ITimeRangeable
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
  ;; TODO: Rename maximum to 'greater' and minimum to 'lesser'
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  ITimeRangeable
  (range
    ([from] (iterate #(.plusSeconds % 1) from))
    ([from to] (cond->> (iterate #(.plusSeconds % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type LocalTime
  ITimeArithmetic
  (+ [t x] (.plus t x))
  (- [t x] (.minus t x))
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  )

(extend-type YearMonth
  ITimeArithmetic
  (+ [t x] (if (number? x) (.plusMonths t x) (.plus t x)))
  (- [t x] (if (number? x) (.minusMonths t x) (.minus t x)))
  (inc [t] (.plusMonths t 1))
  (dec [t] (.minusMonths t 1))
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  ITimeRangeable
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
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  ITimeRangeable
  (range
    ([from] (iterate #(.plusYears % 1) from))
    ([from to] (cond->> (iterate #(.plusYears % 1) from)
                 to (take-while #(< % to) )))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(defprotocol IDivisbleDuration
  (divide-duration [divisor duration] "Divide a duration"))

(extend-protocol IDivisbleDuration
  Long
  (divide-duration [n duration] (.dividedBy duration n))
  Duration
  (divide-duration [divisor duration]
    (clojure.core// (.getSeconds duration) (.getSeconds divisor))))

(extend-type Duration
  ITimeArithmetic
  (+ [d x] (.plus d x))
  (- [d x] (.minus d x))
  (inc [d] (.plusSeconds d 1))
  (dec [d] (.minusSeconds d 1))
  (maximum [x y] (if (neg? (compare x y)) y x))
  (minimum [x y] (if (neg? (compare x y)) x y))
  IDivisible
  (/ [d x] (divide-duration x d)))

(defprotocol ITimeSpan
  (beginning [_] "Return the beginning of a span of time")
  (end [_] "Return the end of a span of time"))

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

(extend-protocol ITimeAt
  LocalTime
  (on [t date] (.atTime date t))
  OffsetTime
  (on [t date] (.atTime date t))
  LocalDate
  (at [date t] (.atTime date (time t))))

;; Not sure about at-zone - perhaps in-zone, as-global, etc.

(defprotocol IAtZone
  (at-zone [t zone] "Put time at zone")
  (as-local [t] [t zone] "Convert to local time at zone."))

(extend-protocol IAtZone
  LocalDateTime
  (at-zone [t z] (.atZone t (zone z)))
  (as-local
    ([t] t)
    ([t z] (as-local (at-zone t (zone z)))))
  Instant
  (at-zone [t z] (.atZone t (zone z)))
  (as-local
    ([t] (throw (ex-info "Error, zone required" {})))
    ([t z] (as-local (at-zone t (zone z)))))
  ZonedDateTime
  (at-zone [t z] (.withZoneSameInstant t (zone z)))
  (as-local
    ([t] (.toLocalDateTime t))
    ([t z] (as-local (at-zone t (zone z)))))
  Date
  (at-zone [t z] (at-zone (instant t) (zone z)))
  (as-local
    ([t] (throw (ex-info "Error, zone required" {})))
    ([t z] (as-local (at-zone t (zone z))))))

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
  ZonedDateTime
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
