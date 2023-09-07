;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [min-key max-key format + - inc dec max min range time int long = < <= > >= next >> << atom swap! swap-vals! compare-and-set! reset! reset-vals! second divide])
  (:require
    [clojure.string :as str]
    [tick.protocols :as p]
    #?@(:clj [[tick.file] ; for protocol extn
              [time-literals.data-readers] ; must be required for literals to work on jvm
              ])
    [time-literals.read-write]
    [cljc.java-time.local-date]
    [cljc.java-time.local-date-time]
    [cljc.java-time.local-time]
    [cljc.java-time.clock]
    [cljc.java-time.instant]
    [cljc.java-time.zone-id]
    [cljc.java-time.zone-offset]
    [cljc.java-time.zoned-date-time]
    [cljc.java-time.offset-date-time]
    [cljc.java-time.offset-time]
    [cljc.java-time.year-month]
    [cljc.java-time.month]
    [cljc.java-time.year]
    [cljc.java-time.day-of-week]
    [cljc.java-time.period]
    [cljc.java-time.duration]
    [cljc.java-time.extn.predicates]
    [cljc.java-time.temporal.temporal-amount]
    [cljc.java-time.temporal.temporal]
    [cljc.java-time.temporal.temporal-adjusters]
    [cljc.java-time.temporal.chrono-field]
    [cljc.java-time.temporal.chrono-unit]
    [cljc.java-time.format.date-time-formatter]
    #?@(:cljs
        [[goog.object]
         [java.time.format :refer [DateTimeFormatter]]
         [java.time :refer [Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime
                            LocalDateTime LocalDate Year YearMonth OffsetDateTime OffsetTime]]
         [cljs.java-time.extend-eq-and-compare]]))
  #?(:cljs
     (:require-macros [tick.core :refer [with-clock modify-printing-of-time-literals-if-enabled!]])
     :clj
     (:import
       [java.util Date]
       [java.time Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime]
       [java.time.format DateTimeFormatter]
       [java.time.temporal Temporal]
       [clojure.lang ILookup Seqable]
       [java.util Locale])))

#?(:clj
   (defonce
     ^{:dynamic true
       :doc     "If true, include the time-literals printer, which will affect the way java.time and js-joda objects are printed"}
     *time-literals-printing*
     (not= "false" (System/getProperty "tick.time-literals.printing"))))

#?(:clj
   (defmacro modify-printing-of-time-literals-if-enabled! []
     (when *time-literals-printing*
       '(do
          (time-literals.read-write/print-time-literals-clj!)
          (time-literals.read-write/print-time-literals-cljs!)))))

(modify-printing-of-time-literals-if-enabled!)

(defn- parse-int [x]
  #?(:clj (Integer/parseInt x)
     :cljs (js/Number x)))

(extend-protocol p/IParseable
  #?(:clj String :cljs string)
  (parse [s]
    (condp re-matches s
      #"(\d{1,2})\s*(am|pm)"
      :>> (fn [[_ h ap]] (cljc.java-time.local-time/of (cond-> (parse-int h) (clojure.core/= "pm" ap) (clojure.core/+ 12)) 0))
      #"(\d{1,2})"
      :>> (fn [[_ h]] (cljc.java-time.local-time/of (parse-int h) 0))
      #"\d{2}:\d{2}\S*"
      :>> (fn [s] (cljc.java-time.local-time/parse s))
      #"(\d{1,2}):(\d{2})"
      :>> (fn [[_ h m]] (cljc.java-time.local-time/of (parse-int h) (parse-int m)))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z"
      :>> (fn [s] (cljc.java-time.instant/parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}"
      :>> (fn [s] (cljc.java-time.offset-date-time/parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?(?:[+-]\d{2}:\d{2}|Z)\[\w+/\w+\]"
      :>> (fn [s] (cljc.java-time.zoned-date-time/parse s))
      #"\d{4}-\d{2}-\d{2}T\S*"
      :>> (fn [s] (cljc.java-time.local-date-time/parse s))
      #"\d{4}-\d{2}-\d{2}"
      :>> (fn [s] (cljc.java-time.local-date/parse s))
      #"\d{4}-\d{2}"
      :>> (fn [s] (cljc.java-time.year-month/parse s))
      #"\d{4}"
      :>> (fn [s] (cljc.java-time.year/parse s))
      (throw (ex-info "Unparseable time string" {:input s})))))


(def ^{:dynamic true} *clock* (cljc.java-time.clock/system-default-zone))

(defn now "same as (t/instant)" []
  (cljc.java-time.instant/now *clock*))

(defn today "same as (t/date)" []
  (cljc.java-time.local-date/now *clock*))

(defn epoch "Constant for the 1970-01-01T00:00:00Z epoch instant" []
  cljc.java-time.instant/epoch)


(defn midnight
  ([] cljc.java-time.local-time/midnight)
  ([^LocalDate date]
   (p/at date cljc.java-time.local-time/midnight)))

(defn noon
  ([] cljc.java-time.local-time/noon)
  ([^LocalDate date]
   (p/at date cljc.java-time.local-time/noon)))

(defn parse-day
  "en locale specific and borderline deprecated.
  consider writing your own regex or use a formatter. For example:

  (-> (t/formatter \"EEE\")
      (cljc.java-time.format.date-time-formatter/parse \"Tue\")
      (cljc.java-time.day-of-week/from))
  "
  [input]
  (condp re-matches (str/lower-case input)
    #"^(mon)(day)?$" cljc.java-time.day-of-week/monday
    #"^(tue)(s|sday)?$" cljc.java-time.day-of-week/tuesday
    #"^(wed)(s|nesday)?$" cljc.java-time.day-of-week/wednesday
    #"^(thur)(s|sday)?$" cljc.java-time.day-of-week/thursday
    #"^(fri)(day)?$" cljc.java-time.day-of-week/friday
    #"^(sat)(urday)?$" cljc.java-time.day-of-week/saturday
    #"^(sun)(day)?$" cljc.java-time.day-of-week/sunday
    nil))

(defn parse-month
  "en locale specific and borderline deprecated. Consider writing your
   own regex or use a formatter. For example:

   (-> (t/formatter \"MMM\")
       (cljc.java-time.format.date-time-formatter/parse \"Jan\")
       (cljc.java-time.month/from))
   "
  [input]
  (condp re-matches (str/lower-case input)
    #"^(jan)(uary)?$" cljc.java-time.month/january
    #"^(feb)(ruary)?$" cljc.java-time.month/february
    #"^(mar)(ch)?$" cljc.java-time.month/march
    #"^(apr)(il)?$" cljc.java-time.month/april
    #"^may$" cljc.java-time.month/may
    #"^(jun)(e)?$" cljc.java-time.month/june
    #"^(jul)(y)?$" cljc.java-time.month/july
    #"^(aug)(ust)?$" cljc.java-time.month/august
    #"^(sep)(tember)?$" cljc.java-time.month/september
    #"^(oct)(ober)?$" cljc.java-time.month/october
    #"^(nov)(ember)?$" cljc.java-time.month/november
    #"^(dec)(ember)?$" cljc.java-time.month/december
    nil))

(defn new-time
  ([] (p/time (now)))
  ([hour minute] (cljc.java-time.local-time/of hour minute))
  ([hour minute second] (cljc.java-time.local-time/of hour minute second))
  ([hour minute second nano] (cljc.java-time.local-time/of hour minute second nano)))

(defn new-date
  ([] (today))
  ([year month day-of-month]
   (cljc.java-time.local-date/of year month day-of-month))
  ([year day-of-year]
   (cljc.java-time.local-date/of-year-day year day-of-year))
  ([epoch-day]
   (cljc.java-time.local-date/of-epoch-day epoch-day)))


(defn new-year-month
  ([] (cljc.java-time.year-month/now))
  ([year month]
   (cljc.java-time.year-month/of year month)))

(defn current-zone
  "Return the current zone, which can be overridden by the *clock* dynamic var"
  []
  (if-let [clk *clock*]
    (cljc.java-time.clock/get-zone clk)
    (cljc.java-time.zone-id/system-default)))

(defn zone
  ([] (current-zone))
  ([z] (p/zone z)))

(defn zone-offset
  ([offset] (p/zone-offset offset))
  ([hours minutes] (cljc.java-time.zone-offset/of-hours-minutes hours minutes))
  ([hours minutes seconds] (cljc.java-time.zone-offset/of-hours-minutes-seconds hours minutes seconds)))

(extend-protocol p/IConversion
  #?(:clj clojure.lang.Fn :cljs function)
  (inst [f] (p/inst (f)))
  (instant [f] (p/instant (f)))
  (offset-date-time [f] (p/offset-date-time (f)))
  (zoned-date-time [f] (p/zoned-date-time (f)))

  Instant
  (inst [i] #?(:clj (Date/from i) :cljs (js/Date. (cljc.java-time.instant/to-epoch-milli i))))
  (instant [i] i)
  (offset-date-time [i] (cljc.java-time.offset-date-time/of-instant i (current-zone)))
  (zoned-date-time [i] (cljc.java-time.zoned-date-time/of-instant i (current-zone)))

  #?(:clj String :cljs string)
  (inst [s] (p/inst (p/instant s)))
  (instant [s] (cljc.java-time.instant/parse s))
  (offset-date-time [s] (cljc.java-time.offset-date-time/parse s))
  (zoned-date-time [s] (cljc.java-time.zoned-date-time/parse s))

  #?(:clj Number :cljs number)
  (instant [n] (cljc.java-time.instant/of-epoch-milli n))

  LocalDateTime
  (inst [ldt] (p/inst (p/zoned-date-time ldt)))
  (instant [ldt] (p/instant (p/zoned-date-time ldt)))
  (offset-date-time [ldt] (cljc.java-time.local-date-time/at-offset
                            ldt
                            (#?(:clj .getOffset :cljs .offset)
                              (-> (current-zone)
                                  (cljc.java-time.zone-id/get-rules))
                              ldt)))
  (zoned-date-time [ldt] (cljc.java-time.local-date-time/at-zone ldt (current-zone)))

  #?(:clj Date :cljs js/Date)
  (inst [d] d)
  (instant [d] #?(:clj (.toInstant ^Date d) :cljs (cljc.java-time.instant/of-epoch-milli (.getTime d))))
  (zoned-date-time [d] (p/zoned-date-time (p/instant d)))
  (offset-date-time [d] (p/offset-date-time (p/instant d)))

  OffsetDateTime
  (inst [odt] (p/inst (p/instant odt)))
  (instant [odt] (cljc.java-time.offset-date-time/to-instant odt))
  (offset-date-time [odt] odt)
  (zoned-date-time [odt] (cljc.java-time.offset-date-time/to-zoned-date-time odt))

  ZonedDateTime
  (inst [zdt] (p/inst (p/instant zdt)))
  (instant [zdt] (cljc.java-time.zoned-date-time/to-instant zdt))
  (offset-date-time [zdt] (cljc.java-time.zoned-date-time/to-offset-date-time zdt))
  (zoned-date-time [zdt] zdt))

(extend-protocol p/IExtraction
  #?(:clj Object :cljs object)
  (int [v] (#?(:clj clojure.core/int :cljs parse-int) v))
  (long [v] (#?(:clj clojure.core/long :cljs parse-int) v))

  #?(:clj clojure.lang.Fn :cljs function)
  (time [f] (p/time (f)))
  (date [f] (p/date (f)))
  (date-time [f] (p/date-time (f)))
  (nanosecond [f] (p/nanosecond (f)))
  (microsecond [f] (p/microsecond (f)))
  (millisecond [f] (p/millisecond (f)))
  (second [f] (p/second (f)))
  (minute [f] (p/minute (f)))
  (hour [f] (p/hour (f)))
  (day-of-week [f] (p/day-of-week (f)))
  (day-of-month [f] (p/day-of-month (f)))
  (int [f] (p/int (f)))
  (long [f] (p/long (f)))
  (month [f] (p/month (f)))
  (year [f] (p/year (f)))
  (year-month [f] (p/year-month (f)))
  (zone [f] (p/zone (f)))
  (zone-offset [f] (p/zone-offset (f)))

  Instant
  (time [i] (p/time (p/zoned-date-time i)))
  (date [i] (p/date (p/zoned-date-time i)))
  (date-time [i] (p/date-time (p/zoned-date-time i)))
  (nanosecond [t] (p/nanosecond (p/zoned-date-time t)))
  (microsecond [t] (p/microsecond (p/zoned-date-time t)))
  (millisecond [t] (p/millisecond (p/zoned-date-time t)))
  (second [t] (p/second (p/zoned-date-time t)))
  (minute [t] (p/minute (p/zoned-date-time t)))
  (hour [t] (p/hour (p/zoned-date-time t)))
  (day-of-week [i] (p/day-of-week (p/date i)))
  (day-of-month [i] (p/day-of-month (p/date i)))
  (int [i] (cljc.java-time.instant/get-nano i))
  (long [i] (cljc.java-time.instant/get-epoch-second i))
  (month [i] (p/month (p/date i)))
  (year [i] (p/year (p/date i)))
  (year-month [i] (p/year-month (p/date i)))
  (zone [_i] (cljc.java-time.zone-id/of "UTC"))
  (zone-offset [_i] cljc.java-time.zone-offset/utc)

  #?(:clj String :cljs string)
  (time [s] (cljc.java-time.local-time/parse s))
  (date [s] (cljc.java-time.local-date/parse s))
  (date-time [s] (cljc.java-time.local-date-time/parse s))
  (day-of-week [s] (or (parse-day s) (p/day-of-week (p/date s))))
  (day-of-month [s] (p/day-of-month (p/date s)))
  (month [s] (or (parse-month s) (p/month (p/date s))))
  (year [s] (cljc.java-time.year/parse s))
  (year-month [s] (cljc.java-time.year-month/parse s))
  (zone [s] (cljc.java-time.zone-id/of s))
  (zone-offset [s] (cljc.java-time.zone-offset/of s))
  (int [s] (cljc.java-time.instant/get-nano (p/instant s)))
  (long [s] (cljc.java-time.instant/get-epoch-second (p/instant s)))

  #?(:clj Number :cljs number)
  (day-of-week [n] (cljc.java-time.day-of-week/of n))
  (month [n] (cljc.java-time.month/of n))
  (year [n] (cljc.java-time.year/of n))
  (zone-offset [s] (cljc.java-time.zone-offset/of-hours s))

  LocalDate
  (date [d] d)
  (day-of-week [d] (cljc.java-time.local-date/get-day-of-week d))
  (day-of-month [d] (cljc.java-time.local-date/get-day-of-month d))
  (month [d] (cljc.java-time.month/from d))
  (year-month [d] (cljc.java-time.year-month/of
                    (cljc.java-time.local-date/get-year d)
                    (cljc.java-time.local-date/get-month-value d)))
  (year [d] (cljc.java-time.year/of (cljc.java-time.local-date/get-year d)))

  LocalTime
  (time [t] t)
  (nanosecond [t] (cljc.java-time.local-time/get t cljc.java-time.temporal.chrono-field/nano-of-second))
  (microsecond [t] (cljc.java-time.local-time/get t cljc.java-time.temporal.chrono-field/micro-of-second))
  (millisecond [t] (cljc.java-time.local-time/get t cljc.java-time.temporal.chrono-field/milli-of-second))
  (second [t] (cljc.java-time.local-time/get-second t))
  (minute [t] (cljc.java-time.local-time/get-minute t))
  (hour [t] (cljc.java-time.local-time/get-hour t))

  Month
  (int [m] (cljc.java-time.month/get-value m)) ;todo

  DayOfWeek
  (int [d] (cljc.java-time.day-of-week/get-value d))
  (day-of-week [d] d)

  LocalDateTime
  (time [dt] (cljc.java-time.local-date-time/to-local-time dt))
  (date [dt] (cljc.java-time.local-date-time/to-local-date dt))
  (date-time [ldt] ldt)
  (second [t] (cljc.java-time.local-date-time/get-second t))
  (minute [t] (cljc.java-time.local-date-time/get-minute t))
  (hour [t] (cljc.java-time.local-date-time/get-hour t))
  (day-of-week [dt] (p/day-of-week (p/date dt)))
  (day-of-month [dt] (p/day-of-month (p/date dt)))
  (year-month [dt] (p/year-month (p/date dt)))
  (month [dt] (cljc.java-time.local-date-time/get-month dt))
  (year [dt] (p/year (p/date dt)))

  #?(:clj Date :cljs js/Date)
  (date [d] (p/date (p/zoned-date-time (p/instant d)))) ; implicit conversion to UTC
  (date-time [d] (p/date-time (p/instant d)))
  (year-month [d] (p/year-month (p/date d)))
  (year [d] (p/year (p/date d)))

  YearMonth
  (year-month [ym] ym)
  (month [ym] (cljc.java-time.year-month/get-month ym))
  (year [ym] (p/year (cljc.java-time.year-month/get-year ym)))

  Year
  (year [y] y)
  (int [y] (cljc.java-time.year/get-value y))

  ZoneId
  (zone [z] z)

  ZoneOffset
  (zone-offset [z] z)
  (zone [z] z)

  OffsetDateTime
  (time [odt] (cljc.java-time.offset-date-time/to-local-time odt))
  (date [odt] (cljc.java-time.offset-date-time/to-local-date odt))
  (date-time [odt] (cljc.java-time.offset-date-time/to-local-date-time odt))
  (nanosecond [t] (cljc.java-time.offset-date-time/get t cljc.java-time.temporal.chrono-field/nano-of-second))
  (microsecond [t] (cljc.java-time.offset-date-time/get t cljc.java-time.temporal.chrono-field/micro-of-second))
  (millisecond [t] (cljc.java-time.offset-date-time/get t cljc.java-time.temporal.chrono-field/milli-of-second))
  (second [t] (cljc.java-time.offset-date-time/get-second t))
  (minute [t] (cljc.java-time.offset-date-time/get-minute t))
  (hour [t] (cljc.java-time.offset-date-time/get-hour t))
  (day-of-week [t] (cljc.java-time.offset-date-time/get-day-of-week t))
  (day-of-month [t] (cljc.java-time.offset-date-time/get-day-of-month t))
  (month [zdt] (cljc.java-time.offset-date-time/get-month zdt))
  (year [odt] (p/year (cljc.java-time.offset-date-time/get-year odt)))
  (zone-offset [odt] (cljc.java-time.offset-date-time/get-offset odt))

  ZonedDateTime
  (time [zdt] (cljc.java-time.zoned-date-time/to-local-time zdt))
  (date [zdt] (cljc.java-time.zoned-date-time/to-local-date zdt))
  (date-time [zdt] (cljc.java-time.zoned-date-time/to-local-date-time zdt))
  (nanosecond [t] (cljc.java-time.zoned-date-time/get t cljc.java-time.temporal.chrono-field/nano-of-second))
  (microsecond [t] (cljc.java-time.zoned-date-time/get t cljc.java-time.temporal.chrono-field/micro-of-second))
  (millisecond [t] (cljc.java-time.zoned-date-time/get t cljc.java-time.temporal.chrono-field/milli-of-second))
  (second [t] (cljc.java-time.zoned-date-time/get-second t))
  (minute [t] (cljc.java-time.zoned-date-time/get-minute t))
  (hour [t] (cljc.java-time.zoned-date-time/get-hour t))
  (day-of-week [t] (cljc.java-time.zoned-date-time/get-day-of-week t))
  (day-of-month [t] (cljc.java-time.zoned-date-time/get-day-of-month t))
  (month [zdt] (cljc.java-time.zoned-date-time/get-month zdt))
  (year [zdt] (p/year (cljc.java-time.zoned-date-time/get-year zdt)))
  (zone [zdt] (cljc.java-time.zoned-date-time/get-zone zdt))
  (zone-offset [zdt] (cljc.java-time.zoned-date-time/get-offset zdt)))

;; Fields

(def 
  ^{:doc "keyword to chrono-field"}
  field-map
  {:aligned-day-of-week-in-month cljc.java-time.temporal.chrono-field/aligned-day-of-week-in-month
   :aligned-day-of-week-in-year  cljc.java-time.temporal.chrono-field/aligned-day-of-week-in-year
   :aligned-week-of-month        cljc.java-time.temporal.chrono-field/aligned-week-of-month
   :aligned-week-of-year         cljc.java-time.temporal.chrono-field/aligned-week-of-year
   :ampm-of-day                  cljc.java-time.temporal.chrono-field/ampm-of-day
   :clock-hour-of-ampm           cljc.java-time.temporal.chrono-field/clock-hour-of-ampm
   :clock-hour-of-day            cljc.java-time.temporal.chrono-field/clock-hour-of-day
   :day-of-month                 cljc.java-time.temporal.chrono-field/day-of-month
   :day-of-week                  cljc.java-time.temporal.chrono-field/day-of-week
   :day-of-year                  cljc.java-time.temporal.chrono-field/day-of-year
   :epoch-day                    cljc.java-time.temporal.chrono-field/epoch-day
   :era                          cljc.java-time.temporal.chrono-field/era
   :hour-of-ampm                 cljc.java-time.temporal.chrono-field/hour-of-ampm
   :hour-of-day                  cljc.java-time.temporal.chrono-field/hour-of-day
   :instant-seconds              cljc.java-time.temporal.chrono-field/instant-seconds
   :micro-of-day                 cljc.java-time.temporal.chrono-field/micro-of-day
   :micro-of-second              cljc.java-time.temporal.chrono-field/micro-of-second
   :milli-of-day                 cljc.java-time.temporal.chrono-field/milli-of-day
   :milli-of-second              cljc.java-time.temporal.chrono-field/milli-of-second
   :minute-of-day                cljc.java-time.temporal.chrono-field/minute-of-day
   :minute-of-hour               cljc.java-time.temporal.chrono-field/minute-of-hour
   :month-of-year                cljc.java-time.temporal.chrono-field/month-of-year
   :nano-of-day                  cljc.java-time.temporal.chrono-field/nano-of-day
   :nano-of-second               cljc.java-time.temporal.chrono-field/nano-of-second
   :offset-seconds               cljc.java-time.temporal.chrono-field/offset-seconds
   :proleptic-month              cljc.java-time.temporal.chrono-field/proleptic-month
   :second-of-day                cljc.java-time.temporal.chrono-field/second-of-day
   :second-of-minute             cljc.java-time.temporal.chrono-field/second-of-minute
   :year                         cljc.java-time.temporal.chrono-field/year
   :year-of-era                  cljc.java-time.temporal.chrono-field/year-of-era})

(defn- fields-map [t]
  (->> field-map
    (keep (fn [[k _v]]
            (let [cf (get field-map k)]
              (when (cljc.java-time.temporal.temporal/is-supported t cf)
                [k (cljc.java-time.temporal.temporal/get-long t cf)]))))
    (into {})))

#?(:bb nil
   :clj
   (deftype FieldsLookup [t]
     Seqable
     (seq [_]
       (seq (fields-map t)))
     ILookup
     (valAt [_ fld]
       (when-let [f (get field-map fld)]
         (cljc.java-time.temporal.temporal/get-long t f)))
     (valAt [_ fld notfound]
       (if-let [f (get field-map fld)]
         (try
           (cljc.java-time.temporal.temporal/get-long t f)
           (catch java.time.temporal.UnsupportedTemporalTypeException _e
             notfound))
         notfound)))
   :cljs
   (deftype FieldsLookup [t]
     ISeqable
     (-seq [_]
       (seq (fields-map t)))
     ILookup
     (-lookup [_ fld]
       (when-let [f (get field-map fld)]
         (cljc.java-time.temporal.temporal/get-long t f)))
     (-lookup [_ fld notfound]
       (if-let [f (get field-map fld)]
         (try
           (cljc.java-time.temporal.temporal/get-long t f)
           (catch js/Error _e
             notfound))
         notfound))))

#?(:bb   (defn fields [t] (fields-map t))
   :clj  (defn fields [t] (->FieldsLookup t))
   :cljs (defn fields [t] (->FieldsLookup t)))

;; With

(defn with
  "Adjust a temporal with an adjuster or field"
  ([t adj]
   (cljc.java-time.temporal.temporal/with t adj))
  ([t fld new-value]
   (when-let [f (get field-map fld)]
     (cljc.java-time.temporal.temporal/with t f new-value))))

;; Built-in adjusters

(defn day-of-week-in-month
  ([ordinal day-of-week] (cljc.java-time.temporal.temporal-adjusters/day-of-week-in-month ordinal (p/day-of-week day-of-week)))
  ([t ordinal day-of-week] (with t (day-of-week-in-month ordinal day-of-week))))

(defn first-day-of-month
  ([] (cljc.java-time.temporal.temporal-adjusters/first-day-of-month))
  ([t] (with t (first-day-of-month))))

(defn first-day-of-next-month
  ([] (cljc.java-time.temporal.temporal-adjusters/first-day-of-next-month))
  ([t] (with t (first-day-of-next-month))))

(defn first-day-of-next-year
  ([] (cljc.java-time.temporal.temporal-adjusters/first-day-of-next-year))
  ([t] (with t (first-day-of-next-year))))

(defn first-day-of-year
  ([] (cljc.java-time.temporal.temporal-adjusters/first-day-of-year))
  ([t] (with t (first-day-of-year))))

(defn first-in-month
  ([day-of-week] (cljc.java-time.temporal.temporal-adjusters/first-in-month (p/day-of-week day-of-week)))
  ([t day-of-week] (with t (first-in-month day-of-week))))

(defn last-day-of-month
  ([] (cljc.java-time.temporal.temporal-adjusters/last-day-of-month))
  ([t] (with t (last-day-of-month))))

(defn last-day-of-year
  ([] (cljc.java-time.temporal.temporal-adjusters/last-day-of-year))
  ([t] (with t (last-day-of-year))))

(defn last-in-month
  ([day-of-week] (cljc.java-time.temporal.temporal-adjusters/last-in-month (p/day-of-week day-of-week)))
  ([t day-of-week] (with t (last-in-month day-of-week))))

(defn next
  ([day-of-week] (cljc.java-time.temporal.temporal-adjusters/next (p/day-of-week day-of-week)))
  ([t day-of-week] (with t (next day-of-week))))

(defn next-or-same
  ([day-of-week] (cljc.java-time.temporal.temporal-adjusters/next-or-same (p/day-of-week day-of-week)))
  ([t day-of-week] (with t (next-or-same day-of-week))))

(defn previous
  ([day-of-week] (cljc.java-time.temporal.temporal-adjusters/previous (p/day-of-week day-of-week)))
  ([t day-of-week] (with t (previous day-of-week))))

(defn previous-or-same
  ([day-of-week] (cljc.java-time.temporal.temporal-adjusters/previous-or-same (p/day-of-week day-of-week)))
  ([t day-of-week] (with t (previous-or-same day-of-week))))

;; Units

(def ^{:doc "keyword to chrono-unit"} 
  unit-map
  {:nanos     cljc.java-time.temporal.chrono-unit/nanos
   :micros    cljc.java-time.temporal.chrono-unit/micros
   :millis    cljc.java-time.temporal.chrono-unit/millis
   :seconds   cljc.java-time.temporal.chrono-unit/seconds
   :minutes   cljc.java-time.temporal.chrono-unit/minutes
   :hours     cljc.java-time.temporal.chrono-unit/hours
   :half-days cljc.java-time.temporal.chrono-unit/half-days
   :days      cljc.java-time.temporal.chrono-unit/days
   :weeks     cljc.java-time.temporal.chrono-unit/weeks
   :months    cljc.java-time.temporal.chrono-unit/months
   :years     cljc.java-time.temporal.chrono-unit/years
   :decades   cljc.java-time.temporal.chrono-unit/decades
   :centuries cljc.java-time.temporal.chrono-unit/centuries
   :millennia cljc.java-time.temporal.chrono-unit/millennia
   :eras      cljc.java-time.temporal.chrono-unit/eras
   :forever   cljc.java-time.temporal.chrono-unit/forever  })

(def reverse-unit-map ^{:doc "chrono-unit to keyword"}
  (into {} (map vec (map reverse unit-map))))

(defn units 
  "the units contained within TemporalAmount x.
  
  Seconds and nanos for Duration.
  Years, months, days for Period
  "
  [x]
  (into {}
    (for [tu (cljc.java-time.temporal.temporal-amount/get-units x)
          :let [k (reverse-unit-map tu)]
          :when k]
      [k (cljc.java-time.temporal.temporal-amount/get x tu)])))

(extend-protocol p/ITruncate
  Instant
  (truncate [x u ]
    (cljc.java-time.instant/truncated-to x (get unit-map u)))
  LocalDateTime
  (truncate [x u ]
    (cljc.java-time.local-date-time/truncated-to x (get unit-map u)))
  ZonedDateTime
  (truncate [x u ]
    (cljc.java-time.zoned-date-time/truncated-to x (get unit-map u)))
  OffsetDateTime
  (truncate [x u ]
    (cljc.java-time.offset-date-time/truncated-to x (get unit-map u)))
  LocalTime
  (truncate [x u ]
    (cljc.java-time.local-time/truncated-to x (get unit-map u))))

(defn truncate 
  "Returns a copy of x truncated to the specified unit."
  [x u]
  {:pre [(contains? unit-map u)]}
  (p/truncate x u))

;; Durations & Periods

(extend-protocol p/IConversion
  ;; Durations between the epoch and a time. These are useful
  ;; conversion functions in the case where numerics are used.
  Duration
  (instant [d] (cljc.java-time.instant/of-epoch-milli (p/millis d)))
  (inst [d] (p/inst (p/instant d))))

(extend-protocol p/ITimeLength
  Duration
  (nanos [d] (cljc.java-time.duration/to-nanos d))
  (micros [d] (#?(:clj Long/divideUnsigned :cljs cljs.core//) (p/nanos d) 1000))
  (millis [d] (cljc.java-time.duration/to-millis d))
  (seconds [d] (cljc.java-time.duration/get-seconds d))
  (minutes [d] (cljc.java-time.duration/to-minutes d))
  (hours [d] (cljc.java-time.duration/to-hours d))
  (days [d] (cljc.java-time.duration/to-days d))

  Period
  (days [p] (cljc.java-time.period/get-days p))
  (months [p] (cljc.java-time.period/get-months p))
  (years [p] (cljc.java-time.period/get-years p)))

(defn new-duration [n u]
  (let [unit (unit-map u)]
    (assert unit (str "Not a unit: " u))
    (cljc.java-time.duration/of n unit)))

(defn new-period [n u]
  (case u
    :days (cljc.java-time.period/of-days n)
    :weeks (cljc.java-time.period/of-weeks n)
    :months (cljc.java-time.period/of-months n)
    :years (cljc.java-time.period/of-years n)))

;; Durations. Convenience functions to create durations of specific
;; units.

(defn of-nanos
  "Takes a java.lang.Long n and returns a duration of n nanoseconds."
  [n]
  (new-duration n :nanos))

(defn of-micros
  "Takes a java.lang.Long n and returns a duration of n micros."
  [n]
  (new-duration n :micros))

(defn of-millis
  "Takes a java.lang.Long n and returns a duration of n micros."
  [n]
  (new-duration n :millis))

(defn of-seconds
  "Takes a java.lang.Long n and returns a duration of n seconds."
  [n]
  (new-duration n :seconds))

(defn of-minutes
  "Takes a java.lang.Long n and returns a duration of n minutes."
  [n]
  (new-duration n :minutes))

(defn of-hours
  "Takes a java.lang.Long n and returns a duration of n hours."
  [n]
  (new-duration n :hours))


;; Periods. Convenience functions to create durations of specific
;; units.

(defn of-days
  "Takes a java.lang.Long n and returns a period of n days."
  [n]
  (new-period n :days))

(defn of-months
  "Takes a java.lang.Long n and returns a period of n months."
  [n]
  (new-period n :months))

(defn of-years
  "Takes a java.lang.Long n and returns a period of n years."
  [n]
  (new-period n :years))

;; Coercions

(extend-protocol p/IExtraction
  Duration
  (zone-offset [_d] (cljc.java-time.zone-offset/of-total-seconds (new-duration 1 :seconds))))

;; Clocks

(defn current-clock []
  *clock*)

(extend-protocol p/IClock
  Instant
  (clock [i] (cljc.java-time.clock/fixed i (current-zone)))

  ZonedDateTime
  (clock [zdt] (cljc.java-time.clock/fixed (cljc.java-time.zoned-date-time/to-instant zdt)
                 (cljc.java-time.zoned-date-time/get-zone zdt)))

  LocalDateTime
  (clock [o] (p/clock (p/zoned-date-time o)))

  OffsetDateTime
  (clock [zdt] (cljc.java-time.clock/fixed (cljc.java-time.offset-date-time/to-instant zdt)
                 (cljc.java-time.offset-date-time/get-offset zdt)))

  Clock
  (clock [clk] clk)

  ZoneId
  (clock [z] (cljc.java-time.clock/system z)))

(defn tick-resolution
  "Obtains a clock that returns instants from the specified clock truncated to the nearest occurrence of the specified duration."
  ([clk]
   (tick-resolution clk (new-duration 1 :seconds)))
  ([clk dur]
   (cljc.java-time.clock/tick clk dur)))

(extend-protocol p/IConversion
  Clock
  (instant [clk] (cljc.java-time.clock/instant clk)))

(extend-protocol p/IExtraction
  Clock
  (zone [clk] (cljc.java-time.clock/get-zone clk)))

(extend-protocol p/ITimeReify
  Clock
  (in [clk zone] (cljc.java-time.clock/with-zone clk (p/zone zone))))

;; Atomic clocks :)

(defrecord AtomicClock [*clock]
  #?(:clj clojure.lang.IDeref :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] (p/instant @*clock))
  p/IClock
  (clock [_] @*clock))

#?(:clj
   (defmethod print-method AtomicClock
     [& args]
     (apply (get-method print-method clojure.lang.IRecord) args))
   ;todo  - for cljs
   )

(defn atom
  "construct atomic clock"
  ([clk] (->AtomicClock (clojure.core/atom clk)))
  ([] (atom (current-clock))))

(defn swap!
  "swap! on atomic clock 'at' "
  [at f & args]
  (apply clojure.core/swap! (:*clock at) f args))

(defn swap-vals!
  "swap-vals! on atomic clock 'at' " 
  [at f & args]
  (apply clojure.core/swap-vals! (:*clock at) f args))

(defn compare-and-set!
  "cas on atomic clock 'at' "
  [at oldval newval]
  (clojure.core/compare-and-set!
   (:*clock at)
   oldval
   newval))

(defn reset! "reset! on atomic clock 'at' " [at newval]
  (clojure.core/reset!
   (:*clock at)
   newval))

(defn reset-vals! "reset-vals! on atomic clock 'at' " [at newval]
  (clojure.core/reset-vals!
   (:*clock at)
   newval))

;; Arithmetic

(extend-protocol p/ITimeArithmetic
  Duration
  (+ [t d] (cljc.java-time.duration/plus t d))
  (- [t d] (cljc.java-time.duration/minus t d))
  Period
  (+ [t d] (cljc.java-time.period/plus t d))
  (- [t d] (cljc.java-time.period/minus t d)))

(defn negated
  "Return the duration as a negative duration"
  [d]
  (cljc.java-time.duration/negated d))

(defn +
  "Sum amounts of time"
  ([] cljc.java-time.duration/zero)
  ([arg] arg)
  ([arg & args]
   (reduce p/+ arg args)))

(defn -
  "Subtract amounts of time."
  ([] cljc.java-time.duration/zero)
  ([arg] (negated arg))
  ([arg & args]
   (reduce p/- arg args)))


(extend-protocol p/ITimeShift
  Instant
  (forward-duration [t d] (cljc.java-time.instant/plus t d))
  (backward-duration [t d] (cljc.java-time.instant/minus t d))
  #?(:clj Date :cljs js/Date)
  (forward-duration [t d] (p/inst (p/forward-duration (p/instant t) d)))
  (backward-duration [t d] (p/inst (p/backward-duration (p/instant t) d)))
  LocalDate
  (forward-number [t n] (cljc.java-time.local-date/plus-days t n))
  (backward-number [t n] (cljc.java-time.local-date/minus-days t n))
  (forward-duration [t d] (cljc.java-time.local-date/plus t d))
  (backward-duration [t d] (cljc.java-time.local-date/minus t d))
  LocalTime
  (forward-duration [t d] (cljc.java-time.local-time/plus t d))
  (backward-duration [t d] (cljc.java-time.local-time/minus t d))
  LocalDateTime
  (forward-duration [t d] (cljc.java-time.local-date-time/plus t d))
  (backward-duration [t d] (cljc.java-time.local-date-time/minus t d))
  OffsetDateTime
  (forward-duration [t d] (cljc.java-time.offset-date-time/plus t d))
  (backward-duration [t d] (cljc.java-time.offset-date-time/minus t d))
  ZonedDateTime
  (forward-duration [t d] (cljc.java-time.zoned-date-time/plus t d))
  (backward-duration [t d] (cljc.java-time.zoned-date-time/minus t d))
  Year
  (forward-number [t n] (cljc.java-time.year/plus-years t n))
  (backward-number [t n] (cljc.java-time.year/minus-years t n))
  (forward-duration [t d] (cljc.java-time.year/plus t d))
  (backward-duration [t d] (cljc.java-time.year/minus t d))
  YearMonth
  (forward-number [t n] (cljc.java-time.year-month/plus-months t n))
  (backward-number [t n] (cljc.java-time.year-month/minus-months t n))
  (forward-duration [t d] (cljc.java-time.year-month/plus t d))
  (backward-duration [t d] (cljc.java-time.year-month/minus t d))
  Clock
  (forward-duration [clk d] (cljc.java-time.clock/offset clk d))
  (backward-duration [clk d] (cljc.java-time.clock/offset clk (negated d))))

(defn >> "shift Temporal forward" [t n-or-d]
  (if (number? n-or-d)
    (p/forward-number t n-or-d)
    (p/forward-duration t n-or-d)))

(defn << "shift Temporal backward" [t n-or-d]
  (if (number? n-or-d)
    (p/backward-number t n-or-d)
    (p/backward-duration t n-or-d)))

(extend-type Instant
  p/ITimeRangeable
  (range
    ([from] (iterate #(cljc.java-time.instant/plus-seconds % 1) from))
    ([from to] (cond->> (iterate #(cljc.java-time.instant/plus-seconds % 1) from)
                 to (take-while #(p/< % to))))
    ([from to step] (cond->> (iterate #(cljc.java-time.instant/plus % step) from)
                      to (take-while #(p/< % to))))))

(extend-type ZonedDateTime
  p/ITimeRangeable
  (range
    ([from] (iterate #(cljc.java-time.zoned-date-time/plus-seconds % 1) from))
    ([from to] (cond->> (iterate #(cljc.java-time.zoned-date-time/plus-seconds % 1) from)
                 to (take-while #(p/< % to))))
    ([from to step] (cond->> (iterate #(cljc.java-time.zoned-date-time/plus % step) from)
                      to (take-while #(p/< % to))))))

(extend-type LocalDate
  p/ITimeRangeable
  (range
    ([from] (iterate #(cljc.java-time.local-date/plus-days % 1) from))
    ([from to] (cond->> (iterate #(cljc.java-time.local-date/plus-days % 1) from)
                 to (take-while #(p/< % to))))
    ([from to step] (cond->> (iterate #(cljc.java-time.local-date/plus % step) from)
                      to (take-while #(p/< % to))))))

(defn inc [t] (p/forward-number t 1))
(defn dec [t] (p/backward-number t 1))

(defn tomorrow []
  (p/forward-number (today) 1))

(defn yesterday []
  (p/backward-number (today) 1))

(extend-type LocalDateTime
  p/ITimeRangeable
  (range
    ([from] (iterate #(cljc.java-time.local-date-time/plus-seconds % 1) from))
    ([from to] (cond->> (iterate #(cljc.java-time.local-date-time/plus-seconds % 1) from)
                 to (take-while #(p/< % to))))
    ([from to step] (cond->> (iterate #(cljc.java-time.local-date-time/plus % step) from)
                      to (take-while #(p/< % to))))))

(extend-type YearMonth
  p/ITimeRangeable
  (range
    ([from] (iterate #(cljc.java-time.year-month/plus-months % 1) from))
    ([from to] (cond->> (iterate #(cljc.java-time.year-month/plus-months % 1) from)
                 to (take-while #(p/< % to))))
    ([from to step] (cond->> (iterate #(cljc.java-time.year-month/plus % step) from)
                      to (take-while #(p/< % to))))))

(extend-type Year
  p/ITimeRangeable
  (range
    ([from] (iterate #(cljc.java-time.year/plus-years % 1) from))
    ([from to] (cond->> (iterate #(cljc.java-time.year/plus-years % 1) from)
                 to (take-while #(p/< % to))))
    ([from to step] (cond->> (iterate #(cljc.java-time.year/plus % step) from)
                      to (take-while #(p/< % to))))))

(extend-protocol p/IDivisibleDuration
  #?(:clj Long :cljs number)
  (divide-duration [n duration] (cljc.java-time.duration/divided-by duration n))
  Duration
  (divide-duration [divisor duration]
    (/
      (cljc.java-time.duration/get-seconds duration)
      (cljc.java-time.duration/get-seconds divisor))))

(extend-type Duration
  p/IDivisible
  (divide [d x] (p/divide-duration x d)))

;;;
(defn between "the span of time between v1 and v2" [v1 v2] (p/between v1 v2))
(defn beginning "the beginning of the range of ITimeSpan v or v" [v] (p/beginning v))
(defn end "the end of the range of ITimeSpan v or v" [v] (p/end v))

(defn duration "return duration contained within the range of ITimeSpan 'x',  which can be a year, year-month or date " [x]
  (cljc.java-time.duration/between (beginning x) (end x)))

;; Periods

(extend-protocol p/IBetween
  LocalDate
  (between [v1 v2] (cljc.java-time.period/between v1 (p/date v2)))
  LocalTime
  (between [v1 v2] (cljc.java-time.duration/between v1 (p/time v2)))
  ZonedDateTime
  (between [v1 v2] (cljc.java-time.duration/between v1 (p/zoned-date-time v2)))
  LocalDateTime
  (between [v1 v2] (cljc.java-time.duration/between v1 (p/date-time v2)))
  Instant
  (between [v1 v2] (cljc.java-time.duration/between v1 (p/instant v2)))
  OffsetDateTime
  (between [v1 v2] (cljc.java-time.duration/between v1 (p/offset-date-time v2)))
  #?@(:clj [Temporal
            (between [v1 v2] (cljc.java-time.duration/between v1 v2))])
  #?(:clj Date :cljs js/Date)
  (between [x y] (p/between (p/instant x) (p/instant y))))

;; TODO: Test concurrent? in tick.core-test

(extend-protocol p/ITimeSpan
  ; ITimeSpan is implemented by default on types with a natural beginning and end
  LocalDate
  (beginning [date] (cljc.java-time.local-date/at-start-of-day date))
  (end [date] (cljc.java-time.local-date/at-start-of-day (inc date)))

  Year
  (beginning [year] (beginning (cljc.java-time.year/at-month year 1)))
  (end [year] (beginning (cljc.java-time.year/at-month (inc year) 1)))

  YearMonth
  (beginning [ym] (beginning (cljc.java-time.year-month/at-day ym 1)))
  (end [ym] (beginning (cljc.java-time.year-month/at-day (inc ym) 1))))

(defn backward-compatible-time-span-extensions 
  "pre v0.7, ITimeSpan was extended as per this body. run this function to create those extensions.
  
  ITimeSpan is implemented by default on types with a natural beginning and end"
  []
  (extend-protocol p/ITimeSpan
    Instant
    (beginning [i] i)
    (end [i] i)

    ZonedDateTime
    (beginning [i] i)
    (end [i] i)

    OffsetDateTime
    (beginning [i] i)
    (end [i] i)

    #?(:clj Date :cljs js/Date)
    (beginning [i] (p/instant i))
    (end [i] (p/instant i))

    LocalDateTime
    (beginning [x] x)
    (end [x] x)

    LocalTime
    (beginning [x] x)
    (end [x] x)

    nil
    (beginning [_] nil)
    (end [_] nil)))

(extend-protocol p/ITimeReify
  LocalTime
  (on [t d] (cljc.java-time.local-time/at-date t (p/date d)))
  OffsetTime
  (on [t date] (cljc.java-time.offset-time/at-date t (p/date date)))
  LocalDate
  (at [date t] (cljc.java-time.local-date/at-time date (p/time t)))
  LocalDateTime
  (in [ldt z] (cljc.java-time.local-date-time/at-zone ldt (p/zone z)))
  (offset-by [ldt offset] (cljc.java-time.local-date-time/at-offset ldt (p/zone-offset offset)))
  Instant
  (in [t z] (cljc.java-time.instant/at-zone t (p/zone z)))
  (offset-by [t offset] (cljc.java-time.instant/at-offset t (p/zone-offset offset)))
  OffsetDateTime
  (in [t z] (cljc.java-time.offset-date-time/at-zone-same-instant t (p/zone z)))
  ZonedDateTime
  (in [t z] (cljc.java-time.zoned-date-time/with-zone-same-instant t (p/zone z)))
  #?(:clj Date :cljs js/Date)
  (in [t z] (p/in (p/instant t) (p/zone z))))

(extend-protocol p/ILocalTime
  #?(:clj Date :cljs js/Date)
  (local? [_d] false)

  Instant
  (local? [_i] false)

  LocalDateTime
  (local? [_i] true)

  LocalTime
  (local? [_i] true)

  nil
  (local? [_] nil))

(extend-protocol p/MinMax
  LocalTime
  (min-of-type [_] cljc.java-time.local-time/min)
  (max-of-type [_] cljc.java-time.local-time/max)
  LocalDate
  (min-of-type [_] cljc.java-time.local-date/min)
  (max-of-type [_] cljc.java-time.local-date/max)
  LocalDateTime
  (min-of-type [_] cljc.java-time.local-date-time/min)
  (max-of-type [_] cljc.java-time.local-date-time/max)
  Instant
  (min-of-type [_] cljc.java-time.instant/min)
  (max-of-type [_] cljc.java-time.instant/max)
  ;; TODO: This may cause surprises - see clojure/java-time. We should
  ;; change the semantics of nil to not imply epoch, forever, or
  ;; whatever.
  nil
  (min-of-type [_] cljc.java-time.instant/min)
  (max-of-type [_] cljc.java-time.instant/max))


;; first/last using java.time.temporal/TemporalAdjuster
;; See also java.time.temporal/TemporalAdjusters

;; java.time.temporal/TemporalAmount

;; adjust

;; Conversions

;; Ago/hence

(defn ago "current instant shifted back by duration 'dur'" [dur]
  (p/backward-duration (now) dur))

(defn hence "current instant shifted forward by duration 'dur'" [dur]
  (p/forward-duration (now) dur))

(defn midnight? [t]
  (clojure.core/= cljc.java-time.local-time/midnight (p/time t)))

;; Predicates
(defn clock?            "true if v is a clock?" [v] (cljc.java-time.extn.predicates/clock? v))
(defn day-of-week?      "true if v is a day-of-week?" [v] (cljc.java-time.extn.predicates/day-of-week? v))
(defn duration?         "true if v is a duration?" [v] (cljc.java-time.extn.predicates/duration? v))
(defn instant?          "true if v is a instant?" [v] (cljc.java-time.extn.predicates/instant? v))
(defn date?             "true if v is a date?" [v] (cljc.java-time.extn.predicates/local-date? v))
(defn date-time?        "true if v is a date-time?" [v] (cljc.java-time.extn.predicates/local-date-time? v))
(defn time?             "true if v is a time?" [v] (cljc.java-time.extn.predicates/local-time? v))
(defn month?            "true if v is a month?" [v] (cljc.java-time.extn.predicates/month? v))
(defn offset-date-time? "true if v is a offset-date-time?" [v] (cljc.java-time.extn.predicates/offset-date-time? v))
(defn period?           "true if v is a period?" [v] (cljc.java-time.extn.predicates/period? v))
(defn year?             "true if v is a year?" [v] (cljc.java-time.extn.predicates/year? v))
(defn year-month?       "true if v is a year-month?" [v] (cljc.java-time.extn.predicates/year-month? v))
(defn zone?             "true if v is a zone?" [v] (cljc.java-time.extn.predicates/zone-id? v))
(defn zone-offset?      "true if v is a zone-offset?" [v] (cljc.java-time.extn.predicates/zone-offset? v))
(defn zoned-date-time?  "true if v is a zoned-date-time?" [v] (cljc.java-time.extn.predicates/zoned-date-time? v))
(defn interval?         "true if v is a interval?" [v] (satisfies? p/ITimeSpan v))

(def MONDAY cljc.java-time.day-of-week/monday)
(def TUESDAY cljc.java-time.day-of-week/tuesday)
(def WEDNESDAY cljc.java-time.day-of-week/wednesday)
(def THURSDAY cljc.java-time.day-of-week/thursday)
(def FRIDAY cljc.java-time.day-of-week/friday)
(def SATURDAY cljc.java-time.day-of-week/saturday)
(def SUNDAY cljc.java-time.day-of-week/sunday)

(def JANUARY cljc.java-time.month/january)
(def FEBRUARY cljc.java-time.month/february)
(def MARCH cljc.java-time.month/march)
(def APRIL cljc.java-time.month/april)
(def MAY cljc.java-time.month/may)
(def JUNE cljc.java-time.month/june)
(def JULY cljc.java-time.month/july)
(def AUGUST cljc.java-time.month/august)
(def SEPTEMBER cljc.java-time.month/september)
(def OCTOBER cljc.java-time.month/october)
(def NOVEMBER cljc.java-time.month/november)
(def DECEMBER cljc.java-time.month/december)

(def UTC (zone "UTC"))

(def ^{:doc "return e.g Instant/MIN given and Instant"} min-of-type p/min-of-type)
(def ^{:doc "return e.g Instant/MAX given and Instant"} max-of-type p/max-of-type)

(def ^{:doc "Returns a lazy seq of times from start (inclusive) to end (exclusive, nil means forever), by step, where start defaults to 0, step to 1, and end to infinity."} 
  range p/range)

(defn int [arg] (p/int arg))
(defn long [arg] (p/long arg))

;; Reification

(defn on "Set time be ON a date" [t d] (p/on t d))
(defn at "Set date to be AT a time" [d t] (p/at d t))
(defn in "Set a date-time to be in a time-zone" [ldt z] (p/in ldt z))
(defn offset-by "Set a date-time to be offset by an amount" [ldt offset] (p/offset-by ldt offset))

(defn date
  ([] (today))
  ([v] (p/date v)))

(defn inst
  (^java.util.Date [] (p/inst (now)))
  (^java.util.Date [v] (p/inst v)))

(defn instant
  ([] (p/instant (now)))
  ([v] (p/instant v)))

(defn date-time
  ([] (p/date-time (now)))
  ([v] (p/date-time v)))

(defn offset-date-time
  ([] (p/offset-date-time (now)))
  ([v] (p/offset-date-time v)))

(defn zoned-date-time
  ([] (p/zoned-date-time (now)))
  ([v] (p/zoned-date-time v)))

;; Comparison

(extend-protocol p/ITimeComparison
  Instant
  (< [x y] (cljc.java-time.instant/is-before x (instant y)))
  (<= [x y] (not (cljc.java-time.instant/is-after x (instant y))))
  (> [x y] (cljc.java-time.instant/is-after x (instant y)))
  (>= [x y] (not (cljc.java-time.instant/is-before x (instant y))))
  (= [x y] (clojure.core/= x (p/instant y)))
  LocalDateTime
  (< [x y] (cljc.java-time.local-date-time/is-before x y))
  (<= [x y] (not (cljc.java-time.local-date-time/is-after x y)))
  (> [x y] (cljc.java-time.local-date-time/is-after x y))
  (>= [x y] (not (cljc.java-time.local-date-time/is-before x y)))
  (= [x y] (clojure.core/= x y))
  #?(:clj Date :cljs js/Date)
  (<  [x y] (neg? (compare x (inst y))))
  (<= [x y] (not (pos? (compare x (inst y)))))
  (>  [x y] (pos? (compare x (inst y))))
  (>= [x y] (not (neg? (compare x (inst y)))))
  (= [x y] (clojure.core/= x (p/inst y)))
  LocalDate
  (< [x y] (cljc.java-time.local-date/is-before x y))
  (<= [x y] (not (cljc.java-time.local-date/is-after x y)))
  (> [x y] (cljc.java-time.local-date/is-after x y))
  (>= [x y] (not (cljc.java-time.local-date/is-before x y)))
  (= [x y] (clojure.core/= x y))
  LocalTime
  (< [x y] (cljc.java-time.local-time/is-before x y))
  (<= [x y] (not (cljc.java-time.local-time/is-after x y)))
  (> [x y] (cljc.java-time.local-time/is-after x y))
  (>= [x y] (not (cljc.java-time.local-time/is-before x y)))
  (= [x y] (clojure.core/= x y))
  OffsetDateTime
  (< [x y] (cljc.java-time.offset-date-time/is-before x (offset-date-time y)))
  (<= [x y] (not (cljc.java-time.offset-date-time/is-after x (offset-date-time y))))
  (> [x y] (cljc.java-time.offset-date-time/is-after x (offset-date-time y)))
  (>= [x y] (not (cljc.java-time.offset-date-time/is-before x (offset-date-time y))))
  (= [x y] (cljc.java-time.offset-date-time/is-equal x (offset-date-time y)))
  ZonedDateTime
  (< [x y] (cljc.java-time.zoned-date-time/is-before x (zoned-date-time y)))
  (<= [x y] (not (cljc.java-time.zoned-date-time/is-after x (zoned-date-time y))))
  (> [x y] (cljc.java-time.zoned-date-time/is-after x (zoned-date-time y)))
  (>= [x y] (not (cljc.java-time.zoned-date-time/is-before x (zoned-date-time y))))
  (= [x y] (cljc.java-time.zoned-date-time/is-equal x (zoned-date-time y)))
  Year
  (< [x y] (cljc.java-time.year/is-before x y))
  (<= [x y] (not (cljc.java-time.year/is-after x y)))
  (> [x y] (cljc.java-time.year/is-after x y))
  (>= [x y] (not (cljc.java-time.year/is-before x y)))
  (= [x y] (clojure.core/= x y))
  YearMonth
  (< [x y] (cljc.java-time.year-month/is-before x y))
  (<= [x y] (not (cljc.java-time.year-month/is-after x y)))
  (> [x y] (cljc.java-time.year-month/is-after x y))
  (>= [x y] (not (cljc.java-time.year-month/is-before x y)))
  (= [x y] (clojure.core/= x y))
  Duration
  (< [x y] (neg? (cljc.java-time.duration/compare-to x y)))
  (<= [x y] (or (clojure.core/= x y) (neg? (cljc.java-time.duration/compare-to x y))))
  (> [x y] (pos? (cljc.java-time.duration/compare-to x y)))
  (>= [x y] (or (clojure.core/= x y) (pos? (cljc.java-time.duration/compare-to x y))))
  (= [x y] (clojure.core/= x y)))

;; Extraction

(defn nanosecond "extract nanosecond from t" [t] (p/nanosecond t))
(defn microsecond "extract microsecond from t" [t] (p/microsecond t))
(defn millisecond "extract millisecond from t" [t] (p/millisecond t))
(defn second "extract second from t" [t] (p/second t))
(defn minute "extract minute from t" [t] (p/minute t))
(defn hour "extract hour from t" [t] (p/hour t))

;; Conversions, with 0-arity defaults

(defn time
  "extract time from v"
  ([] (p/time (now)))
  ([v] (p/time v)))

(defn day-of-week
  "extract day-of-week from v"
  ([] (p/day-of-week (today)))
  ([v] (p/day-of-week v)))

(defn day-of-month
  "extract day-of-month from v"
  ([] (p/day-of-month (today)))
  ([v] (p/day-of-month v)))

(defn month
  "extract month from v"
  ([] (p/month (today)))
  ([v] (p/month v)))

(defn year
  "extract year from v"
  ([] (p/year (today)))
  ([v] (p/year v)))

(defn year-month
  "extract year-month from v"
  ([] (p/year-month (today)))
  ([v] (p/year-month v)))

(defn clock
  "return i as a clock"
  ([] (current-clock))
  ([i] (p/clock i)))

(defmacro with-clock 
  "temporarily change ambient now+zone info 
   the given 'clock' could be an Instant, zone or zoned-date-time"
  [^java.time.Clock clock & body]
  `(binding [*clock* (p/clock ~clock)]
     ~@body))

;; Formatting
(def predefined-formatters
  {:iso-zoned-date-time  cljc.java-time.format.date-time-formatter/iso-zoned-date-time
   :iso-offset-date-time cljc.java-time.format.date-time-formatter/iso-offset-date-time
   :iso-local-time       cljc.java-time.format.date-time-formatter/iso-local-time
   :iso-local-date-time  cljc.java-time.format.date-time-formatter/iso-local-date-time
   :iso-local-date       cljc.java-time.format.date-time-formatter/iso-local-date
   :iso-instant          cljc.java-time.format.date-time-formatter/iso-instant

   ; these exist in java but not in js-joda
   ;:iso-offset-date      (. DateTimeFormatter -ISO_OFFSET_DATE)
   ;:rfc-1123-date-time   (. DateTimeFormatter -RFC_1123_DATE_TIME)
   ;:iso-week-date        (. DateTimeFormatter -ISO_WEEK_DATE)
   ;:iso-ordinal-date     (. DateTimeFormatter -ISO_ORDINAL_DATE)
   ;:iso-time             (. DateTimeFormatter -ISO_TIME)
   ;:iso-date             (. DateTimeFormatter -ISO_DATE)
   ;:basic-iso-date       (. DateTimeFormatter -BASIC_ISO_DATE)
   ;:iso-date-time        (. DateTimeFormatter -ISO_DATE_TIME)
   ;:iso-offset-time      (. DateTimeFormatter -ISO_OFFSET_TIME)
   })

(defn ^DateTimeFormatter formatter
  "Constructs a DateTimeFormatter out of either a

  * format string - \"YYYY/mm/DD\" \"YYY HH:MM\" etc.
  or
  * formatter name - :iso-instant :iso-local-date etc

  and a Locale, which is optional."
  ([fmt]
   (formatter
     fmt
     #?(:clj (Locale/getDefault)
        :cljs (try
                (some->
                  (goog.object/get js/JSJodaLocale "Locale")
                  (goog.object/get "US"))
                (catch js/Error _e)))))
  ([fmt locale]
   (let [^DateTimeFormatter fmt
         (cond (instance? DateTimeFormatter fmt) fmt
               (string? fmt) (if (nil? locale)
                               (throw
                                 #?(:clj (Exception. "Locale is nil")
                                    :cljs (js/Error. (str "Locale is nil, try adding a require '[tick.locale-en-us]"))))
                               (-> (cljc.java-time.format.date-time-formatter/of-pattern fmt)
                                   (cljc.java-time.format.date-time-formatter/with-locale locale)))
               :else (get predefined-formatters fmt))]
     fmt)))

(defn format
  "Formats the given time entity as a string.
  Accepts something that can be converted to a `DateTimeFormatter` as a first
  argument. Given one argument uses the default format."
  ([o] (str o))
  ([fmt o]
   (cljc.java-time.format.date-time-formatter/format (formatter fmt) o)))


;; Comparisons
(defn =
  "Same as clojure.core/=, but works on dates, rather than numbers.
  can compare different types, e.g. Instant vs ZonedDateTime
  "
  ([_x] true)
  ([x y] (p/= x y))
  ([x y & more] (if (p/= x y)
                  (if (clojure.core/next more)
                    (recur y (first more) (clojure.core/next more))
                    (p/= y (first more)))
                  false)))

(defn <
  "Same as clojure.core/<, but works on dates, rather than numbers"
  ([_x] true)
  ([x y] (p/< x y))
  ([x y & more] (if (p/< x y)
                  (if (clojure.core/next more)
                    (recur y (first more) (clojure.core/next more))
                    (p/< y (first more)))
                  false)))

(defn <=
  "Same as clojure.core/<=, but works on dates, rather than numbers"
  ([_x] true)
  ([x y] (p/<= x y))
  ([x y & more] (if (p/<= x y)
                  (if (clojure.core/next more)
                    (recur y (first more) (clojure.core/next more))
                    (p/<= y (first more)))
                  false)))

(defn >
  "Same as clojure.core/>, but works on dates, rather than numbers"
  ([_x] true)
  ([x y] (p/> x y))
  ([x y & more] (if (p/> x y)
                  (if (clojure.core/next more)
                    (recur y (first more) (clojure.core/next more))
                    (p/> y (first more)))
                  false)))

(defn >=
  "Same as clojure.core/>=, but works on dates, rather than numbers"
  ([_x] true)
  ([x y] (p/>= x y))
  ([x y & more] (if (p/>= x y)
                  (if (clojure.core/next more)
                    (recur y (first more) (clojure.core/next more))
                    (p/>= y (first more)))
                  false)))

(defn greater "the greater of x and y" [x y]
  (if (> x y) x y))

(defn coincident?
  "for the 2-arity ver, Does containing-interval wholly contain the given contained-interval?
  
  for the 3-arity, does the event lie within the span of time described by start and end"
  ([containing-interval contained-interval]
   (and
     (<= (beginning containing-interval) (beginning contained-interval))
     (>= (end containing-interval) (end contained-interval))))
  ([start end event]
   (and
     (<= start event)
     (>= end event))))

(defn max
  "Find the latest of the given arguments. Callers should ensure that no
  argument is nil."
  [arg & args]
  (assert (every? some? (cons arg args)))
  (reduce greater arg args))

(defn lesser "the lesser of x and y" [x y]
  (if (< x y) x y))

(defn min
  "Find the earliest of the given arguments. Callers should ensure that no
  argument is nil."
  [arg & args]
  (assert (every? some? (cons arg args)))
  (reduce lesser arg args))

(defn max-key
  "Same as clojure.core/max-key, but works on dates, rather than numbers"
  ([_k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (let [kx (k x) ky (k y)
         [v kv] (if (> kx ky) [x kx] [y ky])]
     (loop [v v kv kv more more]
       (if more
         (let [w (first more)
               kw (k w)]
           (if (>= kw kv)
             (recur w kw (clojure.core/next more))
             (recur v kv (clojure.core/next more))))
         v)))))

(defn min-key
  "Same as clojure.core/min-key, but works on dates, rather than numbers"
  ([_k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (let [kx (k x) ky (k y)
         [v kv] (if (< kx ky) [x kx] [y ky])]
     (loop [v v kv kv more more]
       (if more
         (let [w (first more)
               kw (k w)]
           (if (<= kw kv)
             (recur w kw (clojure.core/next more))
             (recur v kv (clojure.core/next more))))
         v)))))

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
   (extend-protocol p/ITimeSpan
     clojure.lang.IPersistentMap
     (beginning [m] (beginning-composite m))
     (end [m] (end-composite m))))

#?(:cljs
   (extend-protocol p/ITimeSpan
     PersistentArrayMap
     (beginning [m] (beginning-composite m))
     (end [m] (end-composite m))))

#?(:cljs
   (extend-protocol p/ITimeSpan
     PersistentHashMap
     (beginning [m] (beginning-composite m))
     (end [m] (end-composite m))))


;; Lengths of time (durations & periods)

(defn nanos  "extract nanos from 'v'" [v] (p/nanos v))
(defn micros "extract micros from 'v'" [v] (p/micros v))
(defn millis "extract millis from 'v'" [v] (p/millis v))
(defn seconds"extract seconds from 'v'" [v] (p/seconds v))
(defn minutes"extract minutes from 'v'" [v] (p/minutes v))
(defn hours  "extract hours from 'v'" [v] (p/hours v))
(defn days   "extract days from 'v'" [v] (p/days v))
(defn months "extract months from 'v'" [v] (p/months v))
(defn years  "extract years from 'v'" [v] (p/years v))

(defn divide "divide TemporalAmount t by divisor, which is a unit e.g. :hours or a TemporalAmount" [t divisor]
  (p/divide t divisor))

(defn parse-date
  "to parse an iso-formatted date, use (t/date \"2020..\") instead"
  [date-str formatter]
  (cljc.java-time.local-date/parse date-str formatter))
(defn parse-date-time
  "to parse an iso-formatted date-time, use (t/date-time \"2020..\") instead"
  [date-str formatter]
  (cljc.java-time.local-date-time/parse date-str formatter))
(defn parse-time
  "to parse an iso-formatted time, use (t/time \"20:20..\") instead"
  [date-str formatter]
  (cljc.java-time.local-time/parse date-str formatter))
(defn parse-offset-date-time
  "to parse an iso-formatted offset-date-time, use (t/offset-date-time \"2020..\") instead"
  [date-str formatter]
  (cljc.java-time.offset-date-time/parse date-str formatter))
(defn parse-year
  "to parse an iso-formatted year, use (t/year \"2020\") instead"
  [date-str formatter]
  (cljc.java-time.year/parse date-str formatter))
(defn parse-year-month
  "to parse an iso-formatted year-month, use (t/year-month \"2020..\") instead"
  [date-str formatter]
  (cljc.java-time.year-month/parse date-str formatter))
(defn parse-zoned-date-time
  "to parse an iso-formatted zoned-date-time, use (t/zoned-date-time \"2020..\") instead"
  [date-str formatter]
  (cljc.java-time.zoned-date-time/parse date-str formatter))
