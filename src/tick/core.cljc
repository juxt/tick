;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:refer-clojure :exclude [+ - inc dec max min range time int long < <= > >= next >> << atom swap! swap-vals! compare-and-set! reset! reset-vals! second divide])
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
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
    [cljc.java-time.duration]
    [cljc.java-time.year-month]
    [cljc.java-time.month]
    [cljc.java-time.year]
    [cljc.java-time.day-of-week]
    [cljc.java-time.period]
    [cljc.java-time.duration]
    [cljc.java-time.temporal.temporal-amount]
    [cljc.java-time.temporal.temporal-adjusters]
    [cljc.java-time.temporal.chrono-field]
    [cljc.java-time.temporal.chrono-unit]
    
    #?@(:clj
        [
    [tick.time-literals :refer [modify-printing-of-time-literals-if-enabled!]]]
        :cljs
        [[java.time :refer [Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime
                            LocalDateTime LocalDate Year YearMonth OffsetDateTime OffsetTime]]
         [java.time.temporal :refer [ChronoUnit ChronoField Temporal TemporalAdjusters]]
         [cljs.java-time.extend-eq-and-compare]]))
  #?(:cljs
     (:require-macros [tick.time-literals :refer [modify-printing-of-time-literals-if-enabled!]])
     :clj
     (:import
       [java.util Date]
       [java.time Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime]
       [java.time.temporal ChronoUnit ChronoField Temporal TemporalAdjusters]
       [clojure.lang ILookup Seqable])))

(modify-printing-of-time-literals-if-enabled!)

(def ^{:dynamic true} *clock* nil)

(defn now []
  (if *clock*
    (cljc.java-time.instant/now *clock*)
    (cljc.java-time.instant/now)))

(defn today []
  (if *clock*
    (cljc.java-time.local-date/now *clock*)
    (cljc.java-time.local-date/now)))

(defn epoch []
  cljc.java-time.instant/epoch)

(defprotocol ITimeReify
  (on [time date] "Set time be ON a date")
  (at [date time] "Set date to be AT a time")
  (in [dt zone] "Set a date-time to be in a time-zone")
  (offset-by [dt amount] "Set a date-time to be offset by an amount"))

(defn midnight
  ([] cljc.java-time.local-time/midnight)
  ([^LocalDate date]
   (at date cljc.java-time.local-time/midnight)))

(defn noon
  ([] cljc.java-time.local-time/noon)
  ([^LocalDate date]
   (at date cljc.java-time.local-time/noon)))

(s/def ::instant #(instance? Instant %))

(defn parse-day [input]
  (condp re-matches (str/lower-case input)
    #"(mon)(day)?" cljc.java-time.day-of-week/monday
    #"(tue)(s|sday)?" cljc.java-time.day-of-week/tuesday
    #"(wed)(s|nesday)?" cljc.java-time.day-of-week/wednesday
    #"(thur)(s|sday)?" cljc.java-time.day-of-week/thursday
    #"(fri)(day)?" cljc.java-time.day-of-week/friday
    #"(sat)(urday)?" cljc.java-time.day-of-week/saturday
    #"(sun)(day)?" cljc.java-time.day-of-week/sunday
    nil))

(defn parse-month [input]
  (condp re-matches (str/lower-case input)
    #"(jan)(uary)?" cljc.java-time.month/january
    #"(feb)(ruary)?" cljc.java-time.month/february
    #"(mar)(ch)?" cljc.java-time.month/march
    #"(apr)(il)?" cljc.java-time.month/april
    #"may" cljc.java-time.month/may
    #"(jun)(e)?" cljc.java-time.month/june
    #"(jul)(y)?" cljc.java-time.month/july
    #"(aug)(ust)?" cljc.java-time.month/august
    #"(sep)(tember)?" cljc.java-time.month/september
    #"(oct)(tober)?" cljc.java-time.month/october
    #"(nov)(ember)?" cljc.java-time.month/november
    #"(dec)(ember)?" cljc.java-time.month/december
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
      :>> (fn [[_ h ap]] (cljc.java-time.local-time/of (cond-> (parse-int h) (= "pm" ap) (clojure.core/+ 12)) 0))
      #"(\d{1,2})"
      :>> (fn [[_ h]] (cljc.java-time.local-time/of (parse-int h) 0))
      #"\d{2}:\d{2}\S*"
      :>> (fn [s] (cljc.java-time.local-time/parse s))
      #"(\d{1,2}):(\d{2})"
      :>> (fn [[_ h m]] (cljc.java-time.local-time/of (parse-int h) (parse-int m)))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z"
      :>> (fn [s] (cljc.java-time.instant/parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}"
      :>> (fn [s] #?(:clj (cljc.java-time.offset-date-time/parse s)
                     :cljs (cljc.java-time.zoned-date-time/parse s)))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}\[\w+/\w+\]"
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

(defprotocol IConversion
  (inst [_] "Make a java.util.Date instance.")
  (instant [_] "Make a java.time.Instant instance.")
  (offset-date-time [_] "Make a java.time.OffsetDateTime instance.")
  (zoned-date-time [_] "Make a java.time.ZonedDateTime instance."))

(defprotocol IExtraction
  (time [_] "Make a java.time.LocalTime instance.")
  (date [_] "Make a java.time.LocalDate instance.")
  (date-time [_] "Make a java.time.LocalDateTime instance.")
  (nanosecond [_] "Return the millisecond field of the given time")
  (microsecond [_] "Return the millisecond field of the given time")
  (millisecond [_] "Return the millisecond field of the given time")
  (second [_] "Return the second field of the given time")
  (minute [_] "Return the minute field of the given time")
  (hour [_] "Return the hour field of the given time")
  (day-of-week [_] "Make a java.time.DayOfWeek instance.")
  (day-of-month [_] "Return value of the day in the month as an integer.")
  (int [_] "Return value as integer")
  (long [_] "Return value as long")
  (month [_] "Make a java.time.Month instance.")
  (year [_] "Make a java.time.Year instance.")
  (year-month [_] "Make a java.time.YearMonth instance.")
  (zone [_] "Make a java.time.ZoneId instance.")
  (zone-offset [_] "Make a java.time.ZoneOffset instance."))

(defn new-time
  ([] (time (now)))
  ([hour minute] (cljc.java-time.local-time/of hour minute))
  ([hour minute second] (cljc.java-time.local-time/of hour minute second))
  ([hour minute second nano] (cljc.java-time.local-time/of hour minute second nano)))

(defn new-date
  ([] (today))
  ([year month day-of-month]
   (cljc.java-time.local-date/of (clojure.core/int year) (clojure.core/int month) (clojure.core/int day-of-month)))
  ([year day-of-year]
   (cljc.java-time.local-date/of-year-day year day-of-year))
  ([epoch-day]
   (cljc.java-time.local-date/of-epoch-day epoch-day)))

(defn current-zone
  "Return the current zone, which can be overridden by the *clock* dynamic var"
  []
  (if-let [clk *clock*]
    (cljc.java-time.clock/get-zone clk)
    (cljc.java-time.zone-id/system-default)))

(extend-protocol IConversion
  #?(:clj clojure.lang.Fn :cljs function)
  (inst [f] (inst (f)))
  (instant [f] (instant (f)))
  (offset-date-time [f] (offset-date-time (f)))
  (zoned-date-time [f] (zoned-date-time (f)))

  Instant
  (inst [i] #?(:clj (Date/from i) :cljs (js/Date. (cljc.java-time.instant/to-epoch-milli i))))
  (instant [i] i)
  (offset-date-time [i] #?(:clj (cljc.java-time.offset-date-time/of-instant i (current-zone))
                           :cljs (zoned-date-time i)))
  (zoned-date-time [i] (cljc.java-time.zoned-date-time/of-instant i (current-zone)))

  #?(:clj String :cljs string)
  (inst [s] (inst (instant s)))
  (instant [s] (instant (parse s)))
  (offset-date-time [s] #?(:clj (cljc.java-time.offset-date-time/parse s)
                           :cljs (zoned-date-time s)))
  (zoned-date-time [s] (cljc.java-time.zoned-date-time/parse s))

  #?(:clj Number :cljs number)
  (instant [n] (cljc.java-time.instant/of-epoch-milli n))

  LocalDateTime
  (inst [ldt] (inst (zoned-date-time ldt)))
  (instant [ldt] (instant (zoned-date-time ldt)))
  (offset-date-time [ldt] #?(:clj (cljc.java-time.local-date-time/at-offset
                                    ldt
                                    (-> (cljc.java-time.zone-id/system-default)
                                        (cljc.java-time.zone-id/get-rules)
                                        (.getOffset ldt)))
                             :cljs (zoned-date-time ldt)))
  (zoned-date-time [ldt] (cljc.java-time.local-date-time/at-zone ldt (cljc.java-time.zone-id/system-default)))

  #?(:clj Date :cljs js/Date)
  (inst [d] d)
  (instant [d] #?(:clj (.toInstant d) :cljs (cljc.java-time.instant/of-epoch-milli (.getTime d))))
  (zoned-date-time [d] (zoned-date-time (instant d)))
  (offset-date-time [d] (offset-date-time (instant d)))

  OffsetDateTime
  (inst [odt] (inst (instant odt)))
  (instant [odt] (cljc.java-time.offset-date-time/to-instant odt))
  (offset-date-time [odt] odt)
  (zoned-date-time [odt] (cljc.java-time.offset-date-time/to-zoned-date-time odt))

  ZonedDateTime
  (inst [zdt] (inst (instant zdt)))
  (instant [zdt] (.toInstant zdt))
  (offset-date-time [zdt] #?(:clj (cljc.java-time.zoned-date-time/to-offset-date-time zdt)
                             :cljs zdt))
  (zoned-date-time [zdt] zdt))

(extend-protocol IExtraction
  #?(:clj Object :cljs object)
  (int [v] (#?(:clj clojure.core/int :cljs parse-int) v))
  (long [v] (#?(:clj clojure.core/long :cljs parse-int) v))

  #?(:clj clojure.lang.Fn :cljs function)
  (time [f] (time (f)))
  (date [f] (date (f)))
  (date-time [f] (date-time (f)))
  (nanosecond [f] (nanosecond (f)))
  (microsecond [f] (microsecond (f)))
  (millisecond [f] (millisecond (f)))
  (second [f] (second (f)))
  (minute [f] (minute (f)))
  (hour [f] (hour (f)))
  (day-of-week [f] (day-of-week (f)))
  (day-of-month [f] (day-of-month (f)))
  (int [f] (int (f)))
  (long [f] (long (f)))
  (month [f] (month (f)))
  (year [f] (year (f)))
  (year-month [f] (year-month (f)))
  (zone [f] (zone (f)))
  (zone-offset [f] (zone-offset (f)))

  Instant
  (time [i] (time (zoned-date-time i)))
  (date [i] (date (zoned-date-time i)))
  (date-time [i] (date-time (zoned-date-time i)))
  (nanosecond [t] (nanosecond (zoned-date-time t)))
  (microsecond [t] (microsecond (zoned-date-time t)))
  (millisecond [t] (millisecond (zoned-date-time t)))
  (second [t] (second (zoned-date-time t)))
  (minute [t] (minute (zoned-date-time t)))
  (hour [t] (hour (zoned-date-time t)))
  (day-of-week [i] (day-of-week (date i)))
  (day-of-month [i] (day-of-month (date i)))
  (int [i] (cljc.java-time.instant/get-nano i))
  (long [i] (cljc.java-time.instant/get-epoch-second i))
  (month [i] (month (date i)))
  (year [i] (year (date i)))
  (year-month [i] (year-month (date i)))
  (zone [i] (cljc.java-time.zone-id/of "UTC"))
  (zone-offset [i] cljc.java-time.zone-offset/utc)

  #?(:clj String :cljs string)
  (time [s] (time (parse s)))
  (date [s] (date (parse s)))
  (date-time [s] (cljc.java-time.local-date-time/parse s))
  (day-of-week [s] (or (parse-day s) (day-of-week (date s))))
  (day-of-month [s] (day-of-month (date s)))
  (month [s] (or (parse-month s) (month (date s))))
  (year [s] (year (parse s)))
  (year-month [s] (year-month (parse s)))
  (zone [s] (cljc.java-time.zone-id/of s))
  (zone-offset [s] (cljc.java-time.zone-offset/of s))
  (int [s] (cljc.java-time.instant/get-nano (instant s)))
  (long [s] (cljc.java-time.instant/get-epoch-second (instant s)))

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

  LocalDateTime
  (time [dt] (cljc.java-time.local-date-time/to-local-time dt))
  (date [dt] (cljc.java-time.local-date-time/to-local-date dt))
  (date-time [ldt] ldt)
  (second [t] (cljc.java-time.local-date-time/get-second t))
  (minute [t] (cljc.java-time.local-date-time/get-minute t))
  (hour [t] (cljc.java-time.local-date-time/get-hour t))
  (day-of-week [dt] (day-of-week (date dt)))
  (day-of-month [dt] (day-of-month (date dt)))
  (year-month [dt] (year-month (date dt)))
  (year [dt] (year (date dt)))

  #?(:clj Date :cljs js/Date)
  (date [d] (date (zoned-date-time (instant d)))) ; implicit conversion to UTC
  (date-time [d] (date-time (instant d)))
  (year-month [d] (year-month (date d)))
  (year [d] (year (date d)))

  YearMonth
  (year-month [ym] ym)
  (year [ym] (year (cljc.java-time.year-month/get-year ym)))

  Year
  (year [y] y)
  (int [y] (cljc.java-time.year/get-value y))

  ZoneId
  (zone [z] z)

  ZoneOffset
  (zone-offset [z] z)

  OffsetDateTime
  (time [odt] (cljc.java-time.offset-date-time/to-local-time odt))
  (date [odt] (cljc.java-time.offset-date-time/to-local-date odt))
  (date-time [odt] (cljc.java-time.offset-date-time/to-local-date-time odt))

  ZonedDateTime
  (time [zdt] (cljc.java-time.zoned-date-time/to-local-time zdt))
  (date [zdt] (cljc.java-time.zoned-date-time/to-local-date zdt))
  (date-time [zdt] (cljc.java-time.zoned-date-time/to-local-date-time zdt))
  (nanosecond [t] (cljc.java-time.zoned-date-time/get t cljc.java-time.temporal.chrono-field/nano-of-second))
  (microsecond [t] (cljc.java-time.local-time/get t cljc.java-time.temporal.chrono-field/micro-of-second))
  (millisecond [t] (cljc.java-time.local-time/get t cljc.java-time.temporal.chrono-field/milli-of-second))
  (second [t] (cljc.java-time.zoned-date-time/get-second t))
  (minute [t] (cljc.java-time.zoned-date-time/get-minute t))
  (hour [t] (cljc.java-time.zoned-date-time/get-hour t))
  (day-of-week [t] (cljc.java-time.zoned-date-time/get-day-of-week t))
  (day-of-month [t] (cljc.java-time.zoned-date-time/get-day-of-month t))
  (month [zdt] (cljc.java-time.zoned-date-time/get-month zdt))
  (zone [zdt] (cljc.java-time.zoned-date-time/get-zone zdt)))

;; Fields

(def field-map
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
   :year-of-era                  cljc.java-time.temporal.chrono-field/year-of-era                 })

(deftype FieldsLookup [t]
  #?(:clj Seqable :cljs ISeqable)
  (#?(:cljs -seq :clj seq) [_]
    (->> field-map
         (keep (fn [[k v]]
                 (let [cf (get field-map k)]
                   (when (.isSupported t cf)
                     [k (.getLong t cf)]))))
         (into {})
         seq))
  ILookup
  (#?(:clj valAt :cljs -lookup) [_ fld]
    (when-let [f (get field-map fld)]
      (.getLong t f)))
  (#?(:clj valAt :cljs -lookup) [_ fld notfound]
    (if-let [f (get field-map fld)]
      (try
        (.getLong t f)
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
  ([ordinal dow] (cljc.java-time.temporal.temporal-adjusters/day-of-week-in-month ordinal (day-of-week dow)))
  ([t ordinal dow] (with t (day-of-week-in-month ordinal dow))))

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
  ([dow] (cljc.java-time.temporal.temporal-adjusters/first-in-month (day-of-week dow)))
  ([t dow] (with t (first-in-month dow))))

(defn last-day-of-month
  ([] (cljc.java-time.temporal.temporal-adjusters/last-day-of-month))
  ([t] (with t (last-day-of-month))))

(defn last-day-of-year
  ([] (cljc.java-time.temporal.temporal-adjusters/last-day-of-year))
  ([t] (with t (last-day-of-year))))

(defn last-in-month
  ([dow] (cljc.java-time.temporal.temporal-adjusters/last-in-month (day-of-week dow)))
  ([t dow] (with t (last-in-month dow))))

(defn next
  ([dow] (cljc.java-time.temporal.temporal-adjusters/next (day-of-week dow)))
  ([t dow] (with t (next dow))))

(defn next-or-same
  ([dow] (cljc.java-time.temporal.temporal-adjusters/next-or-same (day-of-week dow)))
  ([t dow] (with t (next-or-same dow))))

(defn previous
  ([dow] (cljc.java-time.temporal.temporal-adjusters/previous (day-of-week dow)))
  ([t dow] (with t (previous dow))))

(defn previous-or-same
  ([dow] (cljc.java-time.temporal.temporal-adjusters/previous-or-same (day-of-week dow)))
  ([t dow] (with t (previous-or-same dow))))

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
  (>= [x y] (not (.isBefore x y)))
  Duration
  (< [x y] (neg? (.compareTo x y)))
  (<= [x y] (or (= x y) (.compareTo x y)))
  (> [x y] (pos? (.compareTo x y)))
  (>= [x y] (or (= x y) (pos? (.compareTo x y)))))


;; Units

(def unit-map
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

(def reverse-unit-map (into {} (map vec (map reverse unit-map))))

(defn units [x]
  (into {}
    (for [tu (cljc.java-time.temporal.temporal-amount/get-units x)
          :let [k (reverse-unit-map tu)]
          :when k]
      [k (.get x tu)])))

(defn truncate [x u]
  (when-let [u (get unit-map u)]
    (.truncatedTo x u)))

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

#?(:clj
   (extend-protocol IConversion
     ;; Durations between the epoch and a time. These are useful
     ;; conversion functions in the case where numerics are used.
     Duration
     (instant [d] (java.time.Instant/ofEpochMilli (millis d)))
     (inst [d] (inst (instant d)))))

(extend-protocol ITimeLength
  Duration
  (nanos [d] (.toNanos d))
  (micros [d] (#?(:clj Long/divideUnsigned :cljs cljs.core//) (nanos d) 1000))
  (millis [d] (.toMillis d))
  (seconds [d] (cljc.java-time.duration/get-seconds d))
  (minutes [d] (.toMinutes d))
  (hours [d] (.toHours d))
  (days [d] (.toDays d))

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

;; Coercions

(extend-protocol IExtraction
  Duration
  (zone-offset [d] (cljc.java-time.zone-offset/of-total-seconds (new-duration 1 :seconds))))

;; Clocks

(defn current-clock []
  (or
    *clock*
    (cljc.java-time.clock/system-default-zone)))

(defprotocol IClock
  (clock [_] "Make a clock"))

(extend-protocol IClock
  Instant
  (clock [i] (cljc.java-time.clock/fixed i (cljc.java-time.zone-id/system-default)))

  ZonedDateTime
  (clock [zdt] (cljc.java-time.clock/fixed (.toInstant zdt) (cljc.java-time.zoned-date-time/get-zone zdt)))

  #?(:clj Object :cljs object)
  (clock [o] (clock (zoned-date-time o)))

  Clock
  (clock [clk] clk)

  ZoneId
  (clock [z] (cljc.java-time.clock/system z))

  #?(:clj String :cljs string)
  (clock [s] (clock (parse s))))

(defn advance
  ([clk]
   (advance clk (new-duration 1 :seconds)))
  ([clk dur]
   (cljc.java-time.clock/tick clk dur)))

(extend-protocol IConversion
  Clock
  (instant [clk] (.instant clk)))

(extend-protocol IExtraction
  Clock
  (zone [clk] (cljc.java-time.clock/get-zone clk)))

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
  #?(:clj Date :cljs js/Date)
  (forward-duration [t d] (.plus (instant t) d))
  (backward-duration [t d] (.minus (instant t) d))
  LocalDate
  (forward-number [t n] (.plusDays t n))
  (backward-number [t n] (.minusDays t n))
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.minus t d))
  LocalTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.minus t d))
  LocalDateTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.minus t d))
  OffsetDateTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.minus t d))
  ZonedDateTime
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.minus t d))
  Year
  (forward-number [t n] (.plusYears t n))
  (backward-number [t n] (.minusYears t n))
  YearMonth
  (forward-number [t n] (.plusMonths t n))
  (backward-number [t n] (.minusMonths t n))
  (forward-duration [t d] (.plus t d))
  (backward-duration [t d] (.minus t d))
  Clock
  (forward-duration [clk d] (cljc.java-time.clock/offset clk d))
  (backward-duration [clk d] (cljc.java-time.clock/offset clk (negated d))))

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
                 to (take-while #(< % to))))
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
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type YearMonth
  ITimeRangeable
  (range
    ([from] (iterate #(.plusMonths % 1) from))
    ([from to] (cond->> (iterate #(.plusMonths % 1) from)
                 to (take-while #(< % to))))
    ([from to step] (cond->> (iterate #(.plus % step) from)
                      to (take-while #(< % to))))))

(extend-type Year
  ITimeRangeable
  (range
    ([from] (iterate #(.plusYears % 1) from))
    ([from to] (cond->> (iterate #(.plusYears % 1) from)
                 to (take-while #(< % to))))
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
    (/
      (cljc.java-time.duration/get-seconds duration)
      (cljc.java-time.duration/get-seconds divisor))))

(extend-type Duration
  IDivisible
  (divide [d x] (divide-duration x d)))

(defprotocol ITimeSpan
  (beginning [_] "Return the beginning of a span of time")
  (end [_] "Return the end of a span of time"))

(defn duration [x]
  (cljc.java-time.duration/between (beginning x) (end x)))

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
  #?@(:cljs
      [LocalDate
       (between [v1 v2] (cljc.java-time.period/between v1 (date v2)))
       LocalDateTime
       (between [v1 v2] (cljc.java-time.duration/between v1 (date-time v2)))
       Instant
       (between [v1 v2] (cljc.java-time.duration/between v1 (instant v2)))])
  #?@(:clj [LocalDate
            (between [v1 v2] (Period/between v1 (date v2)))
            Temporal
            (between [v1 v2] (Duration/between v1 v2))])
  #?(:clj String :cljs string)
  (between [v1 v2] (between (parse v1) (parse v2))))

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

  #?(:clj Date :cljs js/Date)
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
  (offset-by [ldt offset] #?(:clj (.atOffset ldt (zone-offset offset))
                             :cljs (.atZone ldt (zone-offset offset))))
  Instant
  ; todo - should use Instant/atZone - await js-joda release with https://github.com/js-joda/js-joda/pull/263
  (in [t z] (cljc.java-time.zoned-date-time/of-instant t z))
  (offset-by [t offset] #?(:clj (.atOffset t (zone-offset offset))
                           ; todo - no OffsetDateTime in js-joda yet
                           :cljs (cljc.java-time.zoned-date-time/of-instant t (zone-offset offset))))
  ZonedDateTime
  (in [t z] (.withZoneSameInstant t (zone z)))
  #?(:clj Date :cljs js/Date)
  (in [t z] (in (instant t) (zone z))))

(defprotocol ILocalTime
  (local? [t] "Is the time a java.time.LocalTime or java.time.LocalDateTime?"))

(extend-protocol ILocalTime
  #?(:clj Date :cljs js/Date)
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
  (min-of-type [_] cljc.java-time.local-time/min)
  (max-of-type [_] cljc.java-time.local-time/max)
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
  (.isZero (cljc.java-time.duration/between t (beginning (date t)))))
