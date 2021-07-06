;; Copyright © 2016-2017, JUXT LTD.

(ns tick.protocols
  (:refer-clojure :exclude [+ - inc dec max min range time int long < <= > >= next >> << atom swap! swap-vals! compare-and-set! reset! reset-vals! second divide]))

(defprotocol ITimeReify
  (on [time date] "Set time be ON a date")
  (at [date time] "Set date to be AT a time")
  (in [dt zone] "Set a date-time to be in a time-zone")
  (offset-by [dt amount] "Set a date-time to be offset by an amount"))

(defprotocol IParseable
  (parse [_] "Parse to most applicable instance."))

(defprotocol ITimeShift
  (forward-number [_ n] "Increment time")
  (forward-duration [_ d] "Increment time")
  (backward-number [_ n] "Decrement time")
  (backward-duration [_ d] "Decrement time"))

(defprotocol ITimeRangeable
  (range [from] [from to] [from to step] "Returns a lazy seq of times from start (inclusive) to end (exclusive, nil means forever), by step, where start defaults to 0, step to 1, and end to infinity."))

(defprotocol IDivisible
  (divide [t divisor] "Divide time"))

(defprotocol IDivisibleDuration
  (divide-duration [divisor duration] "Divide a duration"))

(defprotocol ITimeSpan
  (beginning [_] "Return the beginning of a span of time")
  (end [_] "Return the end of a span of time"))

(defprotocol IConversion
  (inst [_] "Make a java.util.Date instance.")
  (instant [_] "Make a java.time.Instant instance.")
  (offset-date-time [_] "Make a java.time.OffsetDateTime instance.")
  (zoned-date-time [_] "Make a java.time.ZonedDateTime instance."))

(defprotocol ITimeComparison
  (< [x y] "Is x before y?")
  (<= [x y] "Is x before or at the same time as y?")
  (> [x y] "Is x after y?")
  (>= [x y] "Is x after or at the same time as y?")
  (=== [x y] "Is x equals to y?"))

(defprotocol MinMax
  (min-of-type [_] "Return the min")
  (max-of-type [_] "Return the max"))

(defprotocol ITimeArithmetic
  (+ [t d] "Sum amounts of time")
  (- [t d] "Subtract from amount of time, or negate"))

(defprotocol IBetween
  (between [v1 v2] "Return the duration (or period) between two times"))

(defprotocol ILocalTime
  (local? [t] "Is the time a java.time.LocalTime or java.time.LocalDateTime?"))

(defprotocol IClock
  (clock [_] "Make a clock"))

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
