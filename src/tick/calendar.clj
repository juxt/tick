;; Copyright © 2016-2018, JUXT LTD.

(ns tick.calendar
  (:require
   [tick.core :as t]
   [tick.ical :as ical]
   [tick.interval :as ival]
   [clojure.java.io :as io])
  (:import
   [java.time DayOfWeek]))

;; Now individual calendar sources

(defn select-by-year
  "Given the sequence of holidays (which must be intervals, such as iCalendar VEvent objects), select only those of a given year."
  [year holidays]
  (let [year (t/year year)]
    (filter (fn [hol] (= year (t/year hol))) holidays))
  #_(ival/intersection holidays (list (t/year year))))

;; TODO: Promote to API
(defn bank-holidays-in-england-and-wales
  ([]
   (-> "ics/gov.uk/england-and-wales.ics"
       io/resource
       io/reader
       ical/parse-ical
       first
       ical/events
       ))
  ([year]
   (select-by-year
     year
     (bank-holidays-in-england-and-wales))))

;; TODO: Promote to API
(defn weekend?
  "Is the ZonedDateTime during the weekend?"
  [dt]
  (#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} (t/day dt)))

;;(t/year (first (bank-holidays-in-england-and-wales)))

;;(map ival/as-interval (bank-holidays-in-england-and-wales))

;;(ival/as-interval (t/year 2018))

;;(count (select-by-year 2018 (bank-holidays-in-england-and-wales)))
