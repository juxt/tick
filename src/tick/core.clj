;; Copyright © 2016-2017, JUXT LTD.

(ns tick.core
  (:require
   [clojure.spec.alpha :as s])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate]
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
