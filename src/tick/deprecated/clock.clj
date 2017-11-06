;; Copyright © 2016-2017, JUXT LTD.

(ns tick.deprecated.clock
  (:import
   [java.time Clock ZoneId ZonedDateTime]
   [java.time.temporal ChronoUnit]))

(defn clock-ticking-in-seconds []
  (Clock/tickSeconds (ZoneId/systemDefault)))

(defn now
  ([] (ZonedDateTime/now))
  ([clock] (ZonedDateTime/now clock)))

(defn just-now "Now, but truncated to the nearest second"
  ([] (.truncatedTo (now) (ChronoUnit/SECONDS)))
  ([clock] (.truncatedTo (now clock) (ChronoUnit/SECONDS))))

(defn fixed-clock [^ZonedDateTime zdt]
  (Clock/fixed (.toInstant zdt) (.getZone zdt)))
