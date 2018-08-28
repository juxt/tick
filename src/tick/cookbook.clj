(ns tick.cookbook
  (:require [tick.alpha.api :as tick]
            [tick.alpha.api :as t]))


;; Ammend a time / date

(defn add-mins
  [time duration]
  (tick/+ (tick/instant time)
          (tick/new-duration duration :minutes)))

(defn subtract-days
  [date days]
  (tick/- (tick/instant date)
          (tick/new-duration days :days)))

;; Calculate time difference between two instances

(defn time-diff-millis
  "Returning the total time in millis from
  the first instance to the second.
  Handles -ve time"
  [first-instance second-instance]
  (if (tick/< first-instance second-instance)
    (tick/millis (tick/duration {:tick/beginning first-instance
                                 :tick/end second-instance}))
    (let [new-first second-instance
          new-second first-instance]
      (* -1 (time-diff-millis new-first new-second)))))

;; Convert duration into other magnitudes

(defn convert-magnitudes
  "Takes a number and a magnitude keyword and returns
  map containing this time in magnitudes from millis
  to days."
  [x magnitude-kw]
  (let [duration (tick/new-duration x magnitude-kw)]
    {:millis (tick/millis duration)
     :seconds (tick/seconds duration)
     :minutes (tick/minutes duration)
     :hours (tick/hours duration)
     :days (tick/days duration)}))

(tick/new-time)


;; Create a countdown timer

(defn count-down
  [end-time]
  (let [duration (tick/duration
                  {:tick/beginning (tick/instant)
                   :tick/end end-time})
        hours (tick/hours duration)
        minutes (tick/minutes (tick/- duration
                                      (tick/new-duration hours :hours)))
        seconds (tick/seconds (tick/- duration
                                      (tick/new-duration minutes :minutes)
                                      (tick/new-duration hours :hours)))
        millis (tick/millis (tick/- duration
                                     (tick/new-duration seconds :seconds)
                                     (tick/new-duration minutes :minutes)
                                     (tick/new-duration hours :hours)))]
    (if (tick/< (tick/instant) end-time)
      (format "%d hours, %d minutes, %d seconds and %d milliseconds remaining"
              hours minutes seconds millis)
      "Time's up!")))


;; formatting ------------------------------------------------------------------

;; Split up a date-time into a usable map

(defn tick-formatter
  "When given a time-zone will return formatted time in that zone,
  else will return in local-time"
  [t & Z]
  (when inst? t
        (let [t (tick/instant t)
              tz (if Z
                   (apply tick/in t Z)
                   (tick/date-time t))]
          {:day (str (tick/day-of-week t))
           :month (str (tick/month t))
           :dd (tick/day-of-month t)
           :MM (tick/int (tick/month t))
           :yy (- (tick/int (tick/year t))
                  2000)  ;; check this in ~ 800 years
           :yyyy (tick/int (tick/year t))
           :mm (tick/minute t)
           :HH (tick/hour tz)
           :ss (tick/second t)
           :Z (str (tick/zone (if Z tz (tick/zoned-date-time t))))})))

;; HH:mm time -> (str (tick/time)) will do similar, although will give seconds
;; and milliseconds too.

(defn format-time
  [t]
  (when (inst? t)
    (let [mapified-time (tick-formatter t)]
              (format "%02d:%02d"
                      (:HH mapified-time)
                      (:mm mapified-time)))))

;; dd-MM-yy date -> can use (tick/date) to extract a date,
;; but returns yyyy-MM-dd.

(defn format-date
  [t]
  (when (inst? t)
    (let [mapified-time (tick-formatter t)]
      (format "%02d-%02d-%02d"
              (:dd mapified-time)
              (:MM mapified-time)
              (:yy mapified-time)))))

;; Time maths  -----------------------------------------------------------------

;; Split an interval into monthly intervals

(defn months-in-year
  [interval]
  (map #(apply tick/new-interval %)
       (tick/divide-by (tick/new-period 1 :months) interval)))

;; Split an interval into 10 intervals

(defn ten-split
  [interval]
  (map #(apply tick/new-interval %)
       (tick/divide-by 10 interval)))
