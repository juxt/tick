;; Copyright © 2016, JUXT LTD.

(ns tick.core
  (:require
   [clojure.spec :as s])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month]))

(defn clock []
  (Clock/systemDefaultZone))

(defn clock-ticking-in-seconds []
  (Clock/tickSeconds (ZoneId/systemDefault)))

(defn fixed-clock [^Instant i]
  (Clock/fixed i (ZoneId/systemDefault)))

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

(defn parse
  "Given a string in ISO-8601 format, parse to an java.time.Instant."
  [s]
  (Instant/parse s))

(defn periodic-seq
  "Given a clock and a duration, create an infinite sequence of
  instants starting with the clock's instant and advancing by
  the given period."
  ([^Clock c ^Duration period]
   (iterate #(.addTo period %) (.instant c))))

(defn day-of-week
  "Given a ZoneId, create a function that returns the day of a week an instant occurs on"
  [zid]
  (fn [t]
    (.getDayOfWeek (java.time.ZonedDateTime/ofInstant t zid))))

(defn weekend?
  "Given a ZoneId, return a predicate that tests if an instant is during the weekend"
  [zid]
  (let [dowfn (day-of-week zid)]
    (fn [t]
      (let [dow (dowfn t)]
        (#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} dow)))))

(defn easter-sunday? [zid]
  "Given a ZoneId, return a predicate that tests is the instant falls
  on an Easter Sunday. From https://github.com/eivindw/clj-easter-day,
  using Spencer Jones formula."
  (fn [t]
    (let [inst (java.time.ZonedDateTime/ofInstant t zid)
          year (.getYear inst)
          month (.getMonthValue inst)]
      (and
       (= ((day-of-week zid) t) DayOfWeek/SUNDAY)
       (or (= month 3) (= month 4))
       (let [a (mod year 19)
             b (quot year 100)
             c (mod year 100)
             d (quot b 4)
             e (mod b 4)
             f (quot (+ b 8) 25)
             g (quot (+ (- b f) 1) 3)
             h (mod (+ (* 19 a) (- b d g) 15) 30)
             i (quot c 4)
             k (mod c 4)
             l (mod (- (+ 32 (* 2 e) (* 2 i)) h k) 7)
             m (quot (+ a (* 11 h) (* 22 l)) 451)
             n (quot (+ h (- l (* 7 m)) 114) 31)
             p (mod (+ h (- l (* 7 m)) 114) 31)]
         (and (= n month) (= (.getDayOfMonth inst) (+ p 1))))))))

(defn good-friday? [zid]
  (fn [t]
    ((easter-sunday? zid) (.plus t (days 2)))))

(defn easter-monday? [zid]
  (fn [t]
    ((easter-sunday? zid) (.minus t (days 1)))))

;; TODO: rename?
(defn drainer
  "Call sinkf with each past event. Return future events"
  [^Clock clock sinkf]
  (fn [times]
    (let [now (.instant clock)]
      (loop [tms times]
        (if (.isAfter (first tms) now)
          tms
          (do
            (sinkf (first tms))
            (recur (rest tms))))))))

;; Rename to multiplex? or mult? or merge?
(defn- merge-sort
  "Merge sort across set of collections.
   See http://blog.malcolmsparks.com/?p=42 for full details."
  ([^java.util.Comparator comp colls]
   (let [begin (new Object)
         end (new Object)]
     (letfn [(next-item [[_ colls]]
               (if (nil? colls)
                 [end nil]
                 (let [[[yield & p] & q]
                       (sort-by first comp colls)]
                   [yield (if p (cons p q) q)])))]
       (->> colls
            (vector begin)
            (iterate next-item)
            (drop 1)
            (map first)
            (take-while (partial not= end))))))
  ([colls]
   (merge-sort compare colls)))
