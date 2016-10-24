;; Copyright © 2016, JUXT LTD.

(ns tick.core
  (:require
   [clojure.spec :as s])
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate]
   [java.time.temporal ChronoUnit]
   [java.util.concurrent TimeUnit ScheduledThreadPoolExecutor]))

(defn clock []
  (Clock/systemDefaultZone))

(defn clock-ticking-in-seconds []
  (Clock/tickSeconds (ZoneId/systemDefault)))

(defn now
  ([]
   (ZonedDateTime/now))
  ([clock]
   (ZonedDateTime/now clock)))

(defn just-now "Now, but truncated to the nearest second"
  ([] (.truncatedTo (now) (ChronoUnit/SECONDS)))
  ([clock] (.truncatedTo (now clock) (ChronoUnit/SECONDS))))

(defn fixed-clock [^ZonedDateTime zdt]
  (Clock/fixed (.toInstant zdt) (.getZone zdt)))

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

(defn periodic-seq
  "Given a start time, create a timeline with times at constant intervals of period length"
  ([^ZonedDateTime start ^Duration period]
   (iterate #(.addTo period %) start)))

(defn day-of-week
  "Return the day of the week for a given ZonedDateTime"
  [zdt]
  (.getDayOfWeek zdt))

(defn weekend?
  "Is the ZonedDateTime during the weekend?"
  [zdt]
  (#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} (day-of-week zdt)))

(defn- easter-sunday-by-year
  "Return a pair containing [month day] of Easter Sunday given the year."
  ;; TODO: From what year does this algorithm makes sense from, need
  ;; to throw an exception outside this range.
  [year]
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
    [n (+ p 1)]))

(defn easter-sunday? [dt]
  "Copyright © 2016 Eivind Waaler. EPL v1.0. Given a ZoneId, return a
  predicate that tests if the instant falls on an Easter Sunday. From
  https://github.com/eivindw/clj-easter-day, using Spencer Jones
  formula."
  (let [year (.getYear dt)
        month (.getMonthValue dt)]
    (and
     (= (day-of-week dt) DayOfWeek/SUNDAY)
     (or (= month (.getValue java.time.Month/MARCH)) (= month (.getValue java.time.Month/APRIL)))
     (let [[m d] (easter-sunday-by-year year)]
       (and (= m month) (= (.getDayOfMonth dt) d))))))

(defn good-friday? [dt]
  (easter-sunday? (.plusDays dt 2)))

(defn easter-monday? [dt]
  (easter-sunday? (.minusDays dt 1)))

(defn drop-past [now]
  (fn [d] (.isBefore d now)))

(defn easter-sundays
  "Copyright © 2016 Eivind Waaler. EPL v1.0. Given a
  java.time.LocalDate (defaults to now), return a sequence of Easter
  Sundays as LocalData instances. From
  https://github.com/eivindw/clj-easter-day, using Spencer Jones
  formula."
  ([^LocalDate from-local-date]
   (let [year (.getYear from-local-date)]
     (drop-while (drop-past from-local-date)
                 (for [year (range year 2200)]
                   (let [[month day] (easter-sunday-by-year year)]
                     (LocalDate/of year month day))))))
  ([]
   (easter-sundays (LocalDate/now))))

(defn easter-sundays
  "Copyright © 2016 Eivind Waaler. EPL v1.0. Given a
  java.time.LocalDate (defaults to now), return a sequence of Easter
  Sundays as LocalData instances. From
  https://github.com/eivindw/clj-easter-day, using Spencer Jones
  formula."
  ([^LocalDate from-local-date]
   (let [year (.getYear from-local-date)]
     (drop-while (drop-past from-local-date)
                 (for [year (range year 2200)]
                   (let [[month day] (easter-sunday-by-year year)]
                     (LocalDate/of year month day))))))
  ([]
   (easter-sundays (LocalDate/now))))

(defn good-fridays
  ([^LocalDate from-local-date]
   (map
    ;; Strangly, we get an error with (.plus ... (days 2)) which I think is a Java bug.
    #(.minusDays % 2)
    (easter-sundays (.plusDays from-local-date 2))))
  ([]
   (good-fridays (LocalDate/now))))

(defn easter-mondays
  ([^LocalDate from-local-date]
   (map
    #(.plusDays % 1)
    (easter-sundays (.minusDays from-local-date 1))))
  ([]
   (easter-mondays (LocalDate/now))))

;; TODO: Easter Monday

;; Scheduler

;; An atom containing a (potentially ticking) clock.
;; Functions to replace the clock, or programmatically tick a fixed clock
;; A scheduled-future registered with a ScheduledThreadPoolExecutor, which gets cancelled on any change to a clock
;; A function to call with a ZDT as an argument
;;

(defn schedule-next [clock next-time executor cb]
  (when next-time
    (let [dly (.until (.instant clock) next-time ChronoUnit/MILLIS)]
      (.schedule executor ^Callable cb dly TimeUnit/MILLISECONDS))))

(defn callback [timeline clock trigger executor]
  (let [[due next-timeline] (split-with #(not (.isAfter (.toInstant %) (.instant clock))) timeline)]
    (schedule-next clock (first next-timeline) executor #(callback next-timeline clock trigger executor))
    (dorun (map trigger due))))

(defprotocol ITicker
  (start [_ clock] [_ clock executor])
  (pause [_])
  (resume [_])
  (stop [_]))

(defrecord Ticker [trigger timeline]
  ITicker
  (start [this clock]
    (start this clock (new ScheduledThreadPoolExecutor 16)))
  (start [_ clock executor]
    ;; TODO: Store this ScheduledFuture so we can cancel it on 'stop'
    (schedule-next
     clock
     (first timeline)
     executor
     #(callback timeline clock trigger executor)))
  (pause [_] nil)
  (resume [_] nil)
  (stop [_] nil))

(defn mapt
  "Think of this like map, but applying a function over a timeline. Returns a ticker."
  [trigger timeline]
  (map->Ticker {:trigger trigger
                :timeline timeline}))

(defn- merge-timelines
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
   (merge-timelines compare colls)))
