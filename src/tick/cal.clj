;; Copyright © 2016-2017, JUXT LTD.

(ns tick.cal
  (:import
   [java.time Clock ZoneId Instant Duration DayOfWeek Month ZonedDateTime LocalDate]
   [java.time.temporal ChronoUnit]
   ))

(defn day-of-week
  "Return the day of the week for a given ZonedDateTime"
  [zdt]
  (.getDayOfWeek zdt))

(defn weekend?
  "Is the ZonedDateTime during the weekend?"
  [zdt]
  (#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} (day-of-week zdt)))

(defn- easter-sunday-by-year
  "Return a pair containing [month day] of Easter Sunday given the
  year. Copyright © 2016 Eivind Waaler. EPL v1.0. From
  https://github.com/eivindw/clj-easter-day, using Spencer Jones
  formula."
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
  "Given a ZoneId, return a predicate that tests if the instant falls
  on an Easter Sunday."
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

(defn past? [now]
  (fn [d] (.isBefore d now)))

(defn easter-sundays
  "Copyright © 2016 Eivind Waaler. EPL v1.0. Given a
  java.time.LocalDate (defaults to now), return a sequence of Easter
  Sundays as LocalData instances. From
  https://github.com/eivindw/clj-easter-day, using Spencer Jones
  formula."
  ([^LocalDate from-local-date]
   (let [year (.getYear from-local-date)]
     (drop-while (past? from-local-date)
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
