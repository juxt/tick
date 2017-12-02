;; Copyright © 2016-2017, JUXT LTD.

(ns tick.interval
  (:refer-clojure :exclude [contains? complement partition-by group-by conj disj])
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [tick.core :as t])
  (:import
   [java.util Date]
   [java.time Instant Duration Period ZoneId LocalDate LocalDateTime Year YearMonth ZoneId ZonedDateTime]))

;; Use of Allen's Interval Algebra from an idea by Eric Evans.

(s/def ::local (s/and #(t/local? (first %)) #(t/local? (second %))))
(s/def ::non-local (s/and #(not (t/local? (first %))) #(not (t/local? (second %)))))

(s/def ::interval
  (s/and
   (s/or :local ::local :non-local ::non-local)
   #(let [[_ [t1 t2]] %] (t/< t1 t2))))

;; An interval can be between 2 local times or 2 non-local times.
;; When there is a mix, an error occurs.
;; The second interval must be after the first interval.

(defn- make-interval
  "Make an interval from unordered arguments. Arguments must both be
  local, or both non-local (zoned)."
  [v1 v2]
  {:pre [(s/assert
          (s/or :local (s/tuple t/local? t/local?)
                :non-local (s/tuple (comp not t/local?) (comp not t/local?)))
          [v1 v2])]
   ;; Post condition must hold, and it is intentional that is cannot be disabled.
   ;; Intervals must be non-zero as an axiom of Allen's Interval Algebra.
   :post [(neg? (compare (first %) (second %)))]}
  (if (neg? (compare v1 v2))
    [v1 v2]
    [v2 v1]))

(defn- join [ival1 ival2]
  (make-interval
   (t/min (first ival1) (first ival2))
   (t/max (second ival1) (second ival2))))

(extend-type clojure.lang.PersistentVector
  t/ITimeRange
  (start [v] (first v))
  (end [v] (second v))
  t/ITime
  (local? [ival] (and
                   (t/local? (first ival))
                   (t/local? (second ival)))))

(defprotocol ISpan
  (span [_] [_ _] "Return an interval from a bounded period of time."))

(extend-protocol ISpan
  LocalDate
  (span
    ([date] (make-interval (t/start date) (t/end date)))
    ([date1 date2] (join (span date1) (span date2))))

  YearMonth
  (span
    ([ym] (span (t/start ym) (t/end ym)))
    ([ym v] (span (t/start ym) (t/end v))))

  Year
  (span
    ([y] (span (t/start y) (t/end y)))
    ([y v] (span (t/start y) (t/end v))))

  clojure.lang.PersistentVector
  (span
    ([v]
     (if (= 2 (count v))
       (span (first v) (second v))
       (vec (clojure.core/concat (span (first v) (second v))
                                 (drop 2 v)))))
    ([v1 v2] (join (span v1) (span v2))))

  LocalDateTime
  (span
    ([x] [x x])
    ([x y] (join (span x) (span y))))

  Instant
  (span
    ([x] [x x])
    ([x y] (join (span x) (span y))))

  ZonedDateTime
  (span
    ([x] [x x])
    ([x y] (join (span x) (span y))))

  String
  (span
    ([x] (span (t/parse x)))
    ([x y] (join (span x) (span y))))

  Date
  (span
    ([d] [(t/instant d) (t/instant d)])
    ([d1 d2] (join (span d1) (span d2)))))

(defn interval
  ([v] (span v))
  ([v1 & args]
   (reduce span v1 args)))

(defn- interval-at-zone
  "Put the given interval at the given zone."
  [interval ^ZoneId zone]
  (s/assert ::interval interval)
  (-> interval
      (update 0 t/at-zone zone)
      (update 1 t/at-zone zone)))

(defn- local-interval
  "Put the given interval at the given zone and convert to local time."
  ([interval]
   (s/assert ::interval interval)
   (-> interval
       (update 0 t/to-local)
       (update 1 t/to-local)))
  ([interval ^ZoneId zone]
   (s/assert ::interval interval)
   (-> interval
       (update 0 t/to-local zone)
       (update 1 t/to-local zone))))

(extend-protocol t/IAtZone
  clojure.lang.PersistentVector
  (at-zone [interval zone] (interval-at-zone interval zone))
  (to-local
    ([interval] (local-interval interval))
    ([interval zone] (local-interval interval zone))))

(extend-protocol t/IDurationCoercion
  clojure.lang.PersistentVector
  (duration
    ([v]
     (let [interval (interval v)]
       (s/assert :tick.interval/interval interval)
       (Duration/between (first interval) (second interval))))))

(defn am [^LocalDate date]
  (interval (t/start date) (t/noon date)))

(defn pm [^LocalDate date]
  (interval (t/noon date) (t/end date)))

;; Allen's Basic Relations

(defn precedes? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (t/< (second x) (first y)))

(defn equals? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (= (take 2 x) (take 2 y)))

(defn meets? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (= (second x) (first y)))

(defn overlaps? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (t/< (first x) (first y))
   (t/> (second x) (first y))
   (t/< (second x) (second y))))

(defn during? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (t/> (first x) (first y))
   (t/< (second x) (second y))))

(defn starts? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (= (first x) (first y))
   (t/< (second x) (second y))))

(defn finishes? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (t/> (first x) (first y))
   (= (second x) (second y))))

;; Six pairs of the relations are converses.  For example, the converse of "a precedes b" is "b preceded by a"; whenever the first relation is true, its converse is true also.
(defn conv
  "The converse of a basic relation."
  [f]
  (fn [x y]
    (f y x)))

(defn preceded-by? [x y] ((conv precedes?) x y))
(defn met-by? [x y] ((conv meets?) x y))
(defn overlapped-by? [x y] ((conv overlaps?) x y))
(defn finished-by? [x y] ((conv finishes?) x y))
(defn contains? [x y] ((conv during?) x y))
(defn started-by? [x y] ((conv starts?) x y))

(def code {precedes? \p
           meets? \m
           overlaps? \o
           finished-by? \F
           contains? \D
           starts? \s
           equals? \e
           started-by? \S
           during? \d
           finishes? \f
           overlapped-by? \O
           met-by? \M
           preceded-by? \P})

(def basic-relations
  [precedes? meets? overlaps? finished-by? contains?
   starts? equals? started-by? during? finishes? overlapped-by?
   met-by? preceded-by?])

;; Allen's General Relations

(defrecord GeneralRelation [relations]
  clojure.lang.IFn
  (invoke [_ x y]
    (s/assert ::interval x)
    (s/assert ::interval y)
    (some (fn [f] (when (f x y) f)) relations)))

;; Relations are 'basic relations' in [ALSPAUGH-2009]. Invoking a
;; general relation on two intervals returns the basic relation that
;; causes the general relation to hold. Note there can only be one
;; such basic relation due to the relations being distinct.

(defn make-relation [& basic-relations]
  (->GeneralRelation basic-relations))

(def ^{:doc "A function to determine the (basic) relation between two intervals."}
  relation
  (apply make-relation basic-relations))

;; Operations on relations

(defn complement-r
  "Return the complement of the general relation. The complement ~r of
  a relation r is the relation consisting of all basic relations not
  in r."
  [^GeneralRelation r]
  (assoc r :relations (remove (set (:relations r)) basic-relations)))

(defn compose-r
  "Return the composition of r and s"
  [r s]
  (throw (new UnsupportedOperationException "Not yet implemented")))

(defn converse-r
  "Return the converse of the given general relation. The converse !r
  of a relation r is the relation consisting of the converses of all
  basic relations in r."
  [^GeneralRelation r]
  (assoc r :relations (map conv (:relations r))))

(defn intersection-r
  "Return the intersection of the r with s"
  [^GeneralRelation r ^GeneralRelation s]
  (s/assert r #(instance? GeneralRelation %))
  (->GeneralRelation (set/intersection (set (:relations r))))
  (throw (new UnsupportedOperationException "Not yet implemented")))

;; Useful named general relations

(def disjoint? (make-relation precedes? preceded-by? meets? met-by?))
(def concur? (complement-r disjoint?))

;; Functions that make use of Allens' Interval Algebra

(defn- update-interval
  "Update the interval with the timings of the second interval. The
  purpose of updating is to preserve any extra data in the interval."
  ([ival times]
   (vec (concat (take 2 times) (drop 2 ival))))
  ([ival x y]
   (update-interval ival (interval x y))))

(defn concur
  "Return the interval representing the interval, if there is one,
  representing the interval of time the given intervals are
  concurrent."
  [x y]
  (case (code (relation x y))
    \o (update-interval x (first y) (second x))
    \O (update-interval x (first x) (second y))
    (\s \f \d \e) x
    (\S \F \D) (update-interval x y)
    nil))

(defn ^:experimental concurrencies
  "Return a sequence of occurances where intervals coincide (having
  non-nil concur intervals)."
  [& intervals]
  (let [intervals (vec intervals)]
    (for [xi (range (count intervals))
          yi (range (count intervals))
          :when (< xi yi)
          :let [x (get intervals xi)
                y (get intervals yi)
                conc (concur x y)]
          :when conc]
      {:x x :y y :concur concur})))

;; Comparison. We now have built up the capability of comparing intervals using the normal <, >, <=, >= operators.

(extend-protocol t/ITimeComparison
  LocalDate
  (< [x y] (t/< (interval x) (interval y)))
  (<= [x y] (t/<= (interval x) (interval y)))
  (> [x y] (t/> (interval x) (interval y)))
  (>= [x y] (t/>= (interval x) (interval y)))
  YearMonth
  (< [x y] (t/< (interval x) (interval y)))
  (<= [x y] (t/<= (interval x) (interval y)))
  (> [x y] (t/> (interval x) (interval y)))
  (>= [x y] (t/>= (interval x) (interval y)))
  Year
  (< [x y] (t/< (interval x) (interval y)))
  (<= [x y] (t/<= (interval x) (interval y)))
  (> [x y] (t/> (interval x) (interval y)))
  (>= [x y] (t/>= (interval x) (interval y)))
  clojure.lang.PersistentVector
  (< [x y] (#{precedes? meets?} (relation x y)))
  (<= [x y] (#{precedes? meets? equals? starts? overlaps? finished-by?} (relation x y)))
  (> [x y] (#{preceded-by? met-by?} (relation x y)))
  (>= [x y] (#{preceded-by? met-by? equals? started-by? overlapped-by? finishes?} (relation x y))))

;; Division

(defn divide-by
  "Return a lazy sequence of java.time.Temporal instances over the
  given (local) interval."
  [ival f]
  (cond->
      (t/range
        (f (first ival))
        (f (second ival)))
    ;; Since range is exclusive, we must add one more value, but only
    ;; if it concurs rather than merely meets.
    (concur (interval (f (second ival))) ival)
    (concat [(f (second ival))])))

(defn divide-by-duration
  "Divide an interval by a duration, returning a sequence of
  intervals. If the interval cannot be wholly sub-divided by the
  duration divisor, the last interval will represent the 'remainder'
  of the division and not be as long as the other preceeding
  intervals."
  [ival dur]
  (->> (t/range
         (first ival)
         (second ival)
         dur)
       ;; Bound by given interval, last will become a remainder.
       (map (juxt identity #(t/min (t/+ % dur) (t/end ival))))
       (map interval)))

(defn divide-by-period
  [ival period]
  (->> (t/range
         (first ival)
         (second ival)
         period)
       ;; Bound by given interval, last will become a remainder.
       (map (juxt identity #(t/min (t/+ % period) (t/end ival))))
       (map interval)))

(defn divide-by-divisor [ival divisor]
  (divide-by-duration ival (.dividedBy (t/duration ival) divisor)))

(defprotocol IDivisibleInterval
  (divide [divisor ival] "Divide an interval by a given divisor"))

(defmulti divide-by-keyword ""
  (fn [ival k] k))

(defmethod divide-by-keyword :hours [ival _]
  (divide-by-duration ival (t/duration 1 :hours)))

(defmethod divide-by-keyword :minutes [ival _]
  (divide-by-duration ival (t/duration 1 :minutes)))

(defmethod divide-by-keyword :days [ival _]
  (divide-by ival t/date))

(defmethod divide-by-keyword :months [ival _]
  (divide-by ival t/year-month))

(defmethod divide-by-keyword :years [ival _]
  (divide-by ival t/year))

(extend-protocol IDivisibleInterval
  clojure.lang.Keyword
  (divide [kw ival] (divide-by-keyword ival kw))
  Duration
  (divide [dur ival] (divide-by-duration ival dur))
  Period
  (divide [period ival] (divide-by-period ival period))
  Long
  (divide [divisor ival] (divide-by-divisor ival divisor)))

;; TODO: hours-over, minutes-over, seconds-over, millis-over?,

(extend-protocol t/IDivisible
  LocalDate
  (/ [ld d] (divide d (interval ld)))
  Year
  (/ [n d] (divide d (interval n)))
  YearMonth
  (/ [n d] (divide d (interval n)))
  clojure.lang.PersistentVector
  (/ [ival o] (divide o ival)))

;; Interval sets - sequences of mutually disjoint intervals

(defn ordered-disjoint-intervals?
  "Are all the intervals in the given set temporarily ordered and
  disjoint? This is a useful property of a collection of
  intervals. The given collection must contain proper intervals (that
  is, intervals that have finite greater-than-zero durations)."
  [s]
  (let [rel (make-relation precedes? meets?)]
    (some?
     (loop [[x & xs] s]
       (if (or (nil? x) (nil? (first xs))) true
           (when (rel x (first xs))
             (recur xs)))))))

(defn union
  "Combine multiple collections of intervals into a single ordered
  collection of ordered disjoint intervals."
  [& colls]
    (loop [colls colls result []]
      (let [colls (remove nil? colls)]

        (if (< (count colls) 2)
          (clojure.core/concat result (first colls))

          (let [[c1 c2 & r] (sort-by ffirst colls)]
            (if (disjoint? (first c1) (first c2))
              (recur (apply list (next c1) c2 r) (clojure.core/conj result (first c1)))

              (recur (apply list
                            (next c1)
                            (clojure.core/concat [(interval (first c1) (first c2))]
                                                 (next c2))
                            r)
                     result)))))))

(defn conj [coll interval]
  (union coll [interval]))

(defn intersection
  "Return an interval set that is the intersection of the input
  interval sets."
  ;; Single arity
  ([s1] s1)
  ;; 2-arity
  ([s1 s2]
   (loop [xs s1
          ys s2
          result []]
     (if (and (not-empty xs) (not-empty ys))
       (let [x (first xs) y (first ys)
             code (code (relation x y))]
         (case code
           (\p \m)  (recur (next xs) ys result)
           (\P \M)  (recur xs (next ys) result)
           \S (recur (cons (update-interval x (second y) (second x))
                           (next xs))
                     (next ys)
                     (clojure.core/conj result (update-interval x y)))
           \F (recur (next xs) (next ys) (clojure.core/conj result y))
           \o (recur (cons (update-interval x (first y) (second x))
                           (next xs))
                     (cons (update-interval x (second x) (second y)) (next ys))
                     (clojure.core/conj
                      result
                      (update-interval x (first y) (second x))))
           \O (recur (cons
                      (update-interval x (second y) (second x))
                      (next xs))
                     (next ys)
                     (clojure.core/conj
                      result
                      (update-interval x (first x) (second y))))
           \D (recur (cons
                      (update-interval x (second y) (second x))
                      (next xs))
                     (next ys)
                     (clojure.core/conj result (update-interval x y)))
           \d (recur (next xs) (cons (update-interval x (second x) (second y))
                                     (next ys))
                     (clojure.core/conj result x))
           \e (recur (next xs) (next ys) (clojure.core/conj result x))
           \f (recur (next xs) (next ys) (clojure.core/conj result x))
           \s (recur (next xs)
                     (cons (update-interval x (second x) (second y))
                           (next ys))
                     (clojure.core/conj result x))))
       result)))
  ([s1 s2 & sets]
   (reduce intersection s1 (clojure.core/conj sets s2))))

(defn difference
  "Return an interval set that is the first set without elements of
  the remaining sets."
  ([s1] s1)
  ([s1 s2]
   (loop [xs s1
          ys s2
          result []]
     (if (not-empty xs)
       (if (not-empty ys)
         (let [x (first xs) y (first ys)
               code (code (relation x y))]
           (case code
             (\p \m) (recur (next xs) ys (clojure.core/conj result x))
             (\P \M) (recur xs (next ys) result)
             (\f \d \e) (recur (next xs) (next ys) result)
             \s (recur (next xs) ys result)
             (\S \O) (recur (cons (interval (second y) (second x)) (next xs)) (next ys) result)
             \F (recur (next xs) (next ys) (clojure.core/conj result (update-interval x (first x) (first y))))
             \o (recur (next xs) ys (clojure.core/conj result (update-interval x (first x) (first y))))
             \D (recur (cons (interval (second y) (second x)) (next xs))
                       (next ys)
                       (clojure.core/conj result (update-interval x (first x) (first y))))))
         (apply clojure.core/conj result xs))
       result)))
  ([s1 s2 & sets]
   (reduce difference s1 (clojure.core/conj sets s2))))

(defn disj [coll interval]
  (difference coll [interval]))

(defn complement [coll]
  (if (empty? coll)
    [(interval (t/min-of-type (t/now)) (t/max-of-type (t/now)))]
    (let [r (map (fn [[x y]] (interval (second x) (first y)))
                 (partition 2 1 coll))]
      (cond-> r
        (not= (ffirst coll) (t/min-of-type (ffirst coll)))
        (#(concat [(interval (t/min-of-type (ffirst coll)) (ffirst coll))] %))
        (not= (second (last coll)) (t/max-of-type (second (last coll))))
        (#(concat % [(interval (second (last coll)) (t/max-of-type (second (last coll))))]))))))
