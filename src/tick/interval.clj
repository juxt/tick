;; Copyright © 2016-2017, JUXT LTD.

(ns tick.interval
  (:refer-clojure :exclude [contains? complement partition-by group-by])
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [tick.core :as t])
  (:import
   [java.util Date]
   [java.time Instant Duration ZoneId LocalDate LocalDateTime Year YearMonth ZoneId]))

;; Use of Allen's Interval Algebra from an idea by Eric Evans.

(s/def ::local (s/and #(t/local? (first %)) #(t/local? (second %))))
(s/def ::non-local (s/and #(not (t/local? (first %))) #(not (t/local? (second %)))))

(s/def ::interval
  (s/and
   (s/or :local ::local :non-local ::non-local)
   #(let [[_ [t1 t2]] %] (.isBefore t1 t2))))

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
   :post [(.isBefore (first %) (second %))]}
  (if (neg? (compare v1 v2))
    [v1 v2]
    [v2 v1]))

(defn- join [ival1 ival2]
  (make-interval
   (t/min (first ival1) (first ival2))
   (t/max (second ival1) (second ival2))))

(extend-protocol t/ITime
  clojure.lang.PersistentVector
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
       (vec (concat (span (first v) (second v))
                    (drop 2 v)))))
    ([v1 v2] (join (span v1) (span v2))))

  LocalDateTime
  (span
    ([dt] [dt dt])
    ([dt1 dt2] (join (span dt1) (span dt2))))

  Instant
  (span
    ([i] [i i])
    ([i1 i2] (join (span i1) (span i2))))

  String
  (span
    ([s] (span (t/parse s)))
    ([s1 s2] (join (span s1) (span s2))))

  Date
  (span
    ([d] [(t/instant d) (t/instant d)])
    ([d1 d2] (join (span d1) (span d2)))))

(defn interval
  ([v] (span v))
  ([v1 & args] (reduce span v1 args)))

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

(defn duration
  ([interval]
   (Duration/between (first interval) (second interval)))
  ([i1 i2]
   (Duration/between (t/instant i1) (t/instant i2))))

;; Allen's Basic Relations

(defn precedes? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (.isBefore (second x) (first y)))

(defn equals? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (= x y))

(defn meets? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (= (second x) (first y)))

(defn overlaps? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (.isBefore (first x) (first y))
   (.isAfter (second x) (first y))
   (.isBefore (second x) (second y))))

(defn during? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (.isAfter (first x) (first y))
   (.isBefore (second x) (second y))))

(defn starts? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (= (first x) (first y))
   (.isBefore (second x) (second y))))

(defn finishes? [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (and
   (.isAfter (first x) (first y))
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

(defn complement-relation
  "Return the complement of the general relation. The complement ~r of
  a relation r is the relation consisting of all basic relations not
  in r."
  [^GeneralRelation r]
  (assoc r :relations (remove (set (:relations r)) basic-relations)))

(defn compose-relation
  "Return the composition of r and s"
  [r s]
  (throw (new UnsupportedOperationException "Not yet implemented")))

(defn converse-relation
  "Return the converse of the given general relation. The converse !r
  of a relation r is the relation consisting of the converses of all
  basic relations in r."
  [^GeneralRelation r]
  (assoc r :relations (map conv (:relations r))))

(defn intersection-relation
  "Return the intersection of the r with s"
  [^GeneralRelation r ^GeneralRelation s]
  (s/assert r #(instance? GeneralRelation %))
  (->GeneralRelation (set/intersection (set (:relations r))))
  (throw (new UnsupportedOperationException "Not yet implemented")))

;; Useful relations

(def disjoint? (make-relation precedes? preceded-by? meets? met-by?))
(def concur? (complement-relation disjoint?))

;; Functions that make use of Allens' Interval Algebra

(defn intersection
  "Return the interval representing the intersection of the given intervals. Returns nil if the two given intervals are disjoint."
  [x y]
  (case (code (relation x y))
    \o (make-interval (first y) (second x))
    \O (make-interval (first x) (second y))
    (\s \f \d \e) x
    (\S \F \D) y
    nil))

(defn concurrencies
  "Return a sequence of occurances where intervals coincide (having non-nil intersections)."
  [& intervals]
  (let [intervals (vec intervals)]
    (for [xi (range (count intervals))
          yi (range (count intervals))
          :when (< xi yi)
          :let [x (get intervals xi)
                y (get intervals yi)
                ins (intersection x y)]
          :when ins]
      {:x x :y y :intersection ins})))

;; Useful functions that make use of the above.

(defn dates-over
  "Return a lazy sequence of the dates (inclusive) that the given
  (local) interval spans. Must be a local interval, since calendar
  dates are a local construct."
  [ival]
  {:pre [(s/assert ::local ival)]}
  (cond->
      (t/range
       (t/date (first ival))
       (t/date (second ival)))
    ;; Since range is exclusive, we must add one more value, but only if it intersects rather than merely meets.
    (intersection (interval (t/date (second ival))) ival) (concat [(t/date (second ival))])))

(defn year-months-over
  "Return a lazy sequence of the year-months (inclusive) that the
  given interval spans."
  [ival]
  (cond->
      (t/range
       (t/year-month (first ival))
       (t/year-month (second ival)))
    ;; Since range is exclusive, we must add one more value, but only if it intersects rather than merely meets.
    (intersection (interval (t/year-month (second ival))) ival) (concat [(t/year-month (second ival))])))

(defn years-over
  "Return a lazy sequence of the years (inclusive) that the given
  interval spans."
  [ival]
  (cond->
      (t/range
       (t/year (first ival))
       (t/year (second ival)))
    ;; Since range is exclusive, we must add one more value, but only if it intersects rather than merely meets.
    (intersection (interval (t/year (second ival))) ival) (concat [(t/year (second ival))])))

;; TODO: hours, minutes, seconds

(defn- augment-interval
  "Take any additional data in the old interval and apply to the new interval"
  [new-ival old-ival]
  (vec (concat (take 2 new-ival) (drop 2 old-ival))))

(defn partition-by
  "Split the interval in to a lazy sequence of intervals, one for each local date."
  [f ival]
  (->> (f ival)
       (map interval)
       (map (partial intersection ival))
       (map #(augment-interval % ival))
       (remove nil?)))

(defn group-by
  "Split the interval in to a lazy sequence of intervals, one for each local date."
  [f ival]
  (let [ivals (f ival)]
    (->> ivals
         (map interval)
         (map (partial intersection ival))
         (map (fn [k v] (when v [k (list (augment-interval v ival))])) ivals)
         (remove nil?)
         (into {}))))
