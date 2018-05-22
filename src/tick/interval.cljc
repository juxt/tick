;; Copyright © 2016-2017, JUXT LTD.

(ns tick.interval
  (:refer-clojure :exclude [contains? complement partition-by group-by conj disj extend divide])
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [tick.core :as t]
    #?(:cljs
       [tick.js-joda :refer [Date Clock ZoneId ZoneOffset Instant Duration Period DayOfWeek Month ZonedDateTime LocalTime LocalDateTime LocalDate Year YearMonth ZoneId OffsetDateTime OffsetTime ChronoUnit ChronoField TemporalAdjusters Temporal TemporalAmount]]))
  #?(:clj
     (:import
       [java.util Date]
       [java.time Instant Duration Period ZoneId LocalDate LocalTime LocalDateTime Year YearMonth OffsetDateTime ZoneId ZonedDateTime]
       [java.time.temporal Temporal TemporalAmount])))

;; Use of Allen's Interval Algebra, inspired from a working
;; demonstration of time-count by Eric Evans.

(defrecord Interval [beginning end]
  t/ITimeSpan
  (beginning [_] beginning)
  (end [_] end))

;; Construction

(defn- make-interval [v1 v2]
  (assert (t/< v1 v2))
  (->Interval v1 v2))

(defn temporal? [o]
  #?(:clj (instance? Temporal o)
     :cljs (.isPrototypeOf Temporal (type o))))

(defn temporal-amount? [o]
  #?(:clj (instance? TemporalAmount o)
     :cljs (.isPrototypeOf TemporalAmount (type o))))

;; interval
;; [t0 t1] interval between t0 and t1
;; [t1 t0] interval between t0 and t1
;; [t0 d] interval between t0 and t0+d, where d is a given duration
;; [d t1] interval between t1-d and t1, where d is a given duration

(defn interval
  [v1 v2]
  (let [t1 (t/temporal-value v1)
        t2 (t/temporal-value v2)]
    (assert (t/< t1 t2))
    (->Interval t1 t2)))

;; Adjustments

;; (extend _ & durations) to extend the interval, add durations to the end
;; Durations can be negative, so a retraction is simply an extend with a negative duration.

(defn extend [ival dur]
  (make-interval
    (t/beginning ival)
    (t/forward-duration (t/end ival) dur)))

(defn scale [ival factor]
  (make-interval
    (t/beginning ival)
    (t/forward-duration (t/beginning ival) (.multipliedBy (t/length ival) factor))))

(extend-protocol t/ITimeShift
  Interval
  (forward-duration [ival d]
    (make-interval
      (t/forward-duration (t/beginning ival) d)
      (t/forward-duration (t/end ival) d)))
  (backward-duration [ival d]
    (make-interval
      (t/backward-duration (t/beginning ival) d)
      (t/backward-duration (t/end ival) d))))

;; An interval of duration d to t1 can be constructed like this:
;; (scale (interval t1 d) -1)


;; (>> _ d) to shift the interval into the future by duration d
;; (<< _ d) to shift the interval into the past by duration d

;; (* _) to duplicate into meeting intervals, into a sequence (possibly need a record container for this, IntervalSeq)

;; (interpose IntervalSeq d)

;; / to divide into subintervals

;; Finally, it should be possible to transduce IntervalSeqs


;; Reification

(extend-protocol t/ITimeReify
  Interval
  (on [i date] (interval (t/on (t/beginning i) date) (t/on (t/end i) date)))
  (in [i zone] (interval (t/in (t/beginning i) zone) (t/in (t/end i) zone))))

(defn bounds [& args]
  (interval
    (apply t/min (map t/beginning args))
    (apply t/max (map t/end args))))

(defn am [^LocalDate date]
  (interval (t/beginning date) (t/noon date)))

(defn pm [^LocalDate date]
  (interval (t/noon date) (t/end date)))

;; Allen's Basic Relations

(defn precedes? [x y]
  (t/< (t/end x) (t/beginning y)))

(defn equals? [x y]
  (and
    (= (t/beginning x) (t/beginning y))
    (= (t/end x) (t/end y))))

(defn meets? [x y]
  (= (t/end x) (t/beginning y)))

(defn overlaps? [x y]
  (and
   (t/< (t/beginning x) (t/beginning y))
   (t/> (t/end x) (t/beginning y))
   (t/< (t/end x) (t/end y))))

(defn during? [x y]
  (and
   (t/> (t/beginning x) (t/beginning y))
   (t/< (t/end x) (t/end y))))

(defn starts? [x y]
  (and
   (= (t/beginning x) (t/beginning y))
   (t/< (t/end x) (t/end y))))

(defn finishes? [x y]
  (and
   (t/> (t/beginning x) (t/beginning y))
   (= (t/end x) (t/end y))))

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

;; contains? is semantically similar to tick.core/coincident?
(defn contains? [x y] ((conv during?) x y))
(defn started-by? [x y] ((conv starts?) x y))

(def relation->kw {precedes? :precedes
           meets? :meets
           starts? :starts
           during? :during
           finishes? :finishes
           overlaps? :overlaps
           equals? :equals
           contains? :contains
           started-by? :started-by
           finished-by? :finished-by
           overlapped-by? :overlapped-by
           met-by? :met-by
           preceded-by? :preceded-by})

(def basic-relations
  [precedes? meets? overlaps? finished-by? contains?
   starts? equals? started-by? during? finishes? overlapped-by?
   met-by? preceded-by?])

;; Allen's General Relations

(defrecord GeneralRelation [relations]
  #?(:clj clojure.lang.IFn :cljs cljs.core/IFn)
  (#?(:clj invoke :cljs -invoke) [_ x y]
    (some (fn [f] (when (f x y) f)) relations)))

;; Relations are 'basic relations' in [ALSPAUGH-2009]. Invoking a
;; general relation on two intervals returns the basic relation that
;; causes the general relation to hold. Note there can only be one
;; such basic relation due to the relations being distinct.

(defn make-relation [& basic-relations]
  (->GeneralRelation basic-relations))

(def ^{:doc "A function to determine the (basic) relation between two intervals."}
  basic-relation
  (apply make-relation basic-relations))

(defn relation [x y]
  (relation->kw (basic-relation x y)))

;; Operations on relations

(defn complement-r
  "Return the complement of the general relation. The complement ~r of
  a relation r is the relation consisting of all basic relations not
  in r."
  [^GeneralRelation r]
  (assoc r :relations (remove (set (:relations r)) basic-relations)))

(defn not-yet-implemented []
  #?(:clj (new UnsupportedOperationException "Not yet implemented")
     :cljs (js/Error. "Not yet implemented")))

(defn compose-r
  "Return the composition of r and s"
  [r s]
  (throw (not-yet-implemented)))

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
  (throw (not-yet-implemented)))

;; Useful named general relations

(def disjoint? (make-relation precedes? preceded-by? meets? met-by?))
(def concur? (complement-r disjoint?))

;; Functions that make use of Allens' Interval Algebra

(defprotocol IIntervalOps
  ;; TODO: Rename? Not always narrow, sometimes widen, or generally modify - perhaps 'derive'
  (narrow [_ beginning end] "Narrow the interval to the new given bounds")
  (splice [ival1 ival2] "Splice two intervals together")
  (split [ival t] "Split ival into 2 intervals at t, returned as a vector"))

(defn split-with-assert [ival t]
  (assert
    (and (t/< (t/beginning ival) t)
         (t/< t (t/end ival))))
  (split ival t))

(extend-protocol IIntervalOps
  Interval
  (narrow [_ beginning end] (interval beginning end))
  (splice [ival1 ival2] (interval
                          (t/min (t/beginning ival1) (t/beginning ival2))
                          (t/max (t/end ival1) (t/end ival2))))
  (split [ival t]
    [(interval (t/beginning ival) t) (interval t (t/end ival))])

  LocalDate
  (narrow [date beginning end]
    (assert (t/<= (t/beginning date) beginning))
    (assert (t/>= (t/end date) end))
    (interval beginning end))
  (splice [ival1 ival2]
    (throw (ex-info "Not implemented" {:args [ival1 ival2]})))
  (split [ival t]
    [(interval (t/beginning ival) t) (interval t (t/end ival))])

  YearMonth
  (narrow [ym beginning end]
    (assert (t/<= (t/beginning ym) beginning))
    (assert (t/>= (t/end ym) end))
    (interval beginning end))
  (splice [ival1 ival2]
    (throw (ex-info "Not implemented" {:args [ival1 ival2]})))
  (split [ival t]
    [(interval (t/beginning ival) t) (interval t (t/end ival))])

  Year
  (narrow [yr beginning end]
    (assert (t/<= (t/beginning yr) beginning))
    (assert (t/>= (t/end yr) end))
    (interval beginning end))
  (splice [ival1 ival2]
    (throw (ex-info "Not implemented" {:args [ival1 ival2]})))
  (split [ival t]
    [(interval (t/beginning ival) t) (interval t (t/end ival))]))

(defn concur
  "Return the interval representing the interval, if there is one,
  representing the interval of time the given intervals are
  concurrent."
  [x y]
  (case (relation x y)
    :overlaps (narrow x (t/beginning y) (t/end x))
    :overlapped-by (narrow x (t/beginning x) (t/end y))
    (:starts :finishes :during :equals) x
    (:started-by :finished-by :contains) (narrow x (t/beginning y) (t/end y))
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
      {:x x :y y :relation (relation x y) :concur conc})))

;; Comparison. We have now built up the capability of comparing
;; intervals using the normal <, >, <=, >= operators.

(defn as-interval [t]
  (when (= (t/beginning t) (t/end t))
    (throw (ex-info "t is a zero length interval!" {:t t})))
  (interval (t/beginning t) (t/end t)))

(extend-protocol t/ITimeComparison
  LocalDate
  (< [x y] (t/< (as-interval x) (as-interval y)))
  (<= [x y] (t/<= (as-interval x) (as-interval y)))
  (> [x y] (t/> (as-interval x) (as-interval y)))
  (>= [x y] (t/>= (as-interval x) (as-interval y)))
  YearMonth
  (< [x y] (t/< (as-interval x) (as-interval y)))
  (<= [x y] (t/<= (as-interval x) (as-interval y)))
  (> [x y] (t/> (as-interval x) (as-interval y)))
  (>= [x y] (t/>= (as-interval x) (as-interval y)))
  Year
  (< [x y] (t/< (as-interval x) (as-interval y)))
  (<= [x y] (t/<= (as-interval x) (as-interval y)))
  (> [x y] (t/> (as-interval x) (as-interval y)))
  (>= [x y] (t/>= (as-interval x) (as-interval y)))
  Interval
  (< [x y] (#{precedes? meets?} (basic-relation x y)))
  (<= [x y] (#{precedes? meets? equals? starts? overlaps? finished-by?} (basic-relation x y)))
  (> [x y] (#{preceded-by? met-by?} (basic-relation x y)))
  (>= [x y] (#{preceded-by? met-by? equals? started-by? overlapped-by? finishes?} (basic-relation x y))))

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

;; If we ever want to create a wrapper type of ordered disjoint
;; interval sets...
#_(deftype OrderedDisjointIntervalSet [s]
  clojure.lang.Seqable
  (seq [_] (seq s)))

(defn union
  "Combine multiple collections of intervals into a single ordered
  collection of ordered disjoint intervals."
  [& colls]
  (loop [colls colls result []]
    (let [colls (remove nil? colls)]
      (if (<= (count colls) 1)
        (clojure.core/concat result (first colls))

        (let [[c1 c2 & r] (sort-by (comp t/beginning first) colls)]
          (if (disjoint? (first c1) (first c2))
            (recur (apply list (next c1) c2 r) (clojure.core/conj result (first c1)))

            (recur (apply list
                          (next c1)
                          (clojure.core/concat [(splice (first c1) (first c2))]
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
       (let [x (first xs)
             y (first ys)]
         (case (relation x y)
           (:precedes :meets) (recur (next xs) ys result)
           (:preceded-by :met-by) (recur xs (next ys) result)
           :started-by (recur
                (cons (narrow x (t/end y) (t/end x)) (next xs))
                (next ys)
                (clojure.core/conj result (narrow x (t/beginning y) (t/end y))))
           :finished-by (recur
                (next xs)
                (next ys)
                (clojure.core/conj result (narrow x (t/beginning y) (t/end y))))
           :overlaps (recur
                (cons (narrow x (t/beginning y) (t/end x)) (next xs))
                (cons (narrow y (t/end x) (t/end y)) (next ys))
                (clojure.core/conj result (narrow x (t/beginning y) (t/end x))))
           :overlapped-by (recur
                (cons (narrow x (t/end y) (t/end x)) (next xs))
                (next ys)
                (clojure.core/conj result (narrow x (t/beginning x) (t/end y))))
           :contains (recur
                (cons (narrow x (t/end y) (t/end x)) (next xs))
                (next ys)
                (clojure.core/conj result (narrow x (t/beginning y) (t/end y))))
           :during (recur
                (next xs)
                (cons (narrow y (t/end x) (t/end y)) (next ys))
                (clojure.core/conj result x))
           :equals (recur
                (next xs)
                (next ys)
                (clojure.core/conj result x))
           :finishes (recur
                (next xs)
                (next ys)
                (clojure.core/conj result x))
           :starts (recur
                (next xs)
                (cons (narrow y (t/end x) (t/end y))
                      (next ys))
                (clojure.core/conj result x))))
       result)))
  ([s1 s2 & sets]
   (reduce intersection s1 (clojure.core/conj sets s2))))

(defn intersects? [ivals interval]
  (not-empty (intersection ivals [interval])))

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
         (let [x (first xs) y (first ys)]
           (case (relation x y)
             (:precedes :meets) (recur (next xs) ys (clojure.core/conj result x))
             (:preceded-by :met-by) (recur xs (next ys) result)
             (:finishes :during :equals) (recur (next xs) (next ys) result)
             :starts (recur (next xs) ys result)
             (:started-by :overlapped-by)
             (recur (cons (narrow x (t/end y) (t/end x)) (next xs)) (next ys) result)
             :finished-by (recur (next xs) (next ys) (clojure.core/conj result (narrow x (t/beginning x) (t/beginning y))))
             :overlaps (recur (next xs) ys (clojure.core/conj result (narrow x (t/beginning x) (t/beginning y))))
             :contains (recur (cons (narrow x (t/end y) (t/end x)) (next xs))
                       (next ys)
                       (clojure.core/conj result (narrow x (t/beginning x) (t/beginning y))))))
         (apply clojure.core/conj result xs))
       result)))
  ([s1 s2 & sets]
   (reduce difference s1 (clojure.core/conj sets s2))))

(defn complement [coll]
  (if (empty? coll)
    [(interval (t/min-of-type (t/now)) (t/max-of-type (t/now)))]
    (let [r (map (fn [[x y]] (interval (t/end x) (t/beginning y)))
                 (partition 2 1 coll))]
      (cond-> r
        (not= (t/beginning (first coll)) (t/min-of-type (t/beginning (first coll))))
        (#(concat [(interval (t/min-of-type (t/beginning (first coll))) (t/beginning (first coll)))] %))
        (not= (t/end (last coll)) (t/max-of-type (t/end (last coll))))
        (#(concat % [(interval (t/end (last coll)) (t/max-of-type (t/end (last coll))))]))))))

(defn disjoin
  "Split s1 across the grating defined by s2"
  ([s1] s1)
  ([s1 s2]
   (loop [xs s1
          ys s2
          result []]
     (if (not-empty xs)
       (if (not-empty ys)
         (let [x (first xs) y (first ys)]
           (case (relation x y)
             (:precedes :meets) (recur (next xs) ys (clojure.core/conj result x))
;; TODO:
;;             (:preceded-by :met-by) (recur xs (next ys) result)
;;             (:finishes :during :equals) (recur (next xs) (next ys) result)
;;             :starts (recur (next xs) ys result)
;;             (:started-by :overlapped-by)
;;             (recur (cons (narrow x (t/end y) (t/end x)) (next xs)) (next ys) result)
;;             :finished-by (recur (next xs) (next ys) (clojure.core/conj result (narrow x (t/beginning x) (t/beginning y))))
;;            :overlaps (recur (next xs) ys (clojure.core/conj result (narrow x (t/beginning x) (t/beginning y))))
;;            :contains
             #_(recur (cons (narrow x (t/end y) (t/end x)) (next xs))
                    (next ys)
                    (clojure.core/conj result (narrow x (t/beginning x) (t/beginning y))))))
         (apply clojure.core/conj result xs))
       result)))
  ([s1 s2 & sets]
   (reduce difference s1 (clojure.core/conj sets s2))))

;; Division

(defn- divide-by-apply
  "Return a lazy sequence of java.time.Temporal instances over the
  given (local) interval."
  [ival f]
  (cond->
      (t/range
        (f (t/beginning ival))
        (f (t/end ival)))
    ;; Since range is exclusive, we must add one more value, but only
    ;; if it concurs rather than merely meets.
    (concur (f (t/end ival)) ival)
    (concat [(f (t/end ival))])))

(defn divide-by-duration
  "Divide an interval by a duration, returning a sequence of
  intervals. If the interval cannot be wholly sub-divided by the
  duration divisor, the last interval will represent the 'remainder'
  of the division and not be as long as the other preceeding
  intervals."
  [ival dur]
  (->> (t/range
         (t/beginning ival)
         (t/end ival)
         dur)
       ;; Bound by given interval, last will become a remainder.
       (map (juxt identity #(t/min (t/forward-duration % dur) (t/end ival))))))

(defn divide-by-period
  [ival period]
  (->> (t/range
         (t/beginning ival)
         (t/end ival)
         period)
       ;; Bound by given interval, last will become a remainder.
       (map (juxt identity #(t/min (t/forward-duration % period) (t/end ival))))))

(defn divide-by-divisor [ival divisor]
  (divide-by-duration ival (.dividedBy (t/duration ival) divisor)))

(defprotocol IDivisibleInterval
  (divide-interval [divisor ival] "Divide an interval by a given divisor"))

(extend-protocol IDivisibleInterval
  #?(:clj clojure.lang.Fn :cljs function)
  (divide-interval [f ival] (divide-by-apply ival f))
  Duration
  (divide-interval [dur ival] (divide-by-duration ival dur))
  Period
  (divide-interval [period ival] (divide-by-period ival period))
  #?(:clj Long :cljs number)
  (divide-interval [divisor ival] (divide-by-divisor ival divisor)))

;; TODO: hours-over, minutes-over, seconds-over, millis-over?,

(extend-protocol t/IDivisible
  LocalDate
  (divide [ld d] (divide-interval d ld))
  Year
  (divide [n d] (divide-interval d n))
  YearMonth
  (divide [n d] (divide-interval d n))
  Interval
  (divide [ival o] (divide-interval o ival)))

;; Grouping (similar to Division)

;; TODO: tag literals data_readers.clj for tick?
;; #tick/instant ""
;; #tick/local-date-time ""
;; #tick/local-date ""

;; TODO: Normalise - splice intervals that touch - opposite of disjoin

(defn group-by-intervals
  "Divide intervals in s1 by (disjoint ordered) intervals in s2,
  splitting if necessary, grouping by s2. Complexity is O(n) rather
  than O(n^2)"
  [intervals-to-group-by ivals]
  {:pre [(ordered-disjoint-intervals? intervals-to-group-by)
         (ordered-disjoint-intervals? ivals)]}
  (loop [intervals ivals
         groups intervals-to-group-by
         result {}
         current-intervals []]
    (if (not-empty intervals)
      (if (not-empty groups)
        (let [ival (first intervals)
              group (first groups)]

          (case (relation ival group)
            ;; If precedes or meets, discard ival
            (:precedes :meets)
            (recur (next intervals) groups result current-intervals)

            (:preceded-by :met-by)
            (recur
              intervals (next groups)
              (cond-> result
                (not-empty current-intervals)
                (assoc group current-intervals))
              [])

            :finishes
            (recur
              (next intervals)
              (next groups)
              (assoc result group (clojure.core/conj current-intervals ival))
              [])

            :equals
            (recur
              (next intervals)
              (next groups)
              (assoc result group (clojure.core/conj current-intervals ival))
              [])

            :finished-by
            (let [[seg1 seg2] (split-with-assert ival (t/beginning group))]
              (recur
                (next intervals)
                (next groups)
                (assoc result group (clojure.core/conj current-intervals seg2))
                []))

            :started-by
            (let [[seg1 seg2] (split-with-assert ival (t/end group))]
              (recur
                (cons seg2 (next intervals))
                (next groups)
                (assoc result group (clojure.core/conj current-intervals seg1))
                []))

            :overlapped-by
            (let [[seg1 seg2] (split-with-assert ival (t/end group))]
              (recur
                (cons seg2 (next intervals))
                (next groups)           ; end of this group
                (assoc result group (clojure.core/conj current-intervals seg1))
                []))

            (:starts :during)
            (recur
              (next intervals)
              groups
              result
              (clojure.core/conj current-intervals ival))

            (:contains)
            (recur
              (next intervals)
              (next groups)
              (assoc result group [(narrow ival (t/beginning group) (t/end group))])
              [])

            (:overlaps)
            (recur
              (next intervals)
              groups
              result
              (clojure.core/conj current-intervals (narrow ival (t/beginning group) (t/end ival))))))

        ;; No more groups
        result)

      ;; No more intervals
      (cond-> result
        (and (first groups) (not-empty current-intervals))
        (assoc (first groups) current-intervals)))))

(defprotocol IGroupable
  (group-by [grouping ivals]))

(extend-protocol IGroupable
  #?(:clj clojure.lang.Fn :cljs function)
  (group-by [f ivals]
    (if (empty? ivals)
      {}
      (let [r (apply bounds ivals)
            b (f (t/beginning r))
            e (f (t/end r))
            groups (t/range b (t/inc e))]
        (group-by groups ivals))))
  #?(:clj Iterable :cljs LazySeq)
  (group-by [groups ivals]
    (group-by-intervals groups ivals))
  #?(:cljs PersistentVector)
  #?(:cljs
     (group-by [groups ivals]
       (group-by-intervals groups ivals))))
