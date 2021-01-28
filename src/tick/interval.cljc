;; Copyright © 2016-2017, JUXT LTD.

(ns tick.interval
  (:refer-clojure :exclude [contains? complement partition-by group-by conj extend divide flatten])
  (:require
    [clojure.set :as set]
    [tick.core :as t]
    [tick.protocols :as p]
    [cljc.java-time.duration]
    #?@(:cljs
         [[java.time :refer [ Instant Duration Period ZonedDateTime LocalTime
                             LocalDateTime LocalDate Year YearMonth OffsetDateTime ]]]))
  #?(:clj
     (:import
       [java.util Date]
       [java.time Instant Duration Period LocalDate LocalTime LocalDateTime Year YearMonth OffsetDateTime ZonedDateTime])))

;; Use of Allen's Interval Algebra, inspired from a working
;; demonstration of time-count by Eric Evans.

;; Construction

(defn- make-interval [beginning end]
  (assert (t/< beginning end))
  {:tick/beginning beginning
   :tick/end end})

(defprotocol ITimeSpanable
  (temporal-value [_] "Return a value of a type that satisfies t/ITimeSpan"))

(extend-protocol ITimeSpanable
  #?(:clj clojure.lang.Fn :cljs function)
  (temporal-value [f] (temporal-value (f)))

  Instant
  (temporal-value [i] i)

  #?(:clj String :cljs string)
  (temporal-value [s] (temporal-value (t/parse s)))

  LocalDate
  (temporal-value [d] d)

  LocalTime
  (temporal-value [t] t)

  LocalDateTime
  (temporal-value [ldt] ldt)

 #?(:clj Date :cljs js/Date)
  (temporal-value [d] (t/instant d))

  YearMonth
  (temporal-value [ym] ym)

  Year
  (temporal-value [y] y)

  OffsetDateTime
  (temporal-value [odt] odt)

  ZonedDateTime
  (temporal-value [zdt] zdt))

(defn new-interval [v1 v2]
  (let [t1 (t/beginning (temporal-value v1))
        t2 (t/end (temporal-value v2))]
    (if (t/< t1 t2)
      {:tick/beginning t1
       :tick/end t2}
      (throw
        (ex-info
          "Interval must span between two times, the first must be before the second"
          {:tick/beginning v1 :tick/end v2})))))

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
    (t/forward-duration (t/beginning ival) (cljc.java-time.duration/multiplied-by (t/duration ival) factor))))

(extend-protocol t/ITimeShift
  ;todo - impl for cljs.core.PersistentHashMap
  #?(:clj clojure.lang.IPersistentMap :cljs PersistentArrayMap)
  (forward-duration [ival d]
    (-> ival
        (update :tick/beginning #(t/forward-duration % d))
        (update :tick/end #(t/forward-duration % d))))
  (backward-duration [ival d]
    (-> ival
        (update :tick/beginning #(t/backward-duration % d))
        (update :tick/end #(t/backward-duration % d)))))

;; An interval of duration d to t1 can be constructed like this:
;; (scale (new-interval t1 d) -1)

;; (>> _ d) to shift the interval into the future by duration d
;; (<< _ d) to shift the interval into the past by duration d

;; (* _) to duplicate into meeting intervals, into a sequence (possibly need a record container for this, IntervalSeq)

;; (interpose IntervalSeq d)

;; / to divide into subintervals

;; Finally, it should be possible to transduce IntervalSeqs


;; Reification

(extend-protocol p/ITimeReify
  ;todo - impl for cljs.core.PersistentHashMap
  #?(:clj clojure.lang.IPersistentMap :cljs PersistentArrayMap)
  (on [i date] (new-interval (p/on (t/beginning i) date) (p/on (t/end i) date)))
  (in [i zone] (new-interval (p/in (t/beginning i) zone) (p/in (t/end i) zone))))

(defn bounds [& args]
  (make-interval
    (apply t/min (map t/beginning args))
    (apply t/max (map t/end args))))

(defn am [^LocalDate date]
  (new-interval (t/beginning date) (t/noon date)))

(defn pm [^LocalDate date]
  (new-interval (t/noon date) (t/end date)))

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

(def relation->kw
  {precedes? :precedes
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

(defn new-relation [& basic-relations]
  (->GeneralRelation basic-relations))

(def ^{:doc "A function to determine the (basic) relation between two intervals."}
  basic-relation
  (apply new-relation basic-relations))

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
  [_r _s]
  (throw (not-yet-implemented)))

(defn converse-r
  "Return the converse of the given general relation. The converse !r
  of a relation r is the relation consisting of the converses of all
  basic relations in r."
  [^GeneralRelation r]
  (assoc r :relations (map conv (:relations r))))

(defn intersection-r
  "Return the intersection of the r with s"
  [^GeneralRelation r ^GeneralRelation _s]
  (assert (instance? GeneralRelation r))
  (->GeneralRelation (set/intersection (set (:relations r))))
  (throw (not-yet-implemented)))

;; Useful named general relations

(def disjoint? (new-relation precedes? preceded-by? meets? met-by?))
(def concur? (complement-r disjoint?))
(def precedes-or-meets? (new-relation precedes? meets?))

;; Functions that make use of Allens' Interval Algebra

(defprotocol IIntervalOps
  (slice [this beginning end] "Fit the interval between beginning and end, slicing off one or both ends as necessary")
  (splice [this ival] "Splice another interval on to this one")
  (split [this t] "Split ival into 2 intervals at t, returned as a 2-element vector"))

(defn split-with-assert [ival t]
  (assert
    (and (t/< (t/beginning ival) t)
         (t/< t (t/end ival))))
  (split ival t))

(defn slice-interval [ival beginning end]
  (let [beginning (t/max (t/beginning ival) beginning)
        end (t/min (t/end ival) end)]
    (when (t/< beginning end)
      (if (associative? ival)
        (assoc ival :tick/beginning beginning :tick/end end)
        (make-interval beginning end)))))

(defn split-interval [ival t]
  [(slice-interval ival (t/beginning ival) t)
   (slice-interval ival t (t/end ival))])

;; Maps either represent single intervals, having :tick/beginning and
;; :tick/end, or with a :tick/intervals entry containing groups of
;; time-ordered disjoint intervals. Support groups using plain maps
;; helps preserve data in both maps being spliced together.
;;
;; Recursive structures not yet possible.

;; I'm unhappy that I haven't been able to write a recursive
;; implementation of flatten.
(defn flatten [s]
  (mapcat
    (fn [x]
      (if-let [ivals (:tick/intervals x)]
        ivals [x]))
    s))

(extend-protocol IIntervalOps
  ;todo - impl for cljs.core.PersistentHashMap
  #?(:clj clojure.lang.IPersistentMap :cljs PersistentArrayMap)
  (slice [this beginning end]
    (if-let [intervals (:tick/intervals this)]
      (assoc this :tick/intervals (vec (keep #(slice % beginning end) intervals)))
      (slice-interval this beginning end)))
  (splice [this other]
    (let [this-intervals (:tick/intervals this)
          other-intervals (:tick/intervals other)]
      (cond
        (and this-intervals other-intervals)
        (update this :tick/intervals concat other-intervals)
        this-intervals
        (update this :tick/intervals clojure.core/conj other)
        other-intervals
        (update other :tick/intervals clojure.core/conj this)
        :else
        {:tick/intervals [this other]})))
  (split [this t]
    (if-let [intervals (:tick/intervals this)]
      [(assoc this :tick/intervals
              (vec (keep #(slice % (t/beginning this) t) intervals)))
       (assoc this :tick/intervals
              (vec (keep #(slice % t (t/end this)) intervals)))]
      (split-interval this t)))

  LocalDate
  (slice [this beginning end]
    (slice-interval this beginning end))
  (splice [this ival]
    (throw (ex-info "splice not implemented" {:this this :interval ival})))
  (split [this t]
    (split-interval this t))

  YearMonth
  (slice [this beginning end]
    (slice-interval this beginning end))
  (splice [this ival]
    (throw (ex-info "splice not implemented" {:this this :interval ival})))
  (split [this t]
    (split-interval this t))

  Year
  (slice [this beginning end]
    (slice-interval this beginning end))
  (splice [this ival]
    (throw (ex-info "splice not implemented" {:this this :interval ival})))
  (split [this t]
    (split-interval this t)))

(defn concur
  "Return the interval representing the interval, if there is one,
  representing the interval of time the given intervals are
  concurrent."
  [x y]
  (case (relation x y)
    :overlaps (slice x (t/beginning y) (t/end x))
    :overlapped-by (slice x (t/beginning x) (t/end y))
    (:starts :finishes :during :equals) x
    (:started-by :finished-by :contains) (slice x (t/beginning y) (t/end y))
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

(defn interval [t]
  (new-interval (t/beginning t) (t/end t)))

(extend-protocol t/ITimeComparison
  ;todo - impl for cljs.core.PersistentHashMap
  #?(:clj clojure.lang.IPersistentMap :cljs PersistentArrayMap)
  (< [x y] (#{precedes? meets?} (basic-relation x y)))
  (<= [x y] (#{precedes? meets? equals? starts? overlaps? finished-by?} (basic-relation x y)))
  (> [x y] (#{preceded-by? met-by?} (basic-relation x y)))
  (>= [x y] (#{preceded-by? met-by? equals? started-by? overlapped-by? finishes?} (basic-relation x y))))

;; Interval sequences - time-ordered sequences of disjoint intervals

(defn ordered-disjoint-intervals?
  "Are all the intervals in the given set time-ordered and
  disjoint? This is a useful property of a collection of
  intervals. The given collection must contain proper intervals (that
  is, intervals that have finite greater-than-zero durations)."
  [s]
  (let [rel (new-relation precedes? meets?)]
    (some?
     (loop [[x & xs] s]
       (if (or (nil? x) (nil? (first xs))) true
           (when (rel x (first xs))
             (recur xs)))))))

(defn- assert-proper-head
  "Is the first interval in a sequence time-ordered and disjoint with
  respect to the second? Note, only compares first two in a
  sequence. Used by functions to ensure the head of the (possibly
  lazy) sequence satisfies this invariant."
  [s]
  (let [[initial subsequent] s]
    (when (and (nil? initial) subsequent)
      (throw (ex-info "Unexpected nil in sequence" {:nil-before subsequent})))
    (when subsequent
      (when-not (precedes-or-meets? initial subsequent)
        (throw
          (ex-info
            "Intervals in sequence violate requirement that intervals are time-ordered and disjoint"
            {:interval1 initial
             :interval2 subsequent}))))
    s))

(defn unite
  "Unite concurrent intervals. Intervals must be ordered by beginning
  but not necessarily disjoint (the purpose of this function is to
  splice together intervals that are concurrent resulting in a
  time-ordered sequence of disjoint intervals that is returned."
  [intervals]
  (letfn [(unite [intervals]
            (lazy-seq
              (let [[ival1 ival2 & r] intervals]
                (cond
                  (nil? ival2) (if ival1 (list ival1) (list))
                  :else
                  (case (relation ival1 ival2)
                    (:precedes :meets)
                    (cons ival1 (unite (rest intervals)))
                    (:overlaps :contains :starts :started-by :finished-by)
                    (unite (cons (splice ival1 ival2) r))
                    (throw
                      (ex-info "Intervals in sequence violate requirement that intervals are time-ordered" {:interval1 ival1
                                                                                                            :interval2 ival2
                                                                                                            :relation (relation ival1 ival2)})))))))]
    (unite intervals)))

(defn new-interval-group
  "Return an interval group. Interval groups are maps with
  a :tick/intervals entry that contain a time-ordered sequence of
  disjoint intervals."
  [x]
  (if (:tick/intervals x)
    x
    {:tick/intervals [x]}))

(defn normalize
  "Within a time-ordered sequence of disjoint intervals, return a
  sequence of interval groups, splicing together meeting intervals."
  [intervals]
  (letfn [(normalize [intervals]
            (lazy-seq
              (let [[ival1 ival2 & r] intervals]
                (if (nil? ival2) (if ival1 (list (new-interval-group ival1)) (list))
                    (case (relation ival1 ival2)
                      :meets (normalize (cons (splice ival1 ival2) r))
                      (cons (new-interval-group ival1)
                            (normalize (assert-proper-head (rest intervals)))))))))]
    (normalize (assert-proper-head intervals))))

(defn union
  "Merge multiple time-ordered sequences of disjoint intervals into a
  single sequence of time-ordered disjoint intervals."
  [& colls]
  (letfn [(union [colls]
            (lazy-seq
              (if (<= (count colls) 1)
                (first colls)
                (let [[c1 c2 & r] (sort-by #(t/beginning (first %)) (remove nil? colls))]
                  (if (nil? c2)
                    c1
                    (if (disjoint? (first c1) (first c2))
                      (cons (first c1) (union (apply list (next c1) c2 r)))
                      (union (apply list
                                    (cons (splice (first c1) (first c2))
                                          (next c1))
                                    (next c2)
                                    r))))))))]
    (union (for [coll colls :when coll] (sort-by t/beginning coll)))))

(defn conj [coll interval]
  (union coll [interval]))

(defn intersection
  "Return a time-ordered sequence of disjoint intervals where two or
  more intervals of the given sequences are concurrent. Arguments must
  be time-ordered sequences of disjoint intervals."
  ;; Single arity
  ([s1] s1)
  ;; 2-arity
  ([s1 s2]
   (letfn
       [(intersection [xs ys]
          (lazy-seq
            (let [x (first xs)
                  y (first ys)]
              (if (and x y)
                (case (relation x y)
                  (:precedes :meets)
                  (intersection (assert-proper-head (next xs)) ys)

                  (:preceded-by :met-by)
                  (intersection xs (assert-proper-head (next ys)))

                  :started-by
                  (cons (slice x (t/beginning y) (t/end y))
                        (intersection
                          (assert-proper-head (cons (slice x (t/end y) (t/end x)) (next xs)))
                          (assert-proper-head (next ys))))

                  :finished-by
                  (cons (slice x (t/beginning y) (t/end y))
                        (intersection
                          (assert-proper-head (next xs))
                          (assert-proper-head (next ys))))

                  :overlaps
                  (cons (slice x (t/beginning y) (t/end x))
                        (intersection
                          (assert-proper-head (cons (slice x (t/beginning y) (t/end x)) (next xs)))
                          (assert-proper-head (cons (slice y (t/end x) (t/end y)) (next ys)))))


                  :overlapped-by
                  (cons (slice x (t/beginning x) (t/end y))
                        (intersection
                          (assert-proper-head (cons (slice x (t/end y) (t/end x)) (next xs)))
                          (assert-proper-head (next ys))))

                  :contains
                  (cons (slice x (t/beginning y) (t/end y))
                        (intersection
                          (assert-proper-head (cons (slice x (t/end y) (t/end x)) (next xs)))
                          (assert-proper-head (next ys))))

                  :during
                  (cons x
                        (intersection
                          (assert-proper-head (next xs))
                          (assert-proper-head (cons (slice y (t/end x) (t/end y)) (next ys)))))

                  :equals
                  (cons x
                        (intersection
                          (assert-proper-head (next xs))
                          (assert-proper-head (next ys))))

                  :finishes
                  (cons x
                        (intersection
                          (assert-proper-head (next xs))
                          (assert-proper-head (next ys))))

                  :starts
                  (cons x
                        (intersection
                          (assert-proper-head (next xs))
                          (assert-proper-head (cons (slice y (t/end x) (t/end y))
                                                    (next ys))))))

                ;; List of nothing because one of the collections is
                ;; empty, so the intersection must be empty too.
                (list)))))]

       (intersection
         (assert-proper-head s1)
         (assert-proper-head s2))))

  ([s1 s2 & sets]
   (reduce intersection s1 (clojure.core/conj sets s2))))

(defn intersects? [ivals interval]
  (not-empty (intersection ivals [interval])))

(defn difference
  "Return an interval set that is the first set without elements of
  the remaining sets."
  ([s1] s1)
  ([s1 s2]
   (letfn [(difference [xs ys]
             (let [[x] xs [y] ys]
               (if x
                 (if y
                   (lazy-seq
                     (case (relation x y)
                       (:precedes :meets)
                       (cons x (difference (assert-proper-head (next xs)) ys))

                       (:preceded-by :met-by)
                       (difference xs (assert-proper-head (next ys)))

                       (:finishes :during :equals)
                       (difference
                         (assert-proper-head (next xs))
                         (assert-proper-head ys))

                       :starts
                       (difference
                         (assert-proper-head (next xs))
                         ys)

                       (:started-by :overlapped-by)
                       (difference
                         (assert-proper-head
                           (cons (slice x (t/end y) (t/end x)) (next xs)))
                         (assert-proper-head (next ys)))

                       :finished-by
                       (cons (slice x (t/beginning x) (t/beginning y))
                             (difference
                               (assert-proper-head (next xs))
                               (assert-proper-head (next ys))))

                       :overlaps
                       (cons (slice x (t/beginning x) (t/beginning y))
                             (difference
                               (assert-proper-head (next xs))
                               ys))

                       :contains
                       (cons (slice x (t/beginning x) (t/beginning y))
                             (difference
                               (assert-proper-head
                                 (cons (slice x (t/end y) (t/end x)) (next xs)))
                               (assert-proper-head (next ys))))))
                   ;; If xs but no ys
                   xs)
                 ;; If no xs or ys
                 (list))))]

     (assert-proper-head s1)
     (assert-proper-head s2)

     (difference s1 s2)))

  ([s1 s2 & sets]
   (reduce difference s1 (clojure.core/conj sets s2))))

(defn complement [coll]
  (if (empty? coll)
    [(new-interval (t/min-of-type (t/now)) (t/max-of-type (t/now)))]
    (let [r (->> coll
                 (partition 2 1)
                 (keep (fn [[x y]]
                         (when-not (meets? x y)
                           (new-interval (t/end x) (t/beginning y))))))]
      (cond-> r
        (not= (t/beginning (first coll)) (t/min-of-type (t/beginning (first coll))))
        (#(concat [(new-interval (t/min-of-type (t/beginning (first coll))) (t/beginning (first coll)))] %))
        (not= (t/end (last coll)) (t/max-of-type (t/end (last coll))))
        (#(concat % [(new-interval (t/end (last coll)) (t/max-of-type (t/end (last coll))))]))))))

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
;;             (recur (cons (slice x (t/end y) (t/end x)) (next xs)) (next ys) result)
;;             :finished-by (recur (next xs) (next ys) (clojure.core/conj result (slice x (t/beginning x) (t/beginning y))))
;;            :overlaps (recur (next xs) ys (clojure.core/conj result (slice x (t/beginning x) (t/beginning y))))
;;            :contains
             #_(recur (cons (slice x (t/end y) (t/end x)) (next xs))
                    (next ys)
                    (clojure.core/conj result (slice x (t/beginning x) (t/beginning y))))))
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
  (divide-by-duration ival (cljc.java-time.duration/divided-by (t/duration ival) divisor)))

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
  ;todo - impl for cljs.core.PersistentHashMap
  #?(:clj clojure.lang.IPersistentMap :cljs PersistentArrayMap)
  (divide [ival o] (divide-interval o ival)))

;; Grouping (similar to Division)

;; TODO: tag literals data_readers.clj for tick?
;; #tick/instant ""
;; #tick/local-date-time ""
;; #tick/local-date ""

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
            (let [[_seg1 seg2] (split-with-assert ival (t/beginning group))]
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
              (assoc result group [(slice ival (t/beginning group) (t/end group))])
              [])

            (:overlaps)
            (recur
              (next intervals)
              groups
              result
              (clojure.core/conj current-intervals (slice ival (t/beginning group) (t/end ival))))))

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
