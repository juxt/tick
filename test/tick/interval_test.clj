;; Copyright © 2016-2017, JUXT LTD.

(ns tick.interval-test
  (:refer-clojure :exclude [contains? complement partition-by group-by disj])
  (:require
   [clojure.test :refer :all]
   [clojure.spec.alpha :as s]
   [tick.core :as t]
   [tick.cal :as cal]
   [tick.interval :refer :all])
  (:import [java.time.temporal ChronoUnit]))

(s/check-asserts true)

(deftest basic-relations-test
  (is (= (count basic-relations) 13))
  (is (distinct? basic-relations)))

;; We can construct every possible combination of interval relation with just 4 instants.
(def instants [(t/instant "2017-07-30T09:00:00Z")
               (t/instant "2017-07-30T11:00:00Z")
               (t/instant "2017-07-30T13:00:00Z")
               (t/instant "2017-07-30T15:00:00Z")])

;; Distinct: because no pair of definite intervals can be related by more than one of the relationships.
;; From [ALSPAUGH-2009]
(deftest distinct-test
  (is
   (= [1]   ; Each interval should have just one relation that is true
      (distinct
       (let [f (apply juxt basic-relations)]
         (for [x1 instants
               x2 instants
               y1 instants
               y2 instants
               :when (.isBefore x1 x2)
               :when (.isBefore y1 y2)
               :let [x [x1 x2]
                     y [y1 y2]]]
           ;; For each combination, count how many relations are true
           ;; (should be just one each time)
           (count (filter true? (f x y)))))))))

;; Exhaustive: because any pair of definite intervals are described by one of the relations.
(deftest exhaustive-test []
  (is
   (= 13 ; Thirteen basic relations
      (count
       (distinct
        (for [x1 instants
              x2 instants
              y1 instants
              y2 instants
              :when (.isBefore x1 x2)
              :when (.isBefore y1 y2)
              :let [x [x1 x2]
                    y [y1 y2]]]
          ;; For each combination, count how many relations are true
          ;; (should be just one each time)
          (code (relation x y))))))))


(deftest disjoint-test []
  (is (disjoint?
       [(instants 0) (instants 1)]
       [(instants 2) (instants 3)]))
  (is (= (disjoint?
          [(instants 0) (instants 1)]
          [(instants 2) (instants 3)]) precedes?))
  (is (nil?
       (disjoint?
        [(instants 0) (instants 2)]
        [(instants 1) (instants 3)])))
  (is (nil?
       (disjoint?
        [(instants 0) (instants 3)]
        [(instants 1) (instants 2)]))))

;; concur is really the complement to disjoint, but we'll test it
;; anywhere to ensure the complement function is working as expected.

(deftest concur-test []
  (is (nil?
       (concur?
        [(instants 0) (instants 1)]
        [(instants 2) (instants 3)])))
  (is (= (concur?
          [(instants 0) (instants 2)]
          [(instants 1) (instants 3)])
         overlaps?))
  (is (= (concur?
          [(instants 0) (instants 3)]
          [(instants 1) (instants 2)])
         contains?)))

(deftest intersection-test []
  (is
   (=
    (interval (instants 1) (instants 2))
    (intersection
     (interval (instants 0) (instants 2))
     (interval (instants 1) (instants 3)))))

  (is
   (=
    (interval (instants 1) (instants 2))
    (intersection
     (interval (instants 1) (instants 3))
     (interval (instants 0) (instants 2)))))

  (is
   (nil?
    (intersection
     (interval (instants 0) (instants 1))
     (interval (instants 2) (instants 3)))))

  (is
   (nil?
    (intersection
     (interval (instants 0) (instants 1))
     (interval (instants 1) (instants 2)))))

  (is
   (=
    (interval (instants 0) (instants 2))
    (intersection
     (interval (instants 0) (instants 2))
     (interval (instants 0) (instants 3)))))

  (is
   (=
    (interval (instants 0) (instants 2))
    (intersection
     (interval (instants 0) (instants 3))
     (interval (instants 0) (instants 2)))))

  (is
   (=
    (interval (instants 1) (instants 3))
    (intersection
     (interval (instants 1) (instants 3))
     (interval (instants 0) (instants 3))))))

;; Sequence tests

;; TODO: Support this: (interval (t/now) (t/seconds 10))
;; TODO: Don't allow this: (interval (t/now)) -- returns an illegal interval

(deftest ordered-disjoint-intervals?-test
  (is
   (ordered-disjoint-intervals? []))
  (is
   (ordered-disjoint-intervals?
    [(interval (t/instant "2017-07-30T09:00:00Z")
               (t/instant "2017-07-30T10:00:00Z"))]))
  (is
   (ordered-disjoint-intervals?
    [(interval (t/instant "2017-07-30T09:00:00Z")
               (t/instant "2017-07-30T10:00:00Z"))
     (interval (t/instant "2017-07-30T11:00:00Z")
               (t/instant "2017-07-30T13:00:00Z"))]))
  (is
   (ordered-disjoint-intervals?
    [(interval (t/instant "2017-07-30T09:00:00Z")
               (t/instant "2017-07-30T11:00:00Z"))
     (interval (t/instant "2017-07-30T11:00:00Z")
               (t/instant "2017-07-30T13:00:00Z"))]))
  (is
   (ordered-disjoint-intervals?
    [(interval (t/instant "2017-07-30T09:00:00Z")
               (t/instant "2017-07-30T11:00:00Z"))
     (interval (t/instant "2017-07-30T11:00:00Z")
               (t/instant "2017-07-30T13:00:00Z"))
     (interval (t/instant "2017-07-30T16:00:00Z")
               (t/instant "2017-07-30T18:00:00Z"))]))
  (is
   (false?
    (ordered-disjoint-intervals?
     [(interval (t/instant "2017-07-30T09:00:00Z")
                (t/instant "2017-07-30T12:00:00Z"))
      (interval (t/instant "2017-07-30T11:00:00Z")
                (t/instant "2017-07-30T13:00:00Z"))])))

  (is
   (false?
    (ordered-disjoint-intervals?
     [(interval (t/instant "2017-07-30T11:00:00Z")
                (t/instant "2017-07-30T13:00:00Z"))
      (interval (t/instant "2017-07-30T09:00:00Z")
                (t/instant "2017-07-30T10:00:00Z"))]))))

(deftest disj-test
  (let [coll1 [(interval (t/instant "2017-01-01T06:00:00Z")
                         (t/instant "2017-01-01T07:00:00Z"))

               (interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T09:00:00Z"))

               (interval (t/instant "2017-01-01T09:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))

               (interval (t/instant "2017-01-01T13:00:00Z")
                         (t/instant "2017-01-01T15:00:00Z"))

               (interval (t/instant "2017-01-01T17:00:00Z")
                         (t/instant "2017-01-01T19:00:00Z"))]

        coll2 [(interval (t/instant "2017-01-01T09:00:00Z")
                         (t/instant "2017-01-01T10:00:00Z"))

               (interval (t/instant "2017-01-01T11:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))

               (interval (t/instant "2017-01-01T14:00:00Z")
                         (t/instant "2017-01-01T18:00:00Z"))]]
    (is
     (= [[(t/instant "2017-01-01T06:00:00Z") (t/instant "2017-01-01T07:00:00Z")]
         [(t/instant "2017-01-01T08:00:00Z") (t/instant "2017-01-01T09:00:00Z")]
         [(t/instant "2017-01-01T10:00:00Z") (t/instant "2017-01-01T11:00:00Z")]
         [(t/instant "2017-01-01T13:00:00Z") (t/instant "2017-01-01T14:00:00Z")]
         [(t/instant "2017-01-01T18:00:00Z") (t/instant "2017-01-01T19:00:00Z")]]
        (disj coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T14:00:00Z")
                         (t/instant "2017-01-01T16:00:00Z"))]

        coll2 [(interval (t/instant "2017-01-01T09:00:00Z")
                         (t/instant "2017-01-01T11:00:00Z"))
               (interval (t/instant "2017-01-01T13:00:00Z")
                         (t/instant "2017-01-01T17:00:00Z"))]]

    (is
     (= [[(t/instant "2017-01-01T08:00:00Z")
          (t/instant "2017-01-01T09:00:00Z")]
         [(t/instant "2017-01-01T11:00:00Z")
          (t/instant "2017-01-01T12:00:00Z")]]
        (disj coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T14:00:00Z")
                         (t/instant "2017-01-01T16:00:00Z"))]
        coll2 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))]]
    (is
     (=
      [[(t/instant "2017-01-01T14:00:00Z")
        (t/instant "2017-01-01T16:00:00Z")]]
      (disj coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T17:00:00Z")
                         (t/instant "2017-01-01T19:00:00Z"))]


        coll2 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T18:00:00Z"))]]

    (is (=
         [[(t/instant "2017-01-01T18:00:00Z")
           (t/instant "2017-01-01T19:00:00Z")]]
         (disj coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T12:00:00Z")
                         (t/instant "2017-01-01T14:00:00Z"))]
        coll2 [(interval (t/instant "2017-01-01T11:00:00Z")
                         (t/instant "2017-01-01T14:00:00Z"))]]
    (is (empty? (disj coll1 coll2))))

  (is (= [(interval "2017-07-31" "2017-08-13")]
         (disj
          [(interval "2017-07-31" "2017-08-13")]
          [(interval "2017-01-01")]
          ))))

(deftest combine-test
  (let [coll1 [(interval (t/instant "2017-07-30T09:00:00Z")
                         (t/instant "2017-07-30T12:00:00Z"))]
        coll2 [(interval (t/instant "2017-07-30T11:00:00Z")
                         (t/instant "2017-07-30T15:00:00Z"))]
        coll3 [(interval (t/instant "2017-07-30T17:00:00Z")
                         (t/instant "2017-07-30T19:00:00Z"))]]
    (is (= 1 (count (combine coll1 coll2))))
    (is (ordered-disjoint-intervals? (combine coll1 coll2)))
    (is (= 2 (count (combine coll1 coll2 coll3))))
    (is (ordered-disjoint-intervals? (combine coll1 coll2 coll3))))

  (let [year (t/year 2017)
        holidays (map (comp interval :date) (cal/holidays-in-england-and-wales year))
        weekends (map interval (filter cal/weekend? (dates-over (interval year))))]
    (is (ordered-disjoint-intervals? holidays))
    (is (ordered-disjoint-intervals? weekends))
    (let [both (combine holidays weekends)]
      (is (ordered-disjoint-intervals? both))
      (is (= 113 (count both)))
      (is (= 365 (count (dates-over (interval (t/year 2017))))))
      ;; 252 is the number of working days in 2017 in England & Wales
      (is (= 252 (- 365 113))))))

;; Calc working days by disj of all days with holidays

#_(/
 (.getSeconds
  (apply t/+
         (map duration
              (let [year (t/year 2017)
                    holidays (map (comp interval :date) (cal/holidays-in-england-and-wales year))
                    weekends (map interval (filter cal/weekend? (dates-over (interval year))))]
                (disj
                 [(interval "2017-07-31" "2017-08-13") (interval "2017-04-14" "2017-04-19")]
                 (combine holidays weekends)
                 )
                ))))
 (* 60 60 24)
 )


#_(/
 (.getSeconds
  (reduce t/+
          (map duration
               (let [year (t/year 2017)
                     holidays (map (comp interval :date) (cal/holidays-in-england-and-wales year))
                     weekends (map interval (filter cal/weekend? (dates-over (interval year))))]
                 (disj
                  ;; Booked holidays
                  [(interval "2017-07-31" "2017-08-13")
                   (interval "2017-04-14" "2017-04-19")]
                  (combine holidays weekends)
                  )
                 ))))
 (* 60 60 24))
