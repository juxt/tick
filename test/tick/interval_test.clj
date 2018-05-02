;; Copyright © 2016-2017, JUXT LTD.

(ns tick.interval-test
  (:refer-clojure :exclude [contains? complement partition-by group-by conj extend group-by])
  (:require
   [clojure.test :refer :all]
   [clojure.spec.alpha :as s]
   [tick.core :as t]
   [tick.interval :refer :all])
  (:import
   [java.time.temporal ChronoUnit]))

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
               :when (t/< x1 x2)
               :when (t/< y1 y2)
               :let [x (interval x1 x2)
                     y (interval y1 y2)]]
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
              :when (t/< x1 x2)
              :when (t/< y1 y2)
              :let [x (interval x1 x2)
                    y (interval y1 y2)]]
          ;; For each combination, count how many relations are true
          ;; (should be just one each time)
          (relation x y)))))))


(deftest disjoint-test []
  (is (disjoint?
        (interval (instants 0) (instants 1))
        (interval (instants 2) (instants 3))))
  (is (= (disjoint?
           (interval (instants 0) (instants 1))
           (interval (instants 2) (instants 3))) precedes?))
  (is (nil?
       (disjoint?
         (interval (instants 0) (instants 2))
         (interval (instants 1) (instants 3)))))
  (is (nil?
       (disjoint?
         (interval (instants 0) (instants 3))
         (interval (instants 1) (instants 2))))))

;; concur is really the complement to disjoint, but we'll test it
;; anywhere to ensure the complement function is working as expected.

(deftest concur?-test []
  (is (nil?
       (concur?
         (interval (instants 0) (instants 1))
         (interval (instants 2) (instants 3)))))
  (is (= (concur?
           (interval (instants 0) (instants 2))
           (interval (instants 1) (instants 3)))
         overlaps?))
  (is (= (concur?
           (interval (instants 0) (instants 3))
           (interval (instants 1) (instants 2)))
         contains?)))

(deftest concur-test []
  (is
   (=
    (interval (instants 1) (instants 2))
    (concur
     (interval (instants 0) (instants 2))
     (interval (instants 1) (instants 3)))))

  (is
   (=
    (interval (instants 1) (instants 2))
    (concur
     (interval (instants 1) (instants 3))
     (interval (instants 0) (instants 2)))))

  (is
   (nil?
    (concur
     (interval (instants 0) (instants 1))
     (interval (instants 2) (instants 3)))))

  (is
   (nil?
    (concur
     (interval (instants 0) (instants 1))
     (interval (instants 1) (instants 2)))))

  (is
   (=
    (interval (instants 0) (instants 2))
    (concur
     (interval (instants 0) (instants 2))
     (interval (instants 0) (instants 3)))))

  (is
   (=
    (interval (instants 0) (instants 2))
    (concur
     (interval (instants 0) (instants 3))
     (interval (instants 0) (instants 2)))))

  (is
   (=
    (interval (instants 1) (instants 3))
    (concur
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

(deftest union-test
  (let [coll1 [(interval (t/instant "2017-07-30T09:00:00Z")
                         (t/instant "2017-07-30T12:00:00Z"))]
        coll2 [(interval (t/instant "2017-07-30T11:00:00Z")
                         (t/instant "2017-07-30T15:00:00Z"))]
        coll3 [(interval (t/instant "2017-07-30T17:00:00Z")
                         (t/instant "2017-07-30T19:00:00Z"))]]
    (is (= 1 (count (union coll1 coll2))))
    (is (ordered-disjoint-intervals? (union coll1 coll2)))
    (is (= 2 (count (union coll1 coll2 coll3))))
    (is (ordered-disjoint-intervals? (union coll1 coll2 coll3)))))

(deftest intersection-test
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
      (= [(interval (t/instant "2017-01-01T09:00:00Z") (t/instant "2017-01-01T10:00:00Z"))
          (interval (t/instant "2017-01-01T11:00:00Z") (t/instant "2017-01-01T12:00:00Z"))
          (interval (t/instant "2017-01-01T14:00:00Z") (t/instant "2017-01-01T15:00:00Z"))
          (interval (t/instant "2017-01-01T17:00:00Z") (t/instant "2017-01-01T18:00:00Z"))]
        (intersection coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T14:00:00Z")
                         (t/instant "2017-01-01T16:00:00Z"))]

        coll2 [(interval (t/instant "2017-01-01T09:00:00Z")
                         (t/instant "2017-01-01T11:00:00Z"))
               (interval (t/instant "2017-01-01T13:00:00Z")
                         (t/instant "2017-01-01T17:00:00Z"))]]

    (is
      (= [(interval (t/instant "2017-01-01T09:00:00Z") (t/instant "2017-01-01T11:00:00Z"))
          (interval (t/instant "2017-01-01T14:00:00Z") (t/instant "2017-01-01T16:00:00Z"))]
        (intersection coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T14:00:00Z")
                         (t/instant "2017-01-01T16:00:00Z"))]
        coll2 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))]]
    (is
     (=
       [(interval (t/instant "2017-01-01T08:00:00Z")
                  (t/instant "2017-01-01T12:00:00Z"))]
      (intersection coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T17:00:00Z")
                         (t/instant "2017-01-01T19:00:00Z"))]


        coll2 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T18:00:00Z"))]]

    (is (=
         [(interval (t/instant "2017-01-01T08:00:00Z")
                    (t/instant "2017-01-01T12:00:00Z"))
          (interval (t/instant "2017-01-01T17:00:00Z")
                    (t/instant "2017-01-01T18:00:00Z"))]
         (intersection coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T12:00:00Z")
                         (t/instant "2017-01-01T14:00:00Z"))]
        coll2 [(interval (t/instant "2017-01-01T11:00:00Z")
                         (t/instant "2017-01-01T14:00:00Z"))]]
    (is (= [(interval (t/instant "2017-01-01T12:00:00Z")
                      (t/instant "2017-01-01T14:00:00Z"))]
           (intersection coll1 coll2))))

  (let [coll1 [(interval (t/parse "2017-04-11T00:00")
                         (t/parse "2017-04-14T00:00"))
               (interval (t/parse "2017-04-18T00:00")
                         (t/parse "2017-04-20T00:00"))
               (interval (t/parse "2017-12-20T00:00")
                         (t/parse "2017-12-23T00:00"))
               (interval (t/parse "2017-12-27T00:00")
                         (t/parse "2018-01-01T00:00"))
               (interval (t/parse "2018-01-02T00:00")
                         (t/parse "2018-01-08T00:00"))]
        coll2 [(bounds "2017")]]
    (is (= [(interval (t/parse "2017-04-11T00:00")
                      (t/parse "2017-04-14T00:00"))
            (interval (t/parse "2017-04-18T00:00")
                      (t/parse "2017-04-20T00:00"))
            (interval (t/parse "2017-12-20T00:00")
                      (t/parse "2017-12-23T00:00"))
            (interval (t/parse "2017-12-27T00:00")
                      (t/parse "2018-01-01T00:00"))]
           (intersection coll1 coll2))))

  (let [coll1 [(interval (t/parse "2017-04-11T00:00")
                         (t/parse "2017-04-14T00:00"))
               (interval (t/parse "2017-04-18T00:00")
                         (t/parse "2017-04-20T00:00"))
               (interval (t/parse "2017-12-20T00:00")
                         (t/parse "2017-12-23T00:00"))
               (interval (t/parse "2017-12-27T00:00")
                         (t/parse "2018-01-01T00:00"))
               (interval (t/parse "2018-01-02T00:00")
                         (t/parse "2018-01-08T00:00"))]
        coll2 [(bounds "2017")]]
    (is (= [(interval (t/parse "2017-04-11T00:00")
                      (t/parse "2017-04-14T00:00"))
            (interval (t/parse "2017-04-18T00:00")
                      (t/parse "2017-04-20T00:00"))
            (interval (t/parse "2017-12-20T00:00")
                      (t/parse "2017-12-23T00:00"))
            (interval (t/parse "2017-12-27T00:00")
                      (t/parse "2018-01-01T00:00"))]
           (intersection coll1 coll2)))

    (testing "Empty sets"
      (let [coll1 []
            coll2 [(interval (t/instant "2017-01-01T09:00:00Z")
                             (t/instant "2017-01-01T10:00:00Z"))

                   (interval (t/instant "2017-01-01T11:00:00Z")
                             (t/instant "2017-01-01T12:00:00Z"))

                   (interval (t/instant "2017-01-01T14:00:00Z")
                             (t/instant "2017-01-01T18:00:00Z"))]]
        (is
         (= []
            (intersection coll1 coll2)))
        (is
         (= []
            (intersection coll2 coll1)))
        (is
         (= []
            (intersection [] [])))))))

(deftest difference-test
  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T14:00:00Z")
                         (t/instant "2017-01-01T16:00:00Z"))]

        coll2 [(interval (t/instant "2017-01-01T09:00:00Z")
                         (t/instant "2017-01-01T11:00:00Z"))
               (interval (t/instant "2017-01-01T13:00:00Z")
                         (t/instant "2017-01-01T17:00:00Z"))]]

    (is
      (= [(interval (t/instant "2017-01-01T08:00:00Z")
                    (t/instant "2017-01-01T09:00:00Z"))
          (interval (t/instant "2017-01-01T11:00:00Z")
                    (t/instant "2017-01-01T12:00:00Z"))]
         (difference coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T14:00:00Z")
                         (t/instant "2017-01-01T16:00:00Z"))]
        coll2 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))]]
    (is
      (=
        [(interval (t/instant "2017-01-01T14:00:00Z")
                   (t/instant "2017-01-01T16:00:00Z"))]
        (difference coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T12:00:00Z"))
               (interval (t/instant "2017-01-01T17:00:00Z")
                         (t/instant "2017-01-01T19:00:00Z"))]


        coll2 [(interval (t/instant "2017-01-01T08:00:00Z")
                         (t/instant "2017-01-01T18:00:00Z"))]]

    (is (=
          [(interval (t/instant "2017-01-01T18:00:00Z")
                     (t/instant "2017-01-01T19:00:00Z"))]
          (difference coll1 coll2))))

  (let [coll1 [(interval (t/instant "2017-01-01T12:00:00Z")
                         (t/instant "2017-01-01T14:00:00Z"))]
        coll2 [(interval (t/instant "2017-01-01T11:00:00Z")
                         (t/instant "2017-01-01T14:00:00Z"))]]
    (is (empty? (difference coll1 coll2))))

  (is (= [(bounds "2017-07-31" "2017-08-13")]
         (difference
           [(bounds "2017-07-31" "2017-08-13")]
           [(bounds "2017-01-01")])))

  (testing "Empty sets"
    (let [coll1 []
          coll2 [(interval (t/instant "2017-01-01T09:00:00Z")
                           (t/instant "2017-01-01T10:00:00Z"))

                 (interval (t/instant "2017-01-01T11:00:00Z")
                           (t/instant "2017-01-01T12:00:00Z"))

                 (interval (t/instant "2017-01-01T14:00:00Z")
                           (t/instant "2017-01-01T18:00:00Z"))]]
      (is
        (= []
           (difference coll1 coll2)))
      (is
        (= coll2
           (difference coll2 coll1)))
      (is
        (= []
           (difference [] []))))))

;; We are reclaiming 'disjoin' to mean to 'end the joining of' or 'to become disjoint'.

#_(deftest disj-test
  (is (=
        [(interval (t/local-date-time "2017-01-01T00:00")
                   (t/local-date-time "2017-07-04T00:00"))
         (interval (t/local-date-time "2017-07-05T00:00")
                   (t/local-date-time "2018-01-01T00:00"))]
       (disj [(bounds "2017")] (bounds (t/date "2017-07-04"))))))

#_(deftest complement-test
  ;; Not sure why this is failing on the equals check
  #_(is (=
       [[(tick.core/min-of-type (t/time "2017-01-01T00:00:00Z"))
         (t/time "2017-01-01T00:00:00Z")]

        [(t/time "2017-01-04T00:00:00Z")
         (t/time "2017-01-05T00:00:00Z")]

        [(t/time "2018-01-01T00:00:00Z")
         (tick.core/max-of-type (t/time "2017-01-01T00:00:00Z"))
         ]]

       (complement [[(t/time "2017-01-01T00:00:00Z")
                     (t/time "2017-07-04T00:00:00Z")]
                    [(t/time "2017-07-05T00:00:00Z")
                     (t/time "2018-01-01T00:00:00Z")]])))

  (is (= [] (complement (complement [])))))


;; Division test

(deftest division-test
  (is (= 7 (count (t/divide (bounds (t/year 2012) (t/year 2018)) t/year)))))

(deftest group-by-intervals-test
  (testing "p and s"
    (is
      (=
        {(t/year 2017) [(interval
                          (t/local-date-time "2017-12-20T00:00")
                          (t/local-date-time "2018-01-01T00:00"))]
         (t/year 2018) [(interval
                          (t/local-date-time "2018-01-01T00:00")
                          (t/local-date-time "2018-01-10T00:00"))]}
        (group-by
          (t/divide (bounds (t/year 2016) (t/year 2019)) t/year)
          [(interval (t/local-date-time #inst "2017-12-20")
                     (t/local-date-time #inst "2018-01-10"))]))))

  (testing "O"
    (is
      (=
        {(t/year 2015) [(bounds (t/year-month "2015-06") (t/year-month "2015-12"))]
         (t/year 2016) [(bounds (t/year 2016))]
         (t/year 2017) [(bounds (t/year-month "2017-01") (t/year-month "2017-06"))]}
        (group-by
          (t/divide (bounds (t/year 2014) (t/year 2018)) t/year)
          [(bounds (t/year-month "2015-06") (t/year-month "2017-06"))]))))

  (testing "M and e"
    (is
      (=
        {(t/year 2015) [(t/year 2015)]
         (t/year 2016) [(t/year 2016)]}
        (group-by
          (t/divide (bounds (t/year 2014) (t/year 2017)) t/year)
          (t/divide (bounds (t/year 2015) (t/year 2016)) t/year)))))

  (testing "s"
    (is (=
          {(t/year 2015) [(bounds (t/year 2015))]
           (t/year 2016) [(bounds (t/year 2016))]}
          (group-by
            (t/divide (bounds (t/year 2014) (t/year 2017)) t/year)
            [(bounds (t/year 2015) (t/year 2016))]))))

  (testing "f"
    (is
      (=
        {(t/year 2015) [(bounds (t/year-month "2015-06") (t/year-month "2015-12"))]}
        (group-by
          [(t/year 2014) (t/year 2015) (t/year 2016)]
          [(bounds (t/year-month "2015-06") (t/year-month "2015-12"))]))))

  (testing "F"
    (is
      (=
        {(bounds (t/year-month "2015-06") (t/year-month "2015-12"))
         [(bounds (t/year-month "2015-06") (t/year-month "2015-12"))]}
        (group-by
          [(bounds (t/year-month "2015-06") (t/year-month "2015-12"))]
          [(t/year 2014) (t/year 2015) (t/year 2016)]))))

  (testing "d"
    (is
      (=
        {(t/year 2015) [(bounds (t/year-month "2015-03") (t/year-month "2015-09"))]}
        (group-by
          [(t/year 2014) (t/year 2015) (t/year 2016)]
          [(bounds (t/year-month "2015-03") (t/year-month "2015-09"))]))))

  (testing "D"
    (is
      (=
        {(bounds (t/year-month "2015-03") (t/year-month "2015-09"))
         [(bounds (t/year-month "2015-03") (t/year-month "2015-09"))]}

        (group-by
          [(bounds (t/year-month "2015-03") (t/year-month "2015-09"))]
          [(t/year 2014) (t/year 2015) (t/year 2016)]))))

  (testing "o"
    (is
      (=
        {(bounds (t/year-month "2015-06") (t/year-month "2017-06"))
         [(bounds (t/year-month "2015-06") (t/year-month "2015-12"))
          (t/year "2016")
          (bounds (t/year-month "2017-01") (t/year-month "2017-06"))]}
        (group-by
          [(bounds (t/year-month "2015-06") (t/year-month "2017-06"))]
          (t/divide (bounds (t/year 2014) (t/year 2018)) t/year))))))

(deftest group-by-test
  (is
    (= 31
       (count
         (group-by
           t/date
           [(t/date "2015-05-20") (t/year-month "2015-06")])))))

(deftest group-by-empty-test
  (is (= {} (group-by t/date []))))
