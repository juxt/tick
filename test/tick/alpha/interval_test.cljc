;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.interval-test
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :as t]
   [tick.protocols :as p]
   [clojure.test
    :refer [deftest is testing run-tests]
    :refer-macros [deftest is testing run-tests]]
   [tick.alpha.interval :as ti]))

(s/check-asserts true)


(deftest date-relation-test
  (is (=
        (ti/relation
          (ti/new-interval
            (t/zoned-date-time "2021-02-24T00:00Z[GMT]")
            (t/zoned-date-time "2021-02-25T00:00Z[GMT]"))
          (ti/new-interval
            (t/zoned-date-time "2021-02-23T00:00Z[Europe/London]")
            (t/zoned-date-time "2021-02-24T00:00Z[Europe/London]")))
        :met-by)))


(deftest basic-relations-test
  (is (= (count ti/basic-relations) 13))
  (is (distinct? ti/basic-relations)))

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
         (let [f (apply juxt ti/basic-relations)]
           (for [x1 instants
                 x2 instants
                 y1 instants
                 y2 instants
                 :when (t/< x1 x2)
                 :when (t/< y1 y2)
                 :let [x (ti/new-interval x1 x2)
                       y (ti/new-interval y1 y2)]]
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
                 :let [x (ti/new-interval x1 x2)
                       y (ti/new-interval y1 y2)]]
             ;; For each combination, count how many relations are true
             ;; (should be just one each time)
             (ti/relation x y)))))))


(deftest disjoint-test []
  (is (ti/disjoint?
        (ti/new-interval (instants 0) (instants 1))
        (ti/new-interval (instants 2) (instants 3))))
  (is (= (ti/disjoint?
           (ti/new-interval (instants 0) (instants 1))
           (ti/new-interval (instants 2) (instants 3))) ti/precedes?))
  (is (nil?
        (ti/disjoint?
          (ti/new-interval (instants 0) (instants 2))
          (ti/new-interval (instants 1) (instants 3)))))
  (is (nil?
        (ti/disjoint?
          (ti/new-interval (instants 0) (instants 3))
          (ti/new-interval (instants 1) (instants 2))))))

;; concur is really the complement to disjoint, but we'll test it
;; anywhere to ensure the complement function is working as expected.

(deftest concur?-test []
  (is (nil?
        (ti/concur?
          (ti/new-interval (instants 0) (instants 1))
          (ti/new-interval (instants 2) (instants 3)))))
  (is (= (ti/concur?
           (ti/new-interval (instants 0) (instants 2))
           (ti/new-interval (instants 1) (instants 3)))
         ti/overlaps?))
  (is (= (ti/concur?
           (ti/new-interval (instants 0) (instants 3))
           (ti/new-interval (instants 1) (instants 2)))
         ti/contains?)))

(deftest concur-test []
  (is
    (=
      (ti/new-interval (instants 1) (instants 2))
      (ti/concur
        (ti/new-interval (instants 0) (instants 2))
        (ti/new-interval (instants 1) (instants 3)))))

  (is
    (=
      (ti/new-interval (instants 1) (instants 2))
      (ti/concur
        (ti/new-interval (instants 1) (instants 3))
        (ti/new-interval (instants 0) (instants 2)))))

  (is
    (nil?
      (ti/concur
        (ti/new-interval (instants 0) (instants 1))
        (ti/new-interval (instants 2) (instants 3)))))

  (is
    (nil?
      (ti/concur
        (ti/new-interval (instants 0) (instants 1))
        (ti/new-interval (instants 1) (instants 2)))))

  (is
    (=
      (ti/new-interval (instants 0) (instants 2))
      (ti/concur
        (ti/new-interval (instants 0) (instants 2))
        (ti/new-interval (instants 0) (instants 3)))))

  (is
    (=
      (ti/new-interval (instants 0) (instants 2))
      (ti/concur
        (ti/new-interval (instants 0) (instants 3))
        (ti/new-interval (instants 0) (instants 2)))))

  (is
    (=
      (ti/new-interval (instants 1) (instants 3))
      (ti/concur
        (ti/new-interval (instants 1) (instants 3))
        (ti/new-interval (instants 0) (instants 3))))))

;; Sequence tests

;; TODO: Support this: (ti/new-interval (t/now) (t/seconds 10))
;; TODO: Don't allow this: (ti/new-interval (t/now)) -- returns an illegal ti/new-interval

(deftest ordered-disjoint-intervals?-test
  (is
    (ti/ordered-disjoint-intervals? []))
  (is
    (ti/ordered-disjoint-intervals?
      [(ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                        (t/instant "2017-07-30T10:00:00Z"))]))
  (is
    (ti/ordered-disjoint-intervals?
      [(ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                        (t/instant "2017-07-30T10:00:00Z"))
       (ti/new-interval (t/instant "2017-07-30T11:00:00Z")
                        (t/instant "2017-07-30T13:00:00Z"))]))
  (is
    (ti/ordered-disjoint-intervals?
      [(ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                        (t/instant "2017-07-30T11:00:00Z"))
       (ti/new-interval (t/instant "2017-07-30T11:00:00Z")
                        (t/instant "2017-07-30T13:00:00Z"))]))
  (is
    (ti/ordered-disjoint-intervals?
      [(ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                        (t/instant "2017-07-30T11:00:00Z"))
       (ti/new-interval (t/instant "2017-07-30T11:00:00Z")
                        (t/instant "2017-07-30T13:00:00Z"))
       (ti/new-interval (t/instant "2017-07-30T16:00:00Z")
                        (t/instant "2017-07-30T18:00:00Z"))]))
  (is
    (false?
      (ti/ordered-disjoint-intervals?
        [(ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                          (t/instant "2017-07-30T12:00:00Z"))
         (ti/new-interval (t/instant "2017-07-30T11:00:00Z")
                          (t/instant "2017-07-30T13:00:00Z"))])))

  (is
    (false?
      (ti/ordered-disjoint-intervals?
        [(ti/new-interval (t/instant "2017-07-30T11:00:00Z")
                          (t/instant "2017-07-30T13:00:00Z"))
         (ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                          (t/instant "2017-07-30T10:00:00Z"))]))))

(deftest unite-test
  (testing "Unite meeting intervals"
    (is
      (=
        [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))
         (ti/new-interval (t/date "2017-06-26") (t/date "2017-06-30"))]
        (let [intervals
              [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))
               (ti/new-interval (t/date "2017-06-26") (t/date "2017-06-30"))]]
          (ti/unite intervals)))))
  (testing "Unite overlapping intervals"
    (is
      (=
        [(ti/new-interval (t/date "2017-06-10") (t/date "2017-06-25"))
         (ti/new-interval (t/date "2017-07-01") (t/date "2017-07-10"))]

        (map ti/bounds
             (ti/unite [(ti/new-interval (t/date "2017-06-10") (t/date "2017-06-20"))
                        (ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))
                        (ti/new-interval (t/date "2017-07-01") (t/date "2017-07-10"))])))))
  (testing "Unite containing intervals"
    (is
      (=
        [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))]
        (map ti/bounds
             (ti/unite [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))
                        (ti/new-interval (t/date "2017-06-17") (t/date "2017-06-20"))])))))

  (testing "Unite finished-by intervals"
    (is
      (=
        [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))]
        (map ti/bounds
             (ti/unite [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))
                        (ti/new-interval (t/date "2017-06-17") (t/date "2017-06-20"))
                        (ti/new-interval (t/date "2017-06-18") (t/date "2017-06-25"))]))))))

(deftest flatten-test
  (let [ivals [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))
               (ti/new-interval (t/date "2017-06-17") (t/date "2017-06-20"))
               (ti/new-interval (t/date "2017-06-18") (t/date "2017-06-25"))
               (ti/new-interval (t/date "2017-07-18") (t/date "2017-07-25"))
               (ti/new-interval (t/date "2017-08-18") (t/date "2017-08-25"))]]
    (is (= ivals (ti/flatten (ti/unite ivals))))))

(deftest normalize-test
  (let [intervals
        [(ti/new-interval (t/date "2017-06-15") (t/date "2017-06-25"))
         (ti/new-interval (t/date "2017-06-26") (t/date "2017-06-28"))
         (ti/new-interval (t/date "2017-06-30") (t/date "2017-07-04"))]]
    (is (= [{:tick/intervals
             [{:tick/beginning (t/date-time "2017-06-15T00:00")
               :tick/end (t/date-time "2017-06-26T00:00")}
              {:tick/beginning (t/date-time "2017-06-26T00:00")
               :tick/end (t/date-time "2017-06-29T00:00")}]}
            {:tick/intervals
             [{:tick/beginning (t/date-time "2017-06-30T00:00")
               :tick/end (t/date-time "2017-07-05T00:00")}]}]

           (ti/normalize intervals)))))

(deftest union-test
  (testing "counts"
    (let [coll1 [(ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                                  (t/instant "2017-07-30T12:00:00Z"))]
          coll2 [(ti/new-interval (t/instant "2017-07-30T11:00:00Z")
                                  (t/instant "2017-07-30T15:00:00Z"))]
          coll3 [(ti/new-interval (t/instant "2017-07-30T17:00:00Z")
                                  (t/instant "2017-07-30T19:00:00Z"))]]
      (is (= 1 (count (ti/union coll1 coll2))))
      (is (ti/ordered-disjoint-intervals? (ti/union coll1 coll2)))
      (is (= 2 (count (ti/union coll1 coll2 coll3))))
      (is (ti/ordered-disjoint-intervals? (ti/union coll1 coll2 coll3)))))

  (testing "union"
    (let [ival1 (ti/new-interval (t/instant "2017-07-30T09:00:00Z")
                                 (t/instant "2017-07-30T10:00:00Z"))
          ival2 (ti/new-interval (t/instant "2017-07-30T10:00:00Z")
                                 (t/instant "2017-07-30T11:00:00Z"))
          ival3 (ti/new-interval (t/instant "2017-07-30T11:00:00Z")
                                 (t/instant "2017-07-30T12:00:00Z"))
          ival4  (ti/new-interval (t/instant "2017-07-30T12:00:00Z")
                                  (t/instant "2017-07-30T13:00:00Z"))
          ival5 (ti/new-interval (t/instant "2017-07-30T13:00:00Z")
                                 (t/instant "2017-07-30T14:00:00Z"))
          res (ti/union [ival2 ival4] [ival1 ival3 ival5])]
      (is (= res [ival1 ival2 ival3 ival4 ival5])))))

(deftest intersection-test
  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T06:00:00Z")
                                (t/instant "2017-01-01T07:00:00Z"))

               (ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T09:00:00Z"))

               (ti/new-interval (t/instant "2017-01-01T09:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))

               (ti/new-interval (t/instant "2017-01-01T13:00:00Z")
                                (t/instant "2017-01-01T15:00:00Z"))

               (ti/new-interval (t/instant "2017-01-01T17:00:00Z")
                                (t/instant "2017-01-01T19:00:00Z"))]

        coll2 [(ti/new-interval (t/instant "2017-01-01T09:00:00Z")
                                (t/instant "2017-01-01T10:00:00Z"))

               (ti/new-interval (t/instant "2017-01-01T11:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))

               (ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                (t/instant "2017-01-01T18:00:00Z"))]]
    (is
      (= [(ti/new-interval (t/instant "2017-01-01T09:00:00Z") (t/instant "2017-01-01T10:00:00Z"))
          (ti/new-interval (t/instant "2017-01-01T11:00:00Z") (t/instant "2017-01-01T12:00:00Z"))
          (ti/new-interval (t/instant "2017-01-01T14:00:00Z") (t/instant "2017-01-01T15:00:00Z"))
          (ti/new-interval (t/instant "2017-01-01T17:00:00Z") (t/instant "2017-01-01T18:00:00Z"))]
         (ti/intersection coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                (t/instant "2017-01-01T16:00:00Z"))]

        coll2 [(ti/new-interval (t/instant "2017-01-01T09:00:00Z")
                                (t/instant "2017-01-01T11:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T13:00:00Z")
                                (t/instant "2017-01-01T17:00:00Z"))]]

    (is
      (= [(ti/new-interval (t/instant "2017-01-01T09:00:00Z") (t/instant "2017-01-01T11:00:00Z"))
          (ti/new-interval (t/instant "2017-01-01T14:00:00Z") (t/instant "2017-01-01T16:00:00Z"))]
         (ti/intersection coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                (t/instant "2017-01-01T16:00:00Z"))]
        coll2 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))]]
    (is
      (=
        [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                          (t/instant "2017-01-01T12:00:00Z"))]
        (ti/intersection coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T17:00:00Z")
                                (t/instant "2017-01-01T19:00:00Z"))]

        coll2 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T18:00:00Z"))]]

    (is (=
          [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                            (t/instant "2017-01-01T12:00:00Z"))
           (ti/new-interval (t/instant "2017-01-01T17:00:00Z")
                            (t/instant "2017-01-01T18:00:00Z"))]
          (ti/intersection coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T12:00:00Z")
                                (t/instant "2017-01-01T14:00:00Z"))]
        coll2 [(ti/new-interval (t/instant "2017-01-01T11:00:00Z")
                                (t/instant "2017-01-01T14:00:00Z"))]]
    (is (= [(ti/new-interval (t/instant "2017-01-01T12:00:00Z")
                             (t/instant "2017-01-01T14:00:00Z"))]
           (ti/intersection coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/date-time "2017-04-11T00:00")
                                (t/date-time "2017-04-14T00:00"))
               (ti/new-interval (t/date-time "2017-04-18T00:00")
                                (t/date-time "2017-04-20T00:00"))
               (ti/new-interval (t/date-time "2017-12-20T00:00")
                                (t/date-time "2017-12-23T00:00"))
               (ti/new-interval (t/date-time "2017-12-27T00:00")
                                (t/date-time "2018-01-01T00:00"))
               (ti/new-interval (t/date-time "2018-01-02T00:00")
                                (t/date-time "2018-01-08T00:00"))]
        coll2 [(ti/bounds (t/year "2017"))]]
    (is (= [(ti/new-interval (t/date-time "2017-04-11T00:00")
                             (t/date-time "2017-04-14T00:00"))
            (ti/new-interval (t/date-time "2017-04-18T00:00")
                             (t/date-time "2017-04-20T00:00"))
            (ti/new-interval (t/date-time "2017-12-20T00:00")
                             (t/date-time "2017-12-23T00:00"))
            (ti/new-interval (t/date-time "2017-12-27T00:00")
                             (t/date-time "2018-01-01T00:00"))]
           (ti/intersection coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/date-time "2017-04-11T00:00")
                                (t/date-time "2017-04-14T00:00"))
               (ti/new-interval (t/date-time "2017-04-18T00:00")
                                (t/date-time "2017-04-20T00:00"))
               (ti/new-interval (t/date-time "2017-12-20T00:00")
                                (t/date-time "2017-12-23T00:00"))
               (ti/new-interval (t/date-time "2017-12-27T00:00")
                                (t/date-time "2018-01-01T00:00"))
               (ti/new-interval (t/date-time "2018-01-02T00:00")
                                (t/date-time "2018-01-08T00:00"))]
        coll2 [(ti/bounds (t/year "2017"))]]
    (is (= [(ti/new-interval (t/date-time "2017-04-11T00:00")
                             (t/date-time "2017-04-14T00:00"))
            (ti/new-interval (t/date-time "2017-04-18T00:00")
                             (t/date-time "2017-04-20T00:00"))
            (ti/new-interval (t/date-time "2017-12-20T00:00")
                             (t/date-time "2017-12-23T00:00"))
            (ti/new-interval (t/date-time "2017-12-27T00:00")
                             (t/date-time "2018-01-01T00:00"))]
           (ti/intersection coll1 coll2)))

    (testing "Empty sets"
      (let [coll1 []
            coll2 [(ti/new-interval (t/instant "2017-01-01T09:00:00Z")
                                    (t/instant "2017-01-01T10:00:00Z"))

                   (ti/new-interval (t/instant "2017-01-01T11:00:00Z")
                                    (t/instant "2017-01-01T12:00:00Z"))

                   (ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                    (t/instant "2017-01-01T18:00:00Z"))]]
        (is
          (empty? (ti/intersection coll1 coll2)))
        (is
          (empty? (ti/intersection coll2 coll1)))
        (is
          (empty? (ti/intersection [] [])))))))

(deftest difference-test
  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                (t/instant "2017-01-01T16:00:00Z"))]

        coll2 [(ti/new-interval (t/instant "2017-01-01T09:00:00Z")
                                (t/instant "2017-01-01T11:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T13:00:00Z")
                                (t/instant "2017-01-01T17:00:00Z"))]]

    (is
      (= [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                           (t/instant "2017-01-01T09:00:00Z"))
          (ti/new-interval (t/instant "2017-01-01T11:00:00Z")
                           (t/instant "2017-01-01T12:00:00Z"))]
         (ti/difference coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                (t/instant "2017-01-01T16:00:00Z"))]
        coll2 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))]]
    (is
      (=
        [(ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                          (t/instant "2017-01-01T16:00:00Z"))]
        (ti/difference coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T17:00:00Z")
                                (t/instant "2017-01-01T19:00:00Z"))]


        coll2 [(ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T18:00:00Z"))]]

    (is (=
          [(ti/new-interval (t/instant "2017-01-01T18:00:00Z")
                            (t/instant "2017-01-01T19:00:00Z"))]
          (ti/difference coll1 coll2))))

  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T12:00:00Z")
                                (t/instant "2017-01-01T14:00:00Z"))]
        coll2 [(ti/new-interval (t/instant "2017-01-01T11:00:00Z")
                                (t/instant "2017-01-01T14:00:00Z"))]]
    (is (empty? (ti/difference coll1 coll2))))

  (is (= [(ti/bounds (t/date "2017-07-31") (t/date "2017-08-13"))]
         (ti/difference
           [(ti/bounds (t/date "2017-07-31") (t/date "2017-08-13"))]
           [(ti/bounds (t/date "2017-01-01"))])))

  (testing "Empty sets"
    (let [coll1 []
          coll2 [(ti/new-interval (t/instant "2017-01-01T09:00:00Z")
                                  (t/instant "2017-01-01T10:00:00Z"))

                 (ti/new-interval (t/instant "2017-01-01T11:00:00Z")
                                  (t/instant "2017-01-01T12:00:00Z"))

                 (ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                  (t/instant "2017-01-01T18:00:00Z"))]]
      (is
        (= []
           (ti/difference coll1 coll2)))
      (is
        (= coll2
           (ti/difference coll2 coll1)))
      (is
        (= []
           (ti/difference [] []))))))

(deftest difference-invariant-test
  (let [coll1 [(ti/new-interval (t/instant "2017-01-01T14:00:00Z")
                                (t/instant "2017-01-01T16:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T08:00:00Z")
                                (t/instant "2017-01-01T12:00:00Z"))]

        coll2 [(ti/new-interval (t/instant "2017-01-01T09:00:00Z")
                                (t/instant "2017-01-01T11:00:00Z"))
               (ti/new-interval (t/instant "2017-01-01T13:00:00Z")
                                (t/instant "2017-01-01T17:00:00Z"))]]
    (is
      (thrown?
        #?(:clj clojure.lang.ExceptionInfo
           :cljs ExceptionInfo)
        (ti/difference coll1 coll2)))))

;; We are reclaiming 'disjoin' to mean to 'end the joining of' or 'to become disjoint'.

#_(deftest disj-test
    (is (=
          [(ti/new-interval (t/date-time "2017-01-01T00:00")
                            (t/date-time "2017-07-04T00:00"))
           (ti/new-interval (t/date-time "2017-07-05T00:00")
                            (t/date-time "2018-01-01T00:00"))]
          (disj [(bounds "2017")] (bounds (t/date "2017-07-04"))))))

(deftest complement-test
  (testing "complement through max of type"
    (is (= [(ti/new-interval (t/time "01:00") (t/max-of-type (t/time "00:00")))]
           (ti/complement [(ti/new-interval (t/time "00:00") (t/time "01:00"))]))))
  (testing "complement ordered disjoint intervals"
    (is (= [(ti/new-interval (t/time "00:00") (t/time "01:00"))
            (ti/new-interval (t/time "02:00") (t/time "03:00"))
            (ti/new-interval (t/time "04:00") (t/max-of-type (t/time "00:00")))]
           (ti/complement [(ti/new-interval (t/time "01:00") (t/time "02:00"))
                        (ti/new-interval (t/time "03:00") (t/time "04:00"))]))))
  (testing "complement meeting intervals"
    (is (= [(ti/new-interval (t/time "00:00") (t/time "01:00"))
            (ti/new-interval (t/time "03:00") (t/max-of-type (t/time "00:00")))]
           (ti/complement [(ti/new-interval (t/time "01:00") (t/time "02:00"))
                        (ti/new-interval (t/time "02:00") (t/time "03:00"))]))))
  (testing "complement empty interval round trip"
    (is (= [] (ti/complement (ti/complement []))))))


;; Division test

(deftest division-test
  (is (= 7 (count (ti/divide (ti/bounds (t/year 2012) (t/year 2018)) t/year)))))

(deftest group-by-intervals-test
  (testing "p and s"
    (t/with-clock t/UTC
      (is
        (=
          {(t/year 2017) [(ti/new-interval
                            (t/date-time "2017-12-20T00:00")
                            (t/date-time "2018-01-01T00:00"))]
           (t/year 2018) [(ti/new-interval
                            (t/date-time "2018-01-01T00:00")
                            (t/date-time "2018-01-10T00:00"))]}
          (ti/group-by
            (ti/divide (ti/bounds (t/year 2016) (t/year 2019)) t/year)
            [(ti/new-interval (t/date-time #inst "2017-12-20")
               (t/date-time #inst "2018-01-10"))])))))

  (testing "O"
    (is
      (=
        {(t/year 2015) [(ti/bounds (t/year-month "2015-06") (t/year-month "2015-12"))]
         (t/year 2016) [(ti/bounds (t/year 2016))]
         (t/year 2017) [(ti/bounds (t/year-month "2017-01") (t/year-month "2017-06"))]}
        (ti/group-by
          (ti/divide (ti/bounds (t/year 2014) (t/year 2018)) t/year)
          [(ti/bounds (t/year-month "2015-06") (t/year-month "2017-06"))]))))

  (testing "M and e"
    (is
      (=
        {(t/year 2015) [(t/year 2015)]
         (t/year 2016) [(t/year 2016)]}
        (ti/group-by
          (ti/divide (ti/bounds (t/year 2014) (t/year 2017)) t/year)
          (ti/divide (ti/bounds (t/year 2015) (t/year 2016)) t/year)))))

  (testing "s"
    (is (=
          {(t/year 2015) [(ti/bounds (t/year 2015))]
           (t/year 2016) [(ti/bounds (t/year 2016))]}
          (ti/group-by
            (ti/divide (ti/bounds (t/year 2014) (t/year 2017)) t/year)
            [(ti/bounds (t/year 2015) (t/year 2016))]))))

  (testing "f"
    (is
      (=
        {(t/year 2015) [(ti/bounds (t/year-month "2015-06") (t/year-month "2015-12"))]}
        (ti/group-by
          [(t/year 2014) (t/year 2015) (t/year 2016)]
          [(ti/bounds (t/year-month "2015-06") (t/year-month "2015-12"))]))))

  (testing "F"
    (is
      (=
        {(ti/bounds (t/year-month "2015-06") (t/year-month "2015-12"))
         [(ti/bounds (t/year-month "2015-06") (t/year-month "2015-12"))]}
        (ti/group-by
          [(ti/bounds (t/year-month "2015-06") (t/year-month "2015-12"))]
          [(t/year 2014) (t/year 2015) (t/year 2016)]))))

  (testing "d"
    (is
      (=
        {(t/year 2015) [(ti/bounds (t/year-month "2015-03") (t/year-month "2015-09"))]}
        (ti/group-by
          [(t/year 2014) (t/year 2015) (t/year 2016)]
          [(ti/bounds (t/year-month "2015-03") (t/year-month "2015-09"))]))))

  (testing "D"
    (is
      (=
        {(ti/bounds (t/year-month "2015-03") (t/year-month "2015-09"))
         [(ti/bounds (t/year-month "2015-03") (t/year-month "2015-09"))]}

        (ti/group-by
          [(ti/bounds (t/year-month "2015-03") (t/year-month "2015-09"))]
          [(t/year 2014) (t/year 2015) (t/year 2016)]))))

  (testing "o"
    (is
      (=
        {(ti/bounds (t/year-month "2015-06") (t/year-month "2017-06"))
         [(ti/bounds (t/year-month "2015-06") (t/year-month "2015-12"))
          (t/year "2016")
          (ti/bounds (t/year-month "2017-01") (t/year-month "2017-06"))]}
        (ti/group-by
          [(ti/bounds (t/year-month "2015-06") (t/year-month "2017-06"))]
          (ti/divide (ti/bounds (t/year 2014) (t/year 2018)) t/year))))))

(deftest group-by-test
  (is
    (= 31
       (count
         (ti/group-by
           t/date
           [(t/date "2015-05-20") (t/year-month "2015-06")])))))

(deftest group-by-empty-test
  (is (= {} (ti/group-by t/date []))))

(deftest am-test
  (t/with-clock (cljc.java-time.clock/fixed (t/instant "2017-08-08T12:00:00Z") t/UTC)
    (is (= (ti/new-interval (t/date-time "2017-08-08T00:00:00")
             (t/date-time "2017-08-08T12:00:00"))
          (ti/am (t/today))))
    (is (= (ti/new-interval (t/date-time "2017-08-08T12:00:00")
             (t/date-time "2017-08-09T00:00:00"))
          (ti/pm (t/today))))))

;; TODO: Interval testing

(deftest division-test2
  (is (= 365 (count (ti/divide-by t/date (t/year 2017)))))
  (is (= 12 (count (ti/divide-by t/year-month (t/year 2017)))))
  (is (= 30 (count (ti/divide-by t/date "2017-09"))))
  (is (= (t/date "2017-09-01") (first (ti/divide-by t/date "2017-09"))))
  (is (= (t/date "2017-09-30") (last (ti/divide-by t/date "2017-09"))))
  (is (= 31 (count (ti/divide-by t/date "2017-10"))))
  (is (= 8 (count (ti/divide-by t/date (ti/bounds (t/date "2017-10-03") (t/date "2017-10-10"))))))
  (is (= [(t/date "2017-09-10")] (ti/divide-by t/date (ti/bounds (t/date-time "2017-09-10T12:00") (t/date-time "2017-09-10T14:00")))))
  (is (= [(t/date "2017-09-10") (t/date "2017-09-11")] (ti/divide-by t/date (ti/bounds (t/date-time "2017-09-10T12:00") (t/date-time "2017-09-11T14:00")))))
  (is (= 2 (count (ti/divide-by t/year-month (ti/bounds (t/date "2017-09-10") (t/date "2017-10-10"))))))
  (is (= 3 (count (ti/divide-by t/year (ti/bounds (t/date-time "2017-09-10T12:00") (t/year "2019"))))))
  (is (= 3 (count (ti/divide-by t/year (ti/bounds (t/date-time "2017-09-10T12:00") (t/year-month "2019-02"))))))
  (is (= 24 (count (ti/divide-by (t/new-duration 1 :hours) (t/date "2017-09-10"))))))

;; TODO: Divide by duration

;; Concur test

(deftest concur-test2
  (is
    (= 2
      (t/hours
        (t/duration
          (ti/concur (ti/new-interval (t/at (t/today) "16:00")
                      (t/end (t/today)))
            (t/today)
            (ti/new-interval (t/at (t/today) "20:00")
              (t/at (t/today) "22:00"))))))))


;; Do not disturb tests

;; Example: We mustn't disturb people between 10pm and 7am the following morning, in their locale.

(defn moment [t]
  (ti/new-interval
    t
    (t/>> t (t/new-duration 3 :seconds))))

;; TODO: Think about conversions between single instants and intervals. Feather? Widen? Smudge?

;; Can we disturb?
(deftest cannot-disturb-test
  (let
    [disturb-interval [(ti/new-interval (t/time "07:00") (t/time "22:00"))]
     no-disturb-interval (ti/complement disturb-interval)
     can-disturb? (fn [t] (not (some #(t/coincident? % t) no-disturb-interval)))
     ]
    (is (not (can-disturb? (t/time "03:00"))))
    (is (not (can-disturb? (t/time "07:00"))))
    (is (can-disturb? (t/time "07:01")))
    (is (can-disturb? (t/time "12:00")))
    (is (can-disturb? (t/time "21:59")))
    (is (not (can-disturb? (t/time "22:00"))))
    (is (not (can-disturb? (t/time "00:00"))))))

(deftest predicate-test
  (is (true? (t/interval? (moment (t/now))))))
