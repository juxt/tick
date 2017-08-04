;; Copyright © 2016-2017, JUXT LTD.

(ns tick.interval-test
  (:refer-clojure :exclude [contains? complement])
  (:require
   [clojure.test :refer :all]
   [clojure.spec.alpha :as s]
   [tick.core :refer [instant zone local-date]]
   [tick.interval :refer :all]))

(s/check-asserts true)

(deftest to-interval-test []
  (is
   (=
    (interval "2017-08-19T23:00:00Z" "2017-08-20T23:00:00Z")
    (to-interval (local-date "2017-08-20") (zone "Europe/London")))))

(deftest local-dates-test []
  (let [res
        (local-dates (interval "2017-08-19T23:00:00Z" "2017-09-20T23:00:00Z") (zone "Europe/London"))]
    (is (= 33 (count res)))
    (is (= "2017-08-20" (str (first res))))
    (is (= "2017-09-21" (str (last res))))))

;; Allen's Interval Algebra

(deftest basic-relations-test
  (is (= (count basic-relations) 13))
  (is (distinct? basic-relations)))

;; We can construct every possible combination of interval relation with just 4 instants.
(def instants [(instant "2017-07-30T09:00:00Z")
               (instant "2017-07-30T11:00:00Z")
               (instant "2017-07-30T13:00:00Z")
               (instant "2017-07-30T15:00:00Z")])

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
