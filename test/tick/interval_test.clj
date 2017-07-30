;; Copyright © 2016-2017, JUXT LTD.

;; Tests for Allen's interval algebra: http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf

;; See also https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html for clear explanation

(ns tick.interval-test
  (:refer-clojure :exclude [contains?])
  (:require
   [clojure.test :refer :all]
   [clojure.spec.alpha :as s]
   [tick.core :refer [instant]]
   [tick.interval :refer :all]))

;; Allen's Interval Algebra
;; distinct: because no pair of definite intervals can be related by more than one of the relationships
;; (From https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html)

;; We can exhaustively test every combination of interval relation with just 4 instants.
(def instants [(instant "2017-07-30T09:00:00Z")
               (instant "2017-07-30T11:00:00Z")
               (instant "2017-07-30T13:00:00Z")
               (instant "2017-07-30T15:00:00Z")])

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


;; exhaustive: because any pair of definite intervals are described by one of the relations
;; Note: tick.interval/relation throws an exception if a coding error means the 13 relations are not exhaustive.
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
          (relation x y)))))))
