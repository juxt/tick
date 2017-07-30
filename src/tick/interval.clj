;; Copyright © 2016-2017, JUXT LTD.

;; Allen's interval algebra: http://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf
;; See also https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html for clear explanation

;; Use of Allens' interval algebra from a suggestion by Eric Evans.

(ns tick.interval
  (:refer-clojure :exclude [contains?])
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :refer [instant]]
   [clojure.test :refer [is]]))

(s/def ::interval
  (s/and
   (s/tuple :tick.core/instant :tick.core/instant)
   #(.isBefore (first %) (second %))))

(defn ^{:test
        (fn []
          (is (precedes?
               [(instant "2017-07-30T09:00:00Z") (instant "2017-07-30T12:00:00Z")]
               [(instant "2017-07-30T13:00:00Z") (instant "2017-07-30T17:00:00Z")])))}
  precedes? [x y]
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
(defn converse
  "The converse of a relation."
  [f]
  (fn [x y]
    (f y x)))

(def preceded-by? (converse precedes?))
(def met-by? (converse meets?))
(def overlapped-by? (converse overlaps?))
(def finished-by? (converse finishes?))
(def contains? (converse during?))
(def started-by? (converse starts?))

(def ^{:test
       (fn []
         ;; There are 13 basic relations
         (is (= (count basic-relations) 13))
         ;; Distinct check (make sure we're not including any of them more than once)
         (is (distinct? basic-relations)))}
  basic-relations
  [precedes? meets? overlaps? finished-by? contains?
   starts? equals? started-by? during? finishes? overlapped-by?
   met-by? preceded-by?])

(defn relation [x y]
  (s/assert ::interval x)
  (s/assert ::interval y)
  (cond
    (precedes? x y) :precedes
    (meets? x y) :meets
    (overlaps? x y) :overlaps
    (finished-by? x y) :finished-by
    (contains? x y) :contains
    (starts? x y) :starts
    (equals? x y) :equals
    (started-by? x y) :started-by
    (during? x y) :during
    (finishes? x y) :finishes
    (overlapped-by? x y) :overlapped-by
    (met-by? x y) :met-by
    (preceded-by? x y) :preceded-by
    :else (throw (ex-info "Should not get here" {}))))
