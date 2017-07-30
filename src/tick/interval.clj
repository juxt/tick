;; Copyright © 2016-2017, JUXT LTD.

;; Allen's interval algebra:

;; Use of Allens' interval algebra from a suggestion by Eric Evans.

(ns tick.interval
  (:refer-clojure :exclude [contains? complement])
  (:require
   [clojure.set :as set]
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

(defn complement
  "Return the complement of the general relation. The complement ~r of
  a relation r is the relation consisting of all basic relations not
  in r."
  [^GeneralRelation r]
  (assoc r :relations (remove (set (:relations r)) basic-relations)))

(defn compose
  "Return the composition of r and s"
  [r s]
  (throw (new UnsupportedOperationException "Not yet implemented")))

(defn converse
  "Return the converse of the given general relation. The converse !r
  of a relation r is the relation consisting of the converses of all
  basic relations in r."
  [^GeneralRelation r]
  (assoc r :relations (map conv (:relations r))))

(defn intersection
  "Return the intersection of the r with s"
  [^GeneralRelation r ^GeneralRelation s]
  (s/assert r #(instance? GeneralRelation %))
  (->GeneralRelation (set/intersection (set (:relations r))))
  (throw (new UnsupportedOperationException "Not yet implemented")))

;; Useful relations

(def disjoint? (make-relation precedes? preceded-by? meets? met-by?))
(def concur? (complement disjoint?))
