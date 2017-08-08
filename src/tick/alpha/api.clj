;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.api
  (:refer-clojure :exclude [+ - inc dec max min range])
  (:require
   [clojure.spec.alpha :as s]
   [tick.core :as core]
   [tick.cal :as cal]
   [tick.interval :as interval])
  (:import
   [java.time Duration ZoneId]))

;; This API is optimises convenience, API stability and (type) safety
;; over performance. Where performance is critical, use tick.core and
;; friends.

;; clojure.spec assertions are used to check correctness, but these
;; are disabled by default.

(defn nanos [n] (core/nanos n))
(defn millis [n] (core/millis n))
(defn seconds [n] (core/seconds n))
(defn minutes [n] (core/minutes n))
(defn hours [n] (core/hours n))
(defn days [n] (core/days n))
(defn weeks [n] (core/weeks n))
(defn now [] (core/now))

(defn + [arg & args]
  (reduce #(core/+ %1 %2) arg args))

(defn - [arg & args]
  (reduce #(core/- %1 %2) arg args))

(defn inc [arg]
  (core/inc arg))

(defn dec [arg]
  (core/dec arg))

(defn max [arg & args]
  (reduce #(core/max %1 %2) arg args))

(defn min [arg & args]
  (reduce #(core/min %1 %2) arg args))

(def range core/range)

;; Constructors

(defn instant [v]
  (core/instant v))

(defn date
  ([v zone]
   (core/date v zone))
  ([v]
   (core/date v)))

(defn interval
  ([v1 v2]
   {:post [(s/assert :tick.interval/interval %)]}
   (interval/interval v1 v2)))

(defn as-interval [v zone]
  {:post [(s/assert :tick.interval/interval %)]}
  (interval/as-interval v (core/zone zone)))

(defn duration [interval]
  {:pre [(s/assert :tick.interval/interval interval)]}
  (interval/duration interval))

(defn zone [z]
  (core/zone z))

(defn intersection [x y]
  (interval/intersection x y))

(defn partition-by-date [interval zone]
  {:pre [(s/assert :tick.interval/interval interval)]}
  (interval/partition-by-date interval (core/zone zone)))

(defn dates
  "Return a lazy sequence of the dates (inclusive) that the
  given interval spans."
  [interval zone]
  {:pre [(s/assert :tick.interval/interval interval)]}
  (interval/dates interval (core/zone zone)))

;; Assertions

(defn assert-interval
  "Assert an interval without requiring client knowledge of the spec
  keyword (which may change)."
  [interval]
  (s/assert :tick.interval/interval interval))
