;; Copyright © 2016-2017, JUXT LTD.

(ns tick.api
  (:refer-clojure :exclude [+ - inc dec max min range])
  (:require
   [tick.core :as core])
  (:import
   [java.time Duration]))

(defn nanos [n] (core/nanos n))
(defn millis [n] (core/millis n))
(defn seconds [n] (core/seconds n))
(defn minutes [n] (core/minutes n))
(defn hours [n] (core/hours n))
(defn days [n] (core/days n))
(defn weeks [n] (core/weeks n))
(defn now [] (core/now))

(defn + [arg & args]
  (reduce #(core/plus-t %1 %2) arg args))

(defn - [arg & args]
 (reduce #(core/minus-t %1 %2) arg args))

(defn max [arg & args]
  (reduce #(core/max-t %1 %2) arg args))

(defn min [arg & args]
  (reduce #(core/min-t %1 %2) arg args))

(def range core/range-t)
