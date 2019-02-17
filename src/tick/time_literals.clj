(ns tick.time-literals
  (:require [time-literals.read-write]
            [time-literals.data-readers]))

(defonce
  ^{:dynamic true
    :doc "If true, include the time-literals printer, which will affect the way java.time and js-joda objects are printed"}
  *time-literals-printing*
  (not= "false" (System/getProperty "tick.time-literals.printing")))

(defmacro modify-printing-of-time-literals-if-enabled! []
  (when *time-literals-printing*
    '(do 
       (time-literals.read-write/print-time-literals-clj!)
       (time-literals.read-write/print-time-literals-cljs!))))


