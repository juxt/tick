;; Copyright © 2016-2017, JUXT LTD.

(ns tick.ical-test
  (:require
   [tick.ical :as ical]
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [tick.core :as t]
   [clojure.test :refer :all]))

(deftest parse-dtstart
  (let [{:keys [name params value]} (ical/line->contentline "DTSTART;TZID=US-EAST:20180116T140000")]
    (is (= "DTSTART" name))
    (is (= "US-EAST" (get params "TZID")))
    (is (= "20180116T140000" value))))

(deftest parse-unicode
  (let [{:keys [name params value]} (ical/line->contentline "SUMMARY;LANGUAGE=en-us:United Kingdom: St Patrick�s Day (substitute day) (Regional)")]
    (is (= "SUMMARY" name))
    (is (= "en-us" (get params "LANGUAGE")))
    (is (= "United Kingdom: St Patrick�s Day (substitute day) (Regional)" value))))

(deftest stress-test
  (is (= 532
         (count
           (for [line (map first (partition 10 (ical/unfolding-line-seq (io/reader (io/resource "gb.ics")))))]
             (ical/line->contentline line))))))

(deftest parse-file
  (let [result
        (reduce ical/add-contentline-to-model
                {}
                (map-indexed (fn [n o] (assoc o :lineno (inc n)))
                             (map ical/line->contentline
                                  (ical/unfolding-line-seq
                                    (io/reader
                                      (io/resource "gb.ics"))))))]
    (is (= 231 (-> result :curr-object :subobjects first :subobjects count)))))
