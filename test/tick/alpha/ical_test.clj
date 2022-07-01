;; Copyright © 2016-2017, JUXT LTD.

(ns tick.alpha.ical-test
  (:require
   [tick.alpha.ical :as ical]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

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

(deftest ^:tick.test/slow parse-icalendar-test
  (let [result (ical/parse-ical (io/reader (io/resource "gb.ics")))]
    (is (= 231 (-> result first :subobjects count)))))

(deftest line->contentline-test
  (testing "Simple lines can be parsed to contentlines"
    (is (= {:name "SEQUENCE" :value "1" :params {} :string-value "1"} (ical/line->contentline "SEQUENCE:1")))
    (is (= {:name "sequence" :value "1" :params {} :string-value "1"} (ical/line->contentline "sequence:1")))
    (is (= {:name "SeqUence" :value "1" :params {} :string-value "1"} (ical/line->contentline "SeqUence:1")))
    (is (= {:name "REFRESH-INTERVAL" :value "7" :params {} :string-value "7"} (ical/line->contentline "REFRESH-INTERVAL:7"))))

  (testing "Lines with parameters can be parsed to contentlines"
    (let [{:keys [name params value]} (ical/line->contentline "DTSTART;TZID=US-EAST:20180116T140000")]
      (is (= "DTSTART" name))
      (is (= "US-EAST" (get params "TZID")))
      (is (= "20180116T140000" value)))))
