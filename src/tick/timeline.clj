;; Copyright © 2016-2017, JUXT LTD.

(ns tick.timeline
  (:require
   [clojure.spec :as s])
  (:import
   [java.time ZonedDateTime]
   [java.time.temporal Temporal TemporalAmount]))

(defn periodic-seq
  "Given a start time, create a timeline with times at constant intervals of period length"
  ([^Temporal start ^TemporalAmount period]
   (iterate #(.addTo period %) start)))

(s/def :tick/date #(instance? ZonedDateTime %))

(defn timeline-xf
  "A transducer that transforms a sequence of :tick/date into a
  timeline of Clojure maps. Each map contains :tick/date."
  [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (rf result {:tick/date input}))))

(s/def :tick/seq integer?) ; TODO: postive too

(defn sequencer
  "A transducer that adds a :tick/seq to a timeline."
  ([] (sequencer 0))
  ([start]
   (fn [rf]
     (let [counter (volatile! (dec start))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (rf result (assoc input :tick/seq (vswap! counter inc)))))))))

(defn interleave-timelines
  "Interleave a collection of timelines producing a single timeline ordered by time.
   See http://blog.malcolmsparks.com/?p=42 for further discussion."
  [& timelines]
  (let [begin (new Object)
        end (new Object)]
    (letfn [(next-item [[_ timelines]]
              (if (nil? timelines)
                [end nil]
                (let [[[yield & p] & q]
                      (sort-by (comp :tick/date first) compare timelines)]
                  [yield (if p (cons p q) q)])))]
      (->> timelines
           (vector begin)
           (iterate next-item)
           (drop 1)
           (map first)
           (take-while (partial not= end))))))

(defn timeline
  "Create a timeline from a sequence of dates (java.time.ZonedDateTime)"
  ([coll]
   (timeline nil coll))
  ([xf coll]
   (sequence (cond-> timeline-xf xf (comp xf)) coll)))
