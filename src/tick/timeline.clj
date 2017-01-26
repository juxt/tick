;; Copyright © 2016-2017, JUXT LTD.

(ns tick.timeline
  (:import
   [java.time Duration ZonedDateTime]))

(defn periodic-seq
  "Given a start time, create a timeline with times at constant intervals of period length"
  ([^ZonedDateTime start ^Duration period]
   (iterate #(.addTo period %) start)))

(defn merge-timelines
  "Merge sort across set of collections.
   See http://blog.malcolmsparks.com/?p=42 for full details."
  ([^java.util.Comparator comp colls]
   (let [begin (new Object)
         end (new Object)]
     (letfn [(next-item [[_ colls]]
               (if (nil? colls)
                 [end nil]
                 (let [[[yield & p] & q]
                       (sort-by first comp colls)]
                   [yield (if p (cons p q) q)])))]
       (->> colls
            (vector begin)
            (iterate next-item)
            (drop 1)
            (map first)
            (take-while (partial not= end))))))
  ([colls]
   (merge-timelines compare colls)))
