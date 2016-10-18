;; Copyright © 2016, JUXT LTD.

(ns tick.core
  (:require
   [clojure.spec :as s]
   [clj-time.core :as t]
   [clj-time.periodic :refer [periodic-seq]]))

(defn- merge-sort
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
   (merge-sort compare colls)))

;;(s/def :tick/date )

(def schedule
  (agent
   (periodic-seq
    (t/plus (t/now) (-> 1 t/seconds))
    (-> 1 t/seconds))))

(defn drainer
  "Call sinkf with each past event. Return future events"
  [sinkf]
  (fn [times]
    (let [now (t/now)]
      (loop [tms times]
        (if (t/before? (first tms) now)
          (do
            (sinkf (first tms))
            (recur (rest tms)))
          tms)))))

;; Send the drainer to the agent. This ends up in an infinite loop until I get a Java heap-space issue
(do
  (send schedule (drainer println))
  :ok)
