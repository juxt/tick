(ns tick.impl
  "place "
  (:require [tick.protocols :as p]
            [cljc.java-time.local-time]
            [cljc.java-time.instant]
            [cljc.java-time.zoned-date-time]
            [cljc.java-time.offset-date-time]
            [cljc.java-time.local-date]
            [cljc.java-time.year-month]
            [cljc.java-time.year]
            [cljc.java-time.local-date-time]))

(defn parse-int [x]
  #?(:clj (Integer/parseInt x)
     :cljs (js/Number x)))

(extend-protocol p/IParseable
  #?(:clj String :cljs string)
  (parse [s]
    (condp re-matches s
      #"(\d{1,2})\s*(am|pm)"
      :>> (fn [[_ h ap]] (cljc.java-time.local-time/of (cond-> (parse-int h) (= "pm" ap) (clojure.core/+ 12)) 0))
      #"(\d{1,2})"
      :>> (fn [[_ h]] (cljc.java-time.local-time/of (parse-int h) 0))
      #"\d{2}:\d{2}\S*"
      :>> (fn [s] (cljc.java-time.local-time/parse s))
      #"(\d{1,2}):(\d{2})"
      :>> (fn [[_ h m]] (cljc.java-time.local-time/of (parse-int h) (parse-int m)))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z"
      :>> (fn [s] (cljc.java-time.instant/parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?[+-]\d{2}:\d{2}"
      :>> (fn [s] (cljc.java-time.offset-date-time/parse s))
      #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?(?:[+-]\d{2}:\d{2}|Z)\[\w+/\w+\]"
      :>> (fn [s] (cljc.java-time.zoned-date-time/parse s))
      #"\d{4}-\d{2}-\d{2}T\S*"
      :>> (fn [s] (cljc.java-time.local-date-time/parse s))
      #"\d{4}-\d{2}-\d{2}"
      :>> (fn [s] (cljc.java-time.local-date/parse s))
      #"\d{4}-\d{2}"
      :>> (fn [s] (cljc.java-time.year-month/parse s))
      #"\d{4}"
      :>> (fn [s] (cljc.java-time.year/parse s))
      (throw (ex-info "Unparseable time string" {:input s})))))
