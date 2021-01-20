(ns tick.format
  "originally copied from https://github.com/dm3/clojure.java-time"
  (:refer-clojure :exclude (format))
  (:require [cljc.java-time.format.date-time-formatter]
            #?(:cljs [java.time.format :refer [DateTimeFormatter]]))
  #?(:clj
     (:import [java.time.format DateTimeFormatter]
              [java.util Locale])))

(def predefined-formatters
  {:iso-zoned-date-time  cljc.java-time.format.date-time-formatter/iso-zoned-date-time
   :iso-offset-date-time cljc.java-time.format.date-time-formatter/iso-offset-date-time
   :iso-local-time       cljc.java-time.format.date-time-formatter/iso-local-time
   :iso-local-date-time  cljc.java-time.format.date-time-formatter/iso-local-date-time
   :iso-local-date       cljc.java-time.format.date-time-formatter/iso-local-date
   :iso-instant          cljc.java-time.format.date-time-formatter/iso-instant

   ; these exist in java but not in js-joda 
   ;:iso-offset-date      (. DateTimeFormatter -ISO_OFFSET_DATE)
   ;:rfc-1123-date-time   (. DateTimeFormatter -RFC_1123_DATE_TIME)
   ;:iso-week-date        (. DateTimeFormatter -ISO_WEEK_DATE)
   ;:iso-ordinal-date     (. DateTimeFormatter -ISO_ORDINAL_DATE)
   ;:iso-time             (. DateTimeFormatter -ISO_TIME)
   ;:iso-date             (. DateTimeFormatter -ISO_DATE)
   ;:basic-iso-date       (. DateTimeFormatter -BASIC_ISO_DATE)
   ;:iso-date-time        (. DateTimeFormatter -ISO_DATE_TIME)
   ;:iso-offset-time      (. DateTimeFormatter -ISO_OFFSET_TIME)
   })

(defn ^DateTimeFormatter formatter
  "Constructs a DateTimeFormatter out of either a

  * format string - \"YYYY/mm/DD\" \"YYY HH:MM\" etc.
  or
  * formatter name - :iso-instant :iso-local-date etc
  
  and a Locale, which is optional."
  ([fmt]
   (formatter
     fmt
     #?(:clj (Locale/getDefault)
        :cljs (try
                (some->
                  (goog.object/get js/JSJodaLocale "Locale")
                  (goog.object/get "US"))
                (catch js/Error e)))))
  ([fmt locale]
   (let [^DateTimeFormatter fmt
         (cond (instance? DateTimeFormatter fmt) fmt
               (string? fmt) (if (nil? locale)
                               (throw
                                 #?(:clj (Exception. "Locale is nil")
                                    :cljs (js/Error. (str "Locale is nil, try adding a require '[tick.locale-en-us]"))))
                               (-> (cljc.java-time.format.date-time-formatter/of-pattern fmt)
                                   (cljc.java-time.format.date-time-formatter/with-locale locale)))
               :else (get predefined-formatters fmt))]
     fmt)))

(defn format
  "Formats the given time entity as a string.
  Accepts something that can be converted to a `DateTimeFormatter` as a first
  argument. Given one argument uses the default format."
  ([o] (str o))
  ([fmt o]
   (cljc.java-time.format.date-time-formatter/format (formatter fmt) o)))