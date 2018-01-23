(ns tick.ical
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [tick.core :as t]
   )
  (:import
   [java.time Instant LocalDate LocalDateTime ZonedDateTime ZoneId]))

;; Have considered wrapping ical4j. However, since ical4j does not
;; use Java 8 time (as of the time of writing), any appeals to
;; maturity/stability are moot. Sometimes it's a bad idea to wrap a
;; Java library simply because one exists (concurrency issues, etc.)

(def CRLF "\r\n")

(defn contentline
  "Fold the given string value returning a sequence of strings up to
  75 octets in length, as per RFC 5545 folding rules."
  [s]
  (let [limit 75]
    (loop [acc [] n 0]
      (if (>= (+ n limit) (count s))
        (conj acc (str
                    (if (empty? acc) "" " ")
                    (subs s n)
                    CRLF))
        (recur
          (conj acc (str
                      (if (empty? acc) "" " ")
                      (subs s n (+ n limit))
                      CRLF))
          (+ limit n))))))

(defn print-cl
  "Folding print, where long lines are folded as per RFC 5545 Section 4.1"
  [& args]
  (doseq [line (contentline (apply str args))]
    (print line)))

(defprotocol ICalendarValue
  (serialize-value [_] ""))

(def DATE-PATTERN (java.time.format.DateTimeFormatter/ofPattern "YYYYMMdd"))

(def DATE-TIME-FORM-1-PATTERN (java.time.format.DateTimeFormatter/ofPattern "YYYYMMdd'T'HHmmss"))

;; Only call with ZoneID of UTC otherwise produces invalid ICAL format
(def DATE-TIME-FORM-2-PATTERN (java.time.format.DateTimeFormatter/ofPattern "YYYYMMdd'T'HHmmssX"))

(def DATE-TIME-FORM-3-PATTERN (java.time.format.DateTimeFormatter/ofPattern "YYYYMMdd'T'HHmmss"))

(extend-protocol ICalendarValue
  String
  (serialize-value [s] {:value s})
  Instant
  (serialize-value [i] {:value (.format (ZonedDateTime/ofInstant i (ZoneId/of "UTC")) DATE-TIME-FORM-2-PATTERN)})
  LocalDate
  (serialize-value [s] {:value (.format s DATE-PATTERN)})
  LocalDateTime
  (serialize-value [s] {:value (.format s DATE-TIME-FORM-1-PATTERN)})
  ZonedDateTime
  (serialize-value [s] {:value (.format s DATE-TIME-FORM-3-PATTERN)
                        :params {:tzid (t/zone s)}}))

(defprotocol ICalendarObject
  (print-object [_] "Print as an iCalendar object"))

(defmacro wrap-with [c & body]
  `(do
     (print-cl "BEGIN:" ~c)
     ~@body
     (print-cl "END:" ~c)))

(defrecord Property [prop-name prop-value]
  ICalendarObject
  (print-object [_]
    (let [{:keys [params value]} (serialize-value prop-value)]
      (print-cl
        (str/upper-case (name prop-name))
        (apply str (for [[k v] params]
                     (str ";" (str/upper-case (name k)) "=" v)))
        ":"
        value))))

(defn property [name value]
  (->Property name value))

(defrecord VEvent [beginning end properties]
  t/ITimeSpan
  (beginning [_] beginning)
  (end [_] end)
  ICalendarObject
  (print-object [this]
    (wrap-with
      "VEVENT"
      (doseq [prop properties]
        (print-object prop)))))

(defn vevent [summary dtstart dtend & {:as props}]
  (->VEvent
    (t/beginning dtstart)
    (t/end dtend)
    (concat
      [(property :summary summary)
       (property :dtstart dtstart)
       (property :dtend dtend)]
      (for [[k v] props]
        (property k v)))))

(defrecord VCalendar [objects]
  ICalendarObject
  (print-object [this]
    (wrap-with
      "VCALENDAR"
      (doseq [obj objects]
        (print-object obj)))))

(defn vcalendar [& objects]
  (->VCalendar objects))

(comment
  ;; Form 1: DATE WITH LOCAL TIME
  (with-out-str
    (print-object
      (vcalendar
        (vevent "Malcolm is on holiday!"
                (t/date "2018-07-21")
                (t/date "2018-07-31")
                :description "The content information associated with an iCalendar object is formatted using a syntax similar to that defined by [RFC 2425]. That is, the content information consists of CRLF-separated content lines."))))

  ;; Form 2: DATE WITH UTC TIME
  (with-out-str
    (print-object
      (vcalendar
        (vevent "Malcolm is in a meeting!"
                (t/now)
                (t/+ (t/now) (t/minutes 50))
                :description "The content information associated with an iCalendar object is formatted using a syntax similar to that defined by [RFC 2425]. That is, the content information consists of CRLF-separated content lines."))))

  ;; Form 3: DATE WITH LOCAL TIME AND TIME ZONE REFERENCE
  (with-out-str
    (print-object
      (vcalendar
        (vevent
          "Malcolm is in a meeting, in New York!"
          (t/at-zone (t/at (t/today) "14:00") "America/New_York")
          (t/at-zone (t/at (t/today) "14:50") "America/New_York")
          :description "The content information associated with an iCalendar object is formatted using a syntax similar to that defined by [RFC 2425]. That is, the content information consists of CRLF-separated content lines.")))))


;; Parsing with spec

(s/def ::contentline
  (s/cat
    :name ::name
    :params (s/*
              (s/cat
                :semicolon #{\;}
                :param ::param))
    :colon #{\:}
    :value ::value))

(defn char-range [from to]
  (map char (range (int from) (inc (int to)))))

(def QSAFE-CHAR
  (set (concat [\space \t]
               [(char 0x21)]
               (char-range 0x23 0x7e))))

(def SAFE-CHAR
  (set (concat [\space \t]
               [(char 0x21)]
               (char-range 0x23 0x2b)
               (char-range 0x2d 0x39)
               (char-range 0x3c 0x7e))))

(def VALUE-CHAR
  (set (concat [\space \t]
               (char-range 0x21 0x7e))))
;; TODO: Add NON-US-ASCII

(def CONTROL
  (set (concat (char-range 0x00 0x08)
               (char-range 0x0a 0x1f)
               [0x7f])))

(s/def ::name
  (s/alt :iana-token ::iana-token
         :x-name ::x-name))

(def ALPHA-DIGIT
  (set/union (set (char-range \a \z))
             (set (char-range \A \Z))
             (set (char-range \0 \9))))

(s/def ::iana-token
  (s/+ (conj ALPHA-DIGIT #{\-})))

(s/def ::x-name
  (s/cat
    :prefix (s/cat :x1 #{\X}
                   :x2 #{\-})
    :vendorid (s/? (s/cat :vendorid ::vendorid
                          :dash #{\-}))
    :suffix (s/+ (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890-"))))

(s/def ::vendorid
  (s/and
    (s/+ (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"))
    #(= (count %) 3)))

(s/def ::param
  (s/cat :param-name ::param-name
         :equals #{\=}
         :param-value ::param-value
         :param-values (s/* (s/cat :comma #{,} :param-value ::param-value))))

(s/def ::param-name
  (s/alt :iana-token ::iana-token :x-name ::x-name))

(s/def ::param-value
  (s/alt :paramtext ::paramtext
         :quoted-string ::quoted-string))

(s/def ::paramtext (s/* ::SAFE-CHAR))

(s/def ::value (s/* ::VALUE-CHAR))

(s/def ::quoted-string
  (s/cat
    :open-quote #{\"}
    :content (s/* ::QSAFE-CHAR)
    :close-quote #{\"}))

;; Any character except CONTROL and DQUOTE
(s/def ::QSAFE-CHAR (comp not (conj CONTROL \")))

;; Any character except CONTROL, DQUOTE, ";", ":", ","
(s/def ::SAFE-CHAR (comp not (set/union CONTROL #{\" \; \: \,})))

(s/def ::VALUE-CHAR (comp not CONTROL))

(defn unfolding-line-seq*
  [^java.io.BufferedReader rdr hold]
  (if-let [line (.readLine rdr)]
    (if (= (.charAt line 0) \space)
      (recur rdr (conj hold line))
      (cons (str/join hold) (lazy-seq (unfolding-line-seq* rdr [line]))))
    [(str/join hold)]))

(defn unfolding-line-seq [^java.io.BufferedReader rdr]
  (next (unfolding-line-seq* rdr [])))

(defn extract-name-as-string [[k v]]
  (case k
    :iana-token (apply str v)
    :x-name (str "X-" (apply str (:suffix v)))
    (throw (ex-info "Bad input" {:k k}))))

(defn extract-param-value-as-string [[k v]]
  (case k
    :paramtext (apply str v)
    :quoted-string (apply str (:content v))))

(defn line->contentline [s]
  (let [m (s/conform ::contentline (seq s))]
    (when-not (:name m) (throw (ex-info "No name" {:contentline s})))
    {:name (-> m :name extract-name-as-string)
     :params (->> m :params
                  (map (juxt
                         (comp extract-name-as-string :param-name :param)
                         (comp extract-param-value-as-string :param-value :param)))
                  (into {} ))
     :value (-> m :value str/join)}))

;; JCF tip
;;(s/conform (s/and (s/conformer seq) ::iana-token) "foobar")

(defmulti add-contentline-to-model
  "A reducing function that gives a parsed content-line to an
  accumulator that builds a model."
  (fn [acc cl] (:name cl)))

(defn error [acc contentline message]
  (update acc :errors
          (fnil conj [])
          {:error message
           :lineno (:lineno contentline)}))

(defmethod add-contentline-to-model "BEGIN"
  [acc contentline]
  ;; BEGIN means place the current object in the stack, and start a
  ;; new one
  (cond-> acc
    (:curr-object acc) (update :stack (fnil conj []) (:curr-object acc))
    true (assoc :curr-object {:object (:value contentline)
                              :lineno (:lineno contentline)})))

(defmethod add-contentline-to-model "END"
  [acc contentline]
  ;; END means place the current object in the stack, and start a
  ;; new one
  (let [curr-object (:curr-object acc)
        restore-object (last (:stack acc)) ; check if nil, bad state
        restore-object (update restore-object :subobjects (fnil conj []) curr-object)]
    (-> acc
        (assoc :curr-object restore-object
               :stack (vec (butlast (:stack acc)))))))

(defn property [acc contentline]
  (update-in acc [:curr-object :properties] (fnil conj []) contentline))

(doseq [s ["VERSION" "PRODID" "CALSCALE" "METHOD" "CLASS" "CREATED" "DESCRIPTION"
           "URL" "DTSTART" "DTEND" "DTSTAMP" "LOCATION"
           "PRIORITY" "SEQUENCE" "SUMMARY" "TRANSP"
           "X-WR-CALNAME" "X-WR-CALDESC" "X-MS-OLK-FORCEINSPECTOROPEN"
           "X-MICROSOFT-CDO-BUSYSTATUS"
           "X-MICROSOFT-CDO-IMPORTANCE"
           "X-MICROSOFT-DISALLOW-COUNTER"
           "X-MS-OLK-ALLOWEXTERNCHECK"
           "X-MS-OLK-AUTOFILLLOCATION"
           "X-MICROSOFT-CDO-ALLDAYEVENT"
           "X-MICROSOFT-MSNCALENDAR-ALLDAYEVENT"
           "X-MS-OLK-CONFTYPE"
           "UID"]]
  (defmethod add-contentline-to-model s [acc contentline] (property acc contentline)))

(defmethod add-contentline-to-model :default
  [acc contentline]
  (throw (ex-info (str "Unhandled case " (:name contentline)) {:acc acc :contentline contentline})))
