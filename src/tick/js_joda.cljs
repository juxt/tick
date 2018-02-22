(ns tick.js-joda
  (:require [cljsjs.js-joda]))


(def Date js/Date)
(def Clock (.. js/JSJoda -Clock))
(def ZoneOffset (.. js/JSJoda -ZoneOffset))
(def Instant (.. js/JSJoda -Instant))
(def Duration (.. js/JSJoda -Duration))
(def Period (.. js/JSJoda -Period))
(def DayOfWeek (.. js/JSJoda -DayOfWeek))
(def Month (.. js/JSJoda -Month))
(def ZonedDateTime (.. js/JSJoda -ZonedDateTime))
(def LocalTime (.. js/JSJoda -LocalTime))
(def LocalDateTime (.. js/JSJoda -LocalDateTime))
(def LocalDate (.. js/JSJoda -LocalDate))
(def Year (.. js/JSJoda -Year))
(def YearMonth (.. js/JSJoda -YearMonth))
(def ZoneId (.. js/JSJoda -ZoneId))
(def ChronoUnit (.. js/JSJoda -ChronoUnit))
(def ChronoField (.. js/JSJoda -ChronoField))
(def TemporalAdjusters (.. js/JSJoda -TemporalAdjusters))
(def Temporal (.. js/JSJoda -Temporal))
(def TemporalAmount (.. js/JSJoda -TemporalAmount))

;; todo - get these from threeten-extra? https://github.com/js-joda/js-joda/issues/165
(def OffsetDateTime (.. js/JSJoda -ZonedDateTime))
(def OffsetTime (.. js/JSJoda -LocalTime))
