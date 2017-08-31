;; Copyright © 2016-2017, JUXT LTD.

(ns tick.file
  (:require
   [tick.core :as t])
  (:import
   [java.time Instant]))

(extend-protocol t/ITime
  java.io.File
  (time [f] (Instant/ofEpochMilli (.lastModified f)))
  (local? [t] false)
  java.nio.file.Path
  (time [f] (time (.toFile f)))
  (local? [t] false))
