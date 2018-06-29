;; Copyright © 2016-2017, JUXT LTD.

(ns tick.file
  (:require
   [tick.core :as t])
  (:import
   [java.time Instant]))

(extend-protocol t/IConversion
  java.io.File
  (instant [f] (Instant/ofEpochMilli (.lastModified f)))
  java.nio.file.Path
  (instant [f] (t/instant (.toFile f))))
