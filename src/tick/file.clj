;; Copyright © 2016-2017, JUXT LTD.

(ns tick.file
  (:require
   [tick.protocols :as p])
  (:import
   [java.time Instant]))

(extend-protocol p/IConversion
  java.io.File
  (instant [f] (Instant/ofEpochMilli (.lastModified f)))
  java.nio.file.Path
  (instant [f] (p/instant (.toFile f))))
