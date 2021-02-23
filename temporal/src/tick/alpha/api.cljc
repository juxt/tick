(ns tick.alpha.api
  (:require [tick.protocols :as p]
            #?(:clj [tick.core :as impl]
               :cljs [tick.temporal :as impl])))
