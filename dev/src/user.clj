(ns user
  (:require
   [tick.alpha.api :as t]
   [tick.viz :refer [show-canvas view label]]
   [clojure.spec.alpha :as s]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]
   [cljs :refer :all]
   clojure.test))

(set! *warn-on-reflection* true)

(when (System/getProperty "nrepl.load")
  (require 'nrepl))

(defn test-all []
  (refresh)
  (clojure.test/run-all-tests #"(tick).*test$"))

(comment 
  (refresh-all)
  (test-all)
  
  )