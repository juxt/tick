(ns user
  (:require
   [tick.alpha.api :as t]
   [tick.viz :refer [show-canvas view label]]
   [clojure.spec.alpha :as s]
   [cljs.cljs-repl :as cljs-repl]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]
   [infra]
   clojure.test)
  (:import [java.time DayOfWeek]))

(set! *warn-on-reflection* true)

(when (System/getProperty "nrepl.load")
  (require 'nrepl))

(defn test-all []
  (refresh)
  (clojure.test/run-all-tests #"(tick).*test$"))

(defn node []
  (cljs-repl/node-repl))

(comment 
  (refresh-all)
  (test-all)
  
  )