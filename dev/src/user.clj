(ns user
  (:require
   [cljs :refer :all]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]
   [kaocha.repl :as kr]
   [tick.core :as t]
   [tick.viz :refer [show-canvas view label]]))

(set! *warn-on-reflection* true)

(when (System/getProperty "nrepl.load")
  (require 'nrepl))

(defn test-clj* [] (kr/run :clj))

(defn test-clj []
  (refresh :after 'user/test-clj*))

(comment
  (refresh-all)
  (clojure.tools.namespace.repl/clear)
  (test-clj)
)
