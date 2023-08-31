(ns repl
  (:require
    [cljs :refer :all]
    [clojure.tools.namespace.repl :refer [refresh refresh-all]]
    [kaocha.repl :as kr]
    [tick.core :as t])
  (:import (java.util TimeZone)))

(comment 
  (remove-ns 'repl)
  )

(clojure.tools.namespace.repl/set-refresh-dirs "src" "test")

(set! *warn-on-reflection* true)

(defn set-zone [^String tz]
  (TimeZone/setDefault (TimeZone/getTimeZone tz))
  (alter-var-root #'t/*clock*
    (constantly (cljc.java-time.zone-id/system-default)))
  )

(comment
  
  (set-zone "America/New_York")
  
  )

(when (System/getProperty "nrepl.load")
  (require 'nrepl))

(defn test-clj* [] (kr/run :clj))

(defn test-clj []
  (refresh :after 'repl/test-clj*))

(comment
  (refresh-all)
  (clojure.tools.namespace.repl/clear)
  (test-clj)
  
)
