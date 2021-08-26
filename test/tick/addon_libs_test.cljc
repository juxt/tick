(ns tick.addon-libs-test
  (:require
    [tick.core :as t]
    [tick.timezone]
    [tick.locale-en-us]
    [clojure.test
     :refer [deftest is testing run-tests]
     :refer-macros [deftest is testing run-tests]]))

(deftest tz-test
  (is (t/zone "Europe/Berlin")))

(deftest locale-test 
  (is (t/formatter "yyyy-MMM-dd")))
