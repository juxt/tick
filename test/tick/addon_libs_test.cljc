(ns tick.addon-libs-test
  (:require
    [tick.alpha.api :as t]
    [tick.timezone]
    [tick.locale-en-us]
    [tick.format :as tf]
    #?(:clj [clojure.test :refer :all]
       :cljs [cljs.test :refer-macros [deftest is testing run-tests]])))

(deftest tz-test
  (is (t/zone "Europe/Berlin")))

(deftest locale-test 
  (is (tick.format/formatter "yyyy-MMM-dd")))
