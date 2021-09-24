(ns tick.internals-test
  (:require [clojure.test
             :refer [deftest is testing run-tests]
             :refer-macros [deftest is testing run-tests]]
            [tick.core :as t]
            [tick.protocols :as p]))

(deftest parse-test
  (testing "(time \"4pm\")"
    (is (t/time? (p/parse "4pm")))
    (is (= "16:00" (str (p/parse "4pm"))))))
