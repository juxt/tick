(ns tick.all-tests
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [tick.core-test]
    [tick.alpha.api-test]
    [tick.interval-test]
    [tick.alpha.api.dates-test]
    [cljs.test :refer-macros [run-tests run-all-tests]]))

(doo-tests 'tick.core-test
  'tick.interval-test
  'tick.alpha.api-test
  'tick.alpha.api.dates-test)
