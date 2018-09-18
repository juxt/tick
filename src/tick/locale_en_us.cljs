(ns tick.locale-en-us
  (:require [henryw374.js-joda-locale-en-us]))

(set! js/JSJoda (.use js/JSJoda js/JSJodaLocale))

