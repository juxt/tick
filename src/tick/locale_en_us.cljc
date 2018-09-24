(ns tick.locale-en-us
  #?(:cljs (:require [henryw374.js-joda-locale-en-us])))

#?(:cljs
   (set! js/JSJoda (.use js/JSJoda js/JSJodaLocale)))

