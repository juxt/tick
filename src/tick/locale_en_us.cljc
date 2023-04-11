(ns tick.locale-en-us
  #?(:cljs (:require ["@js-joda/locale_en-us" :as js-joda-locale])))

; doing this for the one-arity tick.core/formatter. (npm users don't get js/JSJodaLocale global automatically)
#?(:cljs (js/goog.exportSymbol "JSJodaLocale" js-joda-locale))


