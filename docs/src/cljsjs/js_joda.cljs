(ns cljsjs.js-joda
  (:require ["js-joda" :as js-joda]
            ["js-joda-timezone"]))

(js/goog.exportSymbol "JSJoda" js-joda)
