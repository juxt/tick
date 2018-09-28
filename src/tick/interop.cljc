(ns tick.interop
  #?(:clj
     (:require [net.cgrand.macrovich :as macros])
     :cljs
     (:require-macros 
       [net.cgrand.macrovich :as macros]
       [tick.interop :refer [static-prop]])))

(macros/deftime

  (defmacro static-prop [target prop]
    (macros/case
        :clj
        `(. ~target ~prop)
        :cljs
        `(goog.object/get ~target ~(str prop)))))


