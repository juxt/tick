(ns tick.interop
  #?(:clj
     (:require [net.cgrand.macrovich :as macros])
     :cljs
     (:require-macros 
       [net.cgrand.macrovich :as macros]
       [tick.interop :refer [static-prop getter]])))

(macros/deftime

  (defmacro static-prop [target prop]
    (macros/case
        :clj
        `(. ~target ~prop)
        :cljs
        `(goog.object/get ~target ~(str prop))))
  
(defmacro getter [p t & args]
  (macros/case
    :clj (let [[start & remainder] (str p)]
           (apply list (symbol (str ".get" (clojure.string/upper-case start) (apply str remainder))) t args))
    :cljs (apply list (symbol (str "." p)) t args))))


