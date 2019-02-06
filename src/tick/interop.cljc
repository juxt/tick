(ns tick.interop
  #?(:clj
     (:require [net.cgrand.macrovich :as macros])
     :cljs
     (:require-macros 
       [net.cgrand.macrovich :as macros]
       [tick.interop :refer [getter]])))

(macros/deftime
  
(defmacro getter [p t & args]
  (macros/case
    :clj (let [[start & remainder] (str p)]
           (apply list (symbol (str ".get" (clojure.string/upper-case start) (apply str remainder))) t args))
    :cljs (apply list (symbol (str "." p)) t args))))


