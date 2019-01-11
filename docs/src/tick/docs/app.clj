(ns tick.docs.app
  (:require [cljs.env :as env]))


(defmacro analyzer-state [[_ ns-sym]]
  `'~(get-in @env/*compiler* [:cljs.analyzer/namespaces ns-sym]))
