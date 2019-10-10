(ns cljs
  (:require [figwheel.main.api :as fig]
            ))


(defn figwheel-start! []
  (fig/start {:mode :serve} "tick")
  (println "auto run tests at http://localhost:9500/figwheel-extra-main/auto-testing"))

(defn figwheel-stop! []
  (fig/stop-all))

(defn cljs-repl []
  (fig/cljs-repl "tick"))

(comment 
  
  (figwheel-start!)
  
  
  )